library(sp, raster)
library(rgdal)
library(plyr, reshape2)
library(bnlearn)
setwd('/usr/local/dev/rdetroit/')
options(stringsAsFactors=FALSE)

FILE.LOC <- '/home/arthur/Workspace/TermProject/'
CENSUS.VARS <- c('med.hhold.income', 'male.pop')
VARS <- c(CENSUS.VARS, 'rec.area.proximity')
ALL.VARS <- c(VARS, 'old')

#####################################
# Data preparation and factorization

require(raster)
load(file='rda/spatialMeasures.rda') # Load census measures and the 30-m land cover data
load(file='rda/graphs.rda') # Load trained Bayesian network(s)

# TODO An improvement would be to have the levels of the layers be in [0, 1, 2, ...]
factors <- matrix(nrow=length(VARS), ncol=length(VARS))
for (i in seq.int(1, length(VARS))) {
  factors[i,] <- c(VARS[i], rownames(get(VARS[i], fit.expert)$prob))
}
factors <- data.frame(factors[,2:length(VARS)], row.names=factors[,1])

##############
# Aggregation

dev2001 <- aggregate(dev2001, fact=10, fun=modal)
dev2006 <- aggregate(dev2006, fact=10, fun=modal)
dev2011 <- aggregate(dev2011, fact=10, fun=modal)

save(dev2001, dev2006, dev2011, file='rda/caAggregates.rda')
# load(file='rda/caAggregates.rda') # Replace 30-m land cover data with aggregated data

# Recreation and outdoor areas
rec.area.dist <- raster::raster(paste0(FILE.LOC,
                                       'ancillary/rec+outdoor_nad83_prox_cut.tiff'))
rec.area.dist <- resample(rec.area.dist, dev2001)

##############
# Rasterizing

layers <- as.list(1:length(CENSUS.VARS))
names(layers) <- CENSUS.VARS
layers$rec.area.proximity <- rec.area.dist

# This is key! This is where the census attributes are set
for (var in CENSUS.VARS) {
  layers[var] <- rasterize(attr2011, dev2011, var) # 2011 census
}

###########################
# Masking and discretizing

# This is key! This is the "old" land cover
layers$old <- raster::mask(dev2006, layers$male.pop, maskvalue=NA) # 2006 land cover
layers$rec.area.proximity <- raster::mask(layers$rec.area.proximity,
                                          get(CENSUS.VARS[1], layers), maskvalue=NA)

save(layers, file='rda/predictionLayers2011.rda')
# load(file='rda/predictionLayers2011.rda')

###############
# Discretizing

# Create a reclass matrix from the levels
for (var in VARS) {
  # Split apart e.g. "(20.1,190]"
  reclass.matrix <- sapply(factors[var,],
                           function (s) as.numeric(unlist(strsplit(gsub('\\[|\\]|\\(|\\)',
                                                                        '', s), ','))))
  reclass.matrix[1] <- 0
  layers[var] <- raster::reclassify(get(var, layers),
                                    cbind(reclass.matrix, c(0, 1)),
                                    right=TRUE) # Intervals closed on right
}

###########
# Stacking

layers <- stack(layers)
save(layers, factors, file='rda/layerStack2011.rda')
# load(file='rda/layerStack2011.rda')

# Clean-up
remove(i, var, vars, rec.area.dist, roads.dist, reclass.matrix)

####################
# Utility functions

# A function to update the posterior probability distribution with evidence
updateNetwork <- function (jtree, states) {
  states <- na.omit(states)

  # Do not do anything if the input data are all NA
  if (dim(states)[1] == 0) {
    return(jtree)
  }

  # Translate the raster classes [0, 1, 2, ...] into factors
  evidence <- data.frame(matrix(nrow=dim(states)[1], ncol=length(VARS)))
  names(evidence) <- VARS
  for (var in VARS) {
    evidence[,var] <- t(factors[var,][states[,var] + 1])
  }

  evidence$old <- as.character(states[,'old'])

  # TODO This might be parallelizable if I don't modify the jtree; rather, return the posterior as a copy
  for (i in seq(1, dim(evidence)[1])) {
    jtree <- setEvidence(jtree, nodes=names(evidence), nslist=mapply(list, evidence[i,]))
  }

  jtree
}

# A function to choose outcomes, one at a time, with the same probability as the given posterior distribution
chooseOutcome <- function (posterior) {
  posterior <- sort(posterior)

  # Sort the posterior probabilties by factors, e.g. "1=0.56,0=0.44" becomes "0=0.44,1=0.56"
  post <- numeric()
  for (i in seq.int(1, length(posterior))) {
    post[i] <- posterior[as.character(i - 1)]
  }

  # Generate a vector of probability thresholds e.g. [0.0, 0.44] for transitions to [0, 1]; upper bound of p=1.0 is implied.
  prob <- rep(0, length(post))
  for (i in seq.int(length(post) - 1, 1, by=-1)) {
    j <- length(post) - i
    prob <- prob + c(rep(0, j), post[(j-1):(length(post)-j)])
  }

  # Generate a random uniform deviate on [0, 1] to determine which factor to output
  r <- runif(1)
  for (i in seq.int(0, length(prob) - 2)) {
    if (r < prob[i + 2]) {
      return(i) # p < threshold in e.g. [0, 0.44]? Output that factor
    }
  }

  (length(prob) - 1) # p < implied upper bound of 1.0? Output last factor
}

############################
# Simulation and prediction

load(file='rda/graphs.rda')

require(gRain)

# We use the junction tree algorithm to create an independence network that we can query
prior.expert <- compile(as.grain(fit.expert))
prior.mmhc <- compile(as.grain(fit.mmhc))

output.expert.2011 <- stackApply(layers, rep(1, length(names(layers))), function (r, ...) {
  chooseOutcome(querygrain(updateNetwork(prior.expert, as.data.frame(t(r))),
                           nodes='new')$new)
})

output.mmhc.2011 <- stackApply(layers, rep(1, length(names(layers))), function (r, ...) {
  chooseOutcome(querygrain(updateNetwork(prior.mmhc, as.data.frame(t(r))),
                           nodes='new')$new)
})

save(output.mmhc.2011, output.expert.2011, file='rda/outputsUnmasked2011.rda')
# load(file='rda/outputsUnmasked2011.rda')

output.mmhc.2011 <- mask(output.mmhc.2011, layers$male.pop, maskvalue=NA)
output.expert.2011 <- mask(output.expert.2011, layers$male.pop, maskvalue=NA)

save(output.mmhc.2011, output.expert.2011, file='rda/outputs2011.rda')
# load(file='rda/outputs2011.rda')

#########################################
# Transition probabilities: Expert graph

require(gRain)
prior <- prior.expert

# Figure out the order of the factors returned from the prior distribution (it isn't guaranteed to be in a predictable order); this is the order of the layers in the transition probabilties RasterStack
labels <- c('0'='prob.undeveloped', '1'='prob.low.dev', '2'='prob.high.dev')
labels. <- c()
for (l in names(querygrain(prior, nodes='new')$new)) {
  labels. <- c(labels., labels[as.numeric(l) + 1])
}

# Find transition probabilities for the expert graph
trans.probs.expert <- raster::calc(layers, function (states) {
  trans <- matrix(nrow=dim(states)[1], ncol=3)
  for (i in seq(1, dim(states)[1])) {
    trans[i,] <- querygrain(updateNetwork(prior, as.data.frame(t(states[i,]))),
                            nodes='new')$new
  }
  return(trans)
}, forcefun=TRUE)
names(trans.probs.expert) <- labels.

# Masking
trans.probs.expert$prob.undeveloped <- mask(trans.probs.expert$prob.undeveloped,
                                            layers$male.pop, maskvalue=NA)
trans.probs.expert$prob.low.dev <- mask(trans.probs.expert$prob.low.dev,
                                        layers$male.pop, maskvalue=NA)
trans.probs.expert$prob.high.dev <- mask(trans.probs.expert$prob.high.dev,
                                         layers$male.pop, maskvalue=NA)

##########################################
# Transition probabilities: Learned graph

# Find transition probabilities for the learned graph
trans.probs.mmhc <- raster::calc(layers, function (states) {
  trans <- matrix(nrow=dim(states)[1], ncol=3)
  for (i in seq(1, dim(states)[1])) {
    trans[i,] <- querygrain(updateNetwork(prior.mmhc, as.data.frame(t(states[i,]))),
                            nodes='new')$new
  }
  return(trans)
}, forcefun=TRUE)
names(trans.probs.mmhc) <- labels.

# Masking
trans.probs.mmhc$prob.undeveloped <- mask(trans.probs.mmhc$prob.undeveloped,
                                          layers$male.pop, maskvalue=NA)
trans.probs.mmhc$prob.low.dev <- mask(trans.probs.mmhc$prob.low.dev,
                                      layers$male.pop, maskvalue=NA)
trans.probs.mmhc$prob.high.dev <- mask(trans.probs.mmhc$prob.high.dev,
                                       layers$male.pop, maskvalue=NA)


save(trans.probs.mmhc, trans.probs.expert, file='rda/transitionProbs2011.rda')
# load(file='rda/transitionProbs2011.rda')

#############
# Validation

load(file='rda/caAggregates.rda') # Replace dev* with aggregated
load(file='rda/outputs2006.rda')
load(file='rda/outputs2011.rda')

# Mask by our prediction for a fair comparison
dev2001 <- mask(dev2001, output.expert.2006, maskvalue=NA)
dev2006 <- mask(dev2006, output.expert.2006, maskvalue=NA)
dev2011 <- mask(dev2011, output.expert.2011, maskvalue=NA)

# Create difference samples
diff0601 <- dev2006 - dev2001
diff1106 <- dev2011 - dev2006
reclass.matrix <- matrix(c(c(0,0,0), c(1,3,1), c(-3,-1,1)), byrow=TRUE, ncol=3)

require(raster)
diff0601 <- raster::reclassify(diff0601, reclass.matrix, right=TRUE)
diff1106 <- raster::reclassify(diff1106, reclass.matrix, right=TRUE)

diffpts0601 <- na.omit(as.data.frame(diff0601, xy=TRUE))
diffpts0601 <- subset(diffpts0601, diffpts0601$layer==1)
diffpts0601$layer <- NULL
diffpts1106 <- na.omit(as.data.frame(diff1106, xy=TRUE))
diffpts1106 <- subset(diffpts1106, diffpts1106$layer==1)
diffpts1106$layer <- NULL

# cols <- c('#FFFFFF', '#AACCEE', '#113355')
# plot(output.mmhc.2006, axes=FALSE, box=FALSE, col=cols, legend=FALSE)
pts <- as.data.frame(output.expert.2006, xy=TRUE)
pts$layer <- NULL
# points(pts, col='red')

# stats <- data.frame(matrix(nrow=6, ncol=7),
#                     row.names=c('2006.observed', '2006.expert', '2006.learned',
#                                 '2011.observed', '2011.expert', '2011.learned'))
stats <- data.frame(matrix(nrow=4, ncol=4),
                    row.names=c('2006.expert', '2006.learned',
                                '2011.expert', '2011.learned'))
names(stats) <- c('Cohens.Kappa', 'Cohens.K.Changed', 'Spearmans.Rho.Changed', 'Spearmans.Rho')

require(raster)
samples.observed.2006 <- extract(dev2006, pts, df=TRUE, factors=TRUE)
samples.mmhc.2006 <- extract(output.mmhc.2006, pts, df=TRUE, factors=TRUE)
samples.expert.2006 <- extract(output.expert.2006, pts, df=TRUE, factors=TRUE)
samples.observed.2011 <- extract(dev2011, pts, df=TRUE, factors=TRUE)
samples.mmhc.2011 <- extract(output.mmhc.2011, pts, df=TRUE, factors=TRUE)
samples.expert.2011 <- extract(output.expert.2011, pts, df=TRUE, factors=TRUE)

dsamples.observed.2006 <- extract(dev2006, diffpts0601, df=TRUE, factors=TRUE)
dsamples.mmhc.2006 <- extract(output.mmhc.2006, diffpts0601, df=TRUE, factors=TRUE)
dsamples.expert.2006 <- extract(output.expert.2006, diffpts0601, df=TRUE, factors=TRUE)
dsamples.observed.2011 <- extract(dev2011, diffpts1106, df=TRUE, factors=TRUE)
dsamples.mmhc.2011 <- extract(output.mmhc.2011, diffpts1106, df=TRUE, factors=TRUE)
dsamples.expert.2011 <- extract(output.expert.2011, diffpts1106, df=TRUE, factors=TRUE)

# Cohen's kappa
require(lpSolve)
require(irr)
stats['2006.expert',
      'Cohens.Kappa'] <- kappa2(data.frame(actual=samples.observed.2006$layer,
                                guess=samples.expert.2006$layer))$value
stats['2006.learned',
      'Cohens.Kappa'] <- kappa2(data.frame(actual=samples.observed.2006$layer,
                                guess=samples.mmhc.2006$layer))$value
stats['2011.expert',
      'Cohens.Kappa'] <- kappa2(data.frame(actual=samples.observed.2011$layer,
                                guess=samples.expert.2011$layer))$value
stats['2011.learned',
      'Cohens.Kappa'] <- kappa2(data.frame(actual=samples.observed.2011$layer,
                                guess=samples.mmhc.2011$layer))$value

stats['2006.expert',
      'Cohens.K.Changed'] <- kappa2(data.frame(actual=dsamples.observed.2006$layer,
                                    guess=dsamples.expert.2006$layer))$value
stats['2006.learned',
      'Cohens.K.Changed'] <- kappa2(data.frame(actual=dsamples.observed.2006$layer,
                                    guess=dsamples.mmhc.2006$layer))$value
stats['2011.expert',
      'Cohens.K.Changed'] <- kappa2(data.frame(actual=dsamples.observed.2011$layer,
                                    guess=dsamples.expert.2011$layer))$value
stats['2011.learned',
      'Cohens.K.Changed'] <- kappa2(data.frame(actual=dsamples.observed.2011$layer,
                                    guess=dsamples.mmhc.2011$layer))$value

# Spearman's rho
stats['2006.expert',
      'Spearmans.Rho'] <- cor.test(as.numeric(samples.observed.2006$layer),
                                   as.numeric(samples.expert.2006$layer),
                                   method='spearman')$estimate
stats['2006.learned',
      'Spearmans.Rho'] <- cor.test(as.numeric(samples.observed.2006$layer),
                                   as.numeric(samples.mmhc.2006$layer),
                                   method='spearman')$estimate
stats['2011.expert',
      'Spearmans.Rho'] <- cor.test(as.numeric(samples.observed.2011$layer),
                                   as.numeric(samples.expert.2011$layer),
                                   method='spearman')$estimate
stats['2011.learned',
      'Spearmans.Rho'] <- cor.test(as.numeric(samples.observed.2011$layer),
                                   as.numeric(samples.mmhc.2011$layer),
                                   method='spearman')$estimate

stats['2006.expert',
      'Spearmans.Rho.Changed'] <- cor.test(as.numeric(dsamples.observed.2006$layer),
                                   as.numeric(dsamples.expert.2006$layer),
                                   method='spearman')$estimate
stats['2006.learned',
      'Spearmans.Rho.Changed'] <- cor.test(as.numeric(dsamples.observed.2006$layer),
                                   as.numeric(dsamples.mmhc.2006$layer),
                                   method='spearman')$estimate
stats['2011.expert',
      'Spearmans.Rho.Changed'] <- cor.test(as.numeric(dsamples.observed.2011$layer),
                                   as.numeric(dsamples.expert.2011$layer),
                                   method='spearman')$estimate
stats['2011.learned',
      'Spearmans.Rho.Changed'] <- cor.test(as.numeric(dsamples.observed.2011$layer),
                                   as.numeric(dsamples.mmhc.2011$layer),
                                   method='spearman')$estimate

# Class frequencies
# stats['2006.observed', 'Undev.freq'] <- count(samples.observed.2006, 'layer')[1,]$freq
# stats['2006.observed', 'Low.dev.freq'] <- count(samples.observed.2006, 'layer')[2,]$freq
# stats['2006.observed', 'High.dev.freq'] <- count(samples.observed.2006, 'layer')[3,]$freq
# stats['2006.learned', 'Undev.freq'] <- count(samples.mmhc.2006, 'layer')[1,]$freq
# stats['2006.learned', 'Low.dev.freq'] <- count(samples.mmhc.2006, 'layer')[2,]$freq
# stats['2006.learned', 'High.dev.freq'] <- count(samples.mmhc.2006, 'layer')[3,]$freq
# stats['2006.expert', 'Undev.freq'] <- count(samples.expert.2006, 'layer')[1,]$freq
# stats['2006.expert', 'Low.dev.freq'] <- count(samples.expert.2006, 'layer')[2,]$freq
# stats['2006.expert', 'High.dev.freq'] <- count(samples.expert.2006, 'layer')[3,]$freq
#
# stats['2011.observed', 'Undev.freq'] <- count(samples.observed.2011, 'layer')[1,]$freq
# stats['2011.observed', 'Low.dev.freq'] <- count(samples.observed.2011, 'layer')[2,]$freq
# stats['2011.observed', 'High.dev.freq'] <- count(samples.observed.2011, 'layer')[3,]$freq
# stats['2011.learned', 'Undev.freq'] <- count(samples.mmhc.2011, 'layer')[1,]$freq
# stats['2011.learned', 'Low.dev.freq'] <- count(samples.mmhc.2011, 'layer')[2,]$freq
# stats['2011.learned', 'High.dev.freq'] <- count(samples.mmhc.2011, 'layer')[3,]$freq
# stats['2011.expert', 'Undev.freq'] <- count(samples.expert.2011, 'layer')[1,]$freq
# stats['2011.expert', 'Low.dev.freq'] <- count(samples.expert.2011, 'layer')[2,]$freq
# stats['2011.expert', 'High.dev.freq'] <- count(samples.expert.2011, 'layer')[3,]$freq

save(stats, file='rda/validationStats.rda')
write.csv(stats, file='~/Workspace/TermProject/outputs/validation.csv')

###################
# Zonal statistics

tracts <- readOGR('/usr/local/dev/rdetroit/shp/t10_nad83.shp', 't10_nad83')
tracts$FIPS <- tracts$GEOID10
tracts <- subset(tracts, select=c('FIPS'))
obs.2006.prop.undev <- extract(dev2006, tracts, method='simple', df=TRUE, sp=TRUE,
                               fun=function (v) length(v[v==0]) / length(v))
obs.2006.prop.lowdev <- extract(dev2006, tracts, method='simple', df=TRUE, sp=TRUE,
                                fun=function (v) length(v[v==1]) / length(v))
obs.2006.prop.highdev <- extract(dev2006, tracts, method='simple', df=TRUE, sp=TRUE,
                                 fun=function (v) length(v[v==2]) / length(v))

obs.2011.prop.undev <- extract(dev2011, tracts, method='simple', df=TRUE, sp=TRUE,
                               fun=function (v) length(v[v==0]) / length(v))
obs.2011.prop.lowdev <- extract(dev2011, tracts, method='simple', df=TRUE, sp=TRUE,
                                fun=function (v) length(v[v==1]) / length(v))
obs.2011.prop.highdev <- extract(dev2011, tracts, method='simple', df=TRUE, sp=TRUE,
                                 fun=function (v) length(v[v==2]) / length(v))

exp.2006.prop.undev <- extract(output.expert.2006, tracts, method='simple', df=TRUE, sp=TRUE,
                               fun=function (v) length(v[v==0]) / length(v))
exp.2006.prop.lowdev <- extract(output.expert.2006, tracts, method='simple', df=TRUE, sp=TRUE,
                                fun=function (v) length(v[v==1]) / length(v))
exp.2006.prop.highdev <- extract(output.expert.2006, tracts, method='simple', df=TRUE, sp=TRUE,
                                 fun=function (v) length(v[v==2]) / length(v))

exp.2011.prop.undev <- extract(output.expert.2011, tracts, method='simple', df=TRUE, sp=TRUE,
                               fun=function (v) length(v[v==0]) / length(v))
exp.2011.prop.lowdev <- extract(output.expert.2011, tracts, method='simple', df=TRUE, sp=TRUE,
                                fun=function (v) length(v[v==1]) / length(v))
exp.2011.prop.highdev <- extract(output.expert.2011, tracts, method='simple', df=TRUE, sp=TRUE,
                                 fun=function (v) length(v[v==2]) / length(v))

zonal.stats.obs <- data.frame(FIPS=obs.2006.prop.undev$FIPS,
                          '2006 Observed: Undeveloped'=obs.2006.prop.undev$layer,
                          '2006 Observed: Low-Intensity'=obs.2006.prop.lowdev$layer,
                          '2006 Observed: High-Intensity'=obs.2006.prop.highdev$layer,
                          '2011 Observed: Undeveloped'=obs.2011.prop.undev$layer,
                          '2011 Observed: Low-Intensity'=obs.2011.prop.lowdev$layer,
                          '2011 Observed: High-Intensity'=obs.2011.prop.highdev$layer,
                          check.names=FALSE)
zonal.stats.exp <- data.frame(FIPS=exp.2006.prop.undev$FIPS,
                          '2006 Expert: Undeveloped'=exp.2006.prop.undev$layer,
                          '2006 Expert: Low-Intensity'=exp.2006.prop.lowdev$layer,
                          '2006 Expert: High-Intensity'=exp.2006.prop.highdev$layer,
                          '2011 Expert: Undeveloped'=exp.2011.prop.undev$layer,
                          '2011 Expert: Low-Intensity'=exp.2011.prop.lowdev$layer,
                          '2011 Expert: High-Intensity'=exp.2011.prop.highdev$layer,
                          check.names=FALSE)

save(zonal.stats.obs, zonal.stats.exp, file='rda/zonalStatistics.rda')

require(reshape2)
require(ggplot2)
ggplot(melt(zonal.stats.obs, id.vars='FIPS'), mapping=aes(group=variable, x=value)) +
  geom_histogram() +
  facet_wrap(~ variable) +
  xlab('') +
  ylab('Census Tract Count') +
  theme_bw() +
  theme(text=element_text(size=16))

ggplot(melt(zonal.stats.exp, id.vars='FIPS'), mapping=aes(group=variable, x=value)) +
  geom_histogram() +
  facet_wrap(~ variable) +
  xlab('') +
  ylab('Census Tract Count') +
  theme_bw() +
  theme(text=element_text(size=16))

