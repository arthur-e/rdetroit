library(sp, raster)
library(rgdal)
library(plyr, reshape2)
library(bnlearn)
setwd('/usr/local/dev/rdetroit/')
options(stringsAsFactors=FALSE)

FILE.LOC <- '/home/arthur/Workspace/TermProject/'
CENSUS.VARS <- c('med.hhold.income', 'male.pop')
VARS <- c(CENSUS.VARS, 'rec.area.proximity')
ALL.VARS <- c(VARS, 'old', 'new')

#####################################
# Data preparation and factorization

require(raster)
load(file='rda/spatialMeasures.rda') # Load census measures and the 30-m land cover data
load(file='rda/graphs.rda') # Load trained Bayesian network(s)

factors.expert <- matrix(nrow=length(VARS), ncol=length(VARS))
for (i in seq.int(1, length(VARS))) {
  factors.expert[i,] <- c(VARS[i], rownames(get(VARS[i], fit.expert)$prob))
}
factors.expert <- data.frame(factors.expert[,2:length(VARS)], row.names=factors.expert[,1])

##############
# Aggregation

dev2001 <- aggregate(dev2001, fact=10, fun=modal)
dev2006 <- aggregate(dev2006, fact=10, fun=modal)
dev2011 <- aggregate(dev2011, fact=10, fun=modal)

save(dev2001, dev2006, dev2011, file='rda/caAggregates.rda')
load(file='rda/caAggregates.rda') # Replace 30-m land cover data with aggregated data

# Recreation and outdoor areas
rec.area.dist <- raster::raster(paste0(FILE.LOC,
                                       'ancillary/rec+outdoor_nad83_prox_cut.tiff'))
rec.area.dist <- resample(rec.area.dist, dev2001)

##############
# Rasterizing

vars <- c('med.hhold.income', 'male.pop')
layers <- as.list(1:length(vars))
names(layers) <- vars
layers$rec.area.proximity <- rec.area.dist

# This is key! This is where the census attributes are set
for (var in vars) {
  layers[var] <- rasterize(attr2011, dev2011, var) # 2011 census
}

###########################
# Masking and discretizing

# This is key! This is the "old" land cover
layers$old <- raster::mask(dev2006, layers$male.pop, maskvalue=NA) # 2006 land cover
layers$rec.area.proximity <- raster::mask(layers$rec.area.proximity,
                                          layers$male.pop, maskvalue=NA)

save(layers, file='rda/predictionLayers2011.rda')
load(file='rda/predictionLayers2011.rda')

###############
# Discretizing

# Create a reclass matrix from the levels
vars <- c('rec.area.proximity', 'med.hhold.income', 'male.pop')
for (var in vars) {
  # Split apart e.g. "(20.1,190]"
  reclass.matrix <- sapply(levels(training.sample[,var]),
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

# TODO An improvement would be to have the levels of the layers be in [0, 1, 2, ...]
layer.levels <- list(rec.area.proximity=levels(training.sample$rec.area.proximity),
                     med.hhold.income=levels(training.sample$med.hhold.income),
                     male.pop=levels(training.sample$male.pop))

save(layers, layer.levels, file='rda/layerStack.rda')
load(file='rda/layerStack.rda')

# Clean-up
remove(var, vars, rec.area.dist, roads.dist, reclass.matrix)

####################
# Utility functions

# TODO An improvement would be to have the levels of the layers be in [0, 1, 2, ...]
layer.levels <- list(rec.area.proximity=levels(training.sample$rec.area.proximity),
                     med.hhold.income=levels(training.sample$med.hhold.income),
                     male.pop=levels(training.sample$male.pop))

# A function to update the posterior probability distribution with evidence
updateNetwork <- function (jtree, states) {
  # Do not do anything if the input data are all NA
  if (all(is.na(states))) {
    return(jtree)
  }

  evidence <- transform(states,
                        med.hhold.income=layer.levels$med.hhold.income[med.hhold.income + 1],
                        male.pop=layer.levels$male.pop[male.pop + 1],
                        rec.area.proximity=layer.levels$rec.area.proximity[rec.area.proximity + 1],
                        old=as.character(old))

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
load(file='rda/layerStack.rda')

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

output.mmhc.2011 <- mask(output.mmhc.2011, layers$male.pop, maskvalue=NA)
output.expert.2011 <- mask(output.expert.2011, layers$male.pop, maskvalue=NA)

save(output.mmhc.2006, output.expert.2006, file='rda/outputs2006.rda')
load(file='rda/outputs2006.rda')









