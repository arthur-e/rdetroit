library(sp, raster)
library(rgdal)
library(plyr, reshape2)
library(bnlearn)

setwd('/usr/local/dev/rdetroit/')
options(stringsAsFactors=FALSE)

census.vars <- c('SE_T003_001', 'SE_T093_001', 'SE_T155_001', 'SE_T156_001', 'SE_T156_002', 'SE_T004_002', 'SE_T005_002', 'SE_T005_003', 'SE_T014_002', 'SE_T014_003', 'SE_T014_005', 'SE_T020_001', 'SE_T020_002', 'SE_T020_003', 'SE_T020_005', 'SE_T020_006', 'SE_T020_008', 'SE_T020_009', 'SE_T025_001', 'SE_T185_004')
acs.vars <- c('SE_T002_001', 'SE_T001_001', 'SE_T004_002', 'SE_T004_003', 'SE_T002_003', 'SE_T057_001', 'SE_T093_001', 'SE_T094_001', 'SE_T094_002', 'SE_T017_001', 'SE_T017_002', 'SE_T017_003', 'SE_T017_005', 'SE_T017_006', 'SE_T017_008', 'SE_T017_009', 'SE_T013_002', 'SE_T013_003', 'SE_T013_005', 'SE_T118_004')

# =======================================
# Reading in census and crosswalk tables
census2000 <- read.csv('census_data/census2000.csv', header=T, skip=1,
                       colClasses=c('Geo_FIPS'='character')) # 2000 Census data
acs2006.2010 <- read.csv('census_data/acs2006-2010.csv', header=T, skip=1,
                         colClasses=c('Geo_FIPS'='character')) # 2006-2010 ACS data
xwalk <- plyr::arrange(read.csv('census_data/crosswalk_2000_2010.csv',
                                colClasses=c('character', 'character')),
                 trtid10) # 2000 to 2010 Crosswalk data
xwalk <- within(xwalk, weight <- as.numeric(weight))

require(plyr)
census2000 <- subset(mutate(census2000, trtid00=Geo_FIPS), select=c('trtid00', census.vars))
acs2006.2010 <- subset(acs2006.2010, select=c('Geo_FIPS', acs.vars))

# Subset the crosswalk table to just those tracts in our census data
xwalk <- subset(xwalk, trtid00 %in% intersect(census2000$trtid00, xwalk$trtid00),
                select=c(trtid00, trtid10, weight))

# We interpolate the 2000 census tracts to 2010 census tracts by a weighted linear combination
require(reshape2)
temp <- melt(join(xwalk, census2000, by=c('trtid00')),
             id.vars=c('trtid00', 'trtid10', 'weight'))
temp <- within(temp, value <- value * weight) # Scale the census measures by Brown et al.'s weights

# Recast 2010 census tracts as a sum of the 2000 census measures
census2000.as.2010 <- within(dcast(temp, trtid10 ~ variable, fun.aggregate=sum, value.var='value'), Geo_FIPS <- trtid10)

# For some reason, a couple of tracts in the ACS are not in the 2010 interpolation of the 2000 census
disjoint.tracts <- union(setdiff(census2000.as.2010$Geo_FIPS, acs2006.2010$Geo_FIPS),
                         setdiff(acs2006.2010$Geo_FIPS, census2000.as.2010$Geo_FIPS))

acs2006.2010 <- subset(acs2006.2010, !Geo_FIPS %in% disjoint.tracts)
census2000.as.2010 <- subset(census2000.as.2010, !Geo_FIPS %in% disjoint.tracts)

require(ggplot2)
ggplot(data=melt(census2000.as.2010, id.vars=c('trtid10', 'Geo_FIPS')),
                 mapping=aes(x=value, group=variable)) +
  geom_histogram() +
  facet_wrap(~ variable, scales='free')

# ========================
# Normalizing census data
survey2000 <- with(census2000.as.2010, data.frame(
  FIPS=Geo_FIPS,
  pop.density=SE_T003_001,
  med.hhold.income=SE_T093_001,
  occupied.housing=SE_T156_001 / SE_T155_001, # Occupied housing units normalized by housing unit count
  owner.occupied=SE_T156_002 / SE_T155_001, # Owner-occupied units normalized by housing unit count
  hholds=SE_T020_001 / SE_T004_002, # Households...by land area
  fam.hholds=SE_T020_002 / SE_T004_002, # Family households...by land area
  married.hholds=SE_T020_003 / SE_T004_002, # Married-couple households...by land area
  lone.male.hholds=SE_T020_005 / SE_T004_002, # Family -holds with lone male householder...by land area
  lone.female.hholds=SE_T020_006 / SE_T004_002, # Family -holds with lone female householder...land area
  male.nonfam.hholds=SE_T020_008 / SE_T004_002, # Non-family -holds with male householder...by land area
  female.nonfam.hholds=SE_T020_009 / SE_T004_002, # Non-family -holds with female householder...land area
  male.pop=SE_T005_002 / SE_T025_001, # Male population normalized by total population
  female.pop=SE_T005_003 / SE_T025_001, # Female population normalized by total pop.
  white.pop=SE_T014_002 / SE_T025_001, # White population...total pop.
  black.pop=SE_T014_003 / SE_T025_001, # Black population...total pop.
  asian.pop=SE_T014_005 / SE_T025_001, # Asian population...total pop.
  poor.pop=SE_T185_004 / SE_T025_001)) # Population poor or struggling...total pop.
survey2006 <- with(acs2006.2010, data.frame(
  FIPS=Geo_FIPS,
  pop.density=SE_T002_001,
  med.hhold.income=SE_T057_001,
  occupied.housing=SE_T094_001 / SE_T093_001, # Occupied housing units normalized by housing unit count
  owner.occupied=SE_T094_002 / SE_T093_001, # Owner-occupied units normalized by housing unit count
  hholds=SE_T017_001 / SE_T002_003, # Households...by land area
  fam.hholds=SE_T017_002 / SE_T002_003, # Family households...by land area
  married.hholds=SE_T017_003 / SE_T002_003, # Married-couple households...by land area
  lone.male.hholds=SE_T017_005 / SE_T002_003, # Family -holds with lone male householder...by land area
  lone.female.hholds=SE_T017_006 / SE_T002_003, # Family -holds with lone female householder...land area
  male.nonfam.hholds=SE_T017_008 / SE_T002_003, # Non-family -holds with male householder...by land area
  female.nonfam.hholds=SE_T017_009 / SE_T002_003, # Non-family -holds with female householder...land area
  male.pop=SE_T004_002 / SE_T001_001, # Male population normalized by total population
  female.pop=SE_T004_003 / SE_T001_001, # Female population normalized by total pop.
  white.pop=SE_T013_002 / SE_T001_001, # White population...total pop.
  black.pop=SE_T013_003 / SE_T001_001, # Black population...total pop.
  asian.pop=SE_T013_005 / SE_T001_001, # Asian population...total pop.
  poor.pop=SE_T118_004 / SE_T001_001)) # Population poor or struggling...total pop.

# Clean-up
remove(acs2006.2010, census2000, census2000.as.2010, temp)

# =================================================
# Join census tract shapefiles and census measures
require(rgdal)
tracts <- readOGR('/usr/local/dev/rdetroit/shp/t10_nad83.shp', 't10_nad83')
tracts$FIPS <- tracts$GEOID10
tracts <- subset(tracts, select=c('FIPS'))

require(plyr)
attr2000 <- merge(tracts, survey2000, by='FIPS')
attr2006 <- merge(tracts, survey2006, by='FIPS')

# ===========================================
# Get and reclassify sample land cover layer
file.loc <- '/home/arthur/Workspace/TermProject/'

require(raster)
rast2000 <- raster::raster(paste0(file.loc, 'nlcd2001_nad83.tif'))
rast2006 <- raster::raster(paste0(file.loc, 'nlcd2006_nad83.tif'))
reclass.matrix <- matrix(c(c(0,10,0), c(10,11,NA), c(12,20,0),
                           c(20,23,1), c(23,24,2), c(24,99,0)),
                         byrow=TRUE, ncol=3)
dev2000 <- raster::reclassify(rast2000, reclass.matrix, right=TRUE) # Intervals closed on right
dev2006 <- raster::reclassify(rast2006, reclass.matrix, right=TRUE)

# Recreation and outdoor areas
rec.area.proximity <- raster::raster(paste0(file.loc,
                                            'ancillary/rec+outdoor_nad83_prox_cut.tiff'))

# Distance to primary roads
road.proximity <- raster::raster(paste0(file.loc, 'ancillary/roads_proximity_cut.tiff'))

# =========================================
# Spatially Join Land Cover and Other Data

# Raster and vector layers have just SLIGHTLY different projection definitions...
# require(rgdal)
attr2000 <- sp::spTransform(attr2000, raster::crs(dev2000))
attr2006 <- sp::spTransform(attr2006, raster::crs(dev2000))

devp2000 <- SpatialPoints(as.data.frame(dev2000, xy=TRUE)[,1:2],
                          proj4string=crs(attr2000))
devp2006 <- SpatialPoints(as.data.frame(dev2006, xy=TRUE)[,1:2],
                          proj4string=crs(attr2006))

# Extract attributes by their location... This takes a long time (about 4 minutes for each)
attr2000 <- sp::over(devp2000, attr2000)
attr2006 <- sp::over(devp2006, attr2006)

# Sample from the distance to roads layer
road.proximities <- data.frame(road.proximity=extract(road.proximity, devp2000))
rec.proximities <- data.frame(rec.area.proximity=extract(rec.area.proximity, devp2000))

save(road.proximities, rec.proximities, attr2000, attr2006, devp2000, devp2006,
     file='rda/allData.rda')
load(file='rda/allData.rda')

# Assume that the rows are in order; we align the land cover pixels with the attributes we just sampled (Naive, but R leaves us with no choice)
require(plyr)
training <- na.omit(cbind(data.frame(old=as.data.frame(dev2000, xy=TRUE)$layer,
                                     new=as.data.frame(dev2006, xy=TRUE)$layer),
                          attr2006, rec.proximities, road.proximities))

# Correct error in type coercion; should be numeric not integer
training$pop.density <- as.numeric(training$pop.density)
training$med.hhold.income <- as.numeric(training$med.hhold.income)
# training <- apply(training, 2, as.numeric)

# How many pixels changed from 2001 to 2006?
dim(training[training$old != training$new,])[1]

# Clean-up
remove(rrast2000, rast2006, tracts, attr2000, attr2006, dev2000, dev2006,
       devp2000, devp2006, rec.area.proximity, rec.proximities,
       road.proximity, road.proximities)

save(training, file='rda/training.rda')
load(file='rda/training.rda')

#=======================
# Discretizing the Data

cases <- data.frame(t(combn(setdiff(colnames(training), c('new', 'FIPS')), 2)))

require(plyr)
corrs <- ddply(cases, ~ X1 + X2, mutate,
      r.sq=cor.test(training[,X1], training[,X2], method='pearson')$estimate,
      p.value=cor.test(training[,X1], training[,X2], method='pearson')$p.value)

save(corrs, file='rda/correlationTests.rda')
load(file='rda/correlationTests.rda')

subset(corrs, abs(r.sq) > 0.5)

# 2014-11-22
# We have to include the following variables: old, new.
# Valid combinations of variables seem to be: [fam.hholds | married.hholds] & [med.hhold.income | owner.occupied | poor.pop | occupied.housing] & [pop.density] & [rec.area.proximity] & [male.pop | female.pop] & [road.proximity | old]
# road.proximity is correlated with old at an r^2 of -0.51

subset(corrs, X1=='old' | X2=='old')
subset(corrs, X1=='rec.area.proximity' | X2=='rec.area.proximity')
subset(corrs, X1=='road.proximity' | X2=='road.proximity')

vars <- c('old', 'new', 'road.proximity', 'rec.area.proximity', 'married.hholds', 'med.hhold.income', 'pop.density', 'male.pop')

training.data <- subset(training, select=vars)

# If necessary (because R's save() changed the column data types)
training.data <- transform(training.data,
                           med.hhold.income=as.numeric(med.hhold.income),
                           pop.density=as.numeric(pop.density))

# So, we discard road.proximity
vars <- c('new', names(dedup(training.data, 0.5)))
training.data <- subset(training.data, select=vars)

training.discrete <- cbind(apply(training.data[,1:2], 2, as.factor),
                           bnlearn::discretize(training.data[,3:length(vars)],
                                               breaks=rep(2, length(vars) - 2),
                                               method='quantile'))

remove(cases, training)

# ===================================
# Training the Bayesian Network (BN)

# Creating a random sample...
training.sample <- training.discrete[sample(nrow(training.discrete),
                                            dim(training.discrete)[1]*0.05),]

plot(bnlearn::iamb(training.sample));title('IAMB')
plot(bnlearn::hc(training.sample));title('Hill-Climbing')
plot(bnlearn::tabu(training.sample));title('Tabu Scoring')
plot(bnlearn::mmhc(training.sample));title('Max-Min Hill Climbing')
plot(bnlearn::rsmax2(training.sample));title('RSMAX2')

# 2014-11-23
# All of the network learning approaches produce a complete or near-complete graph when trained on the full dataset, subset by independent factors, or a random sample thereof. Worse still, no nodes have arcs directed towards new land cover. The complete graph can also be demonstrated to be inferior as it has a higher BIC relative to the more sparse, expert graph.

dim(training.discrete[training.discrete$old != training.discrete$new,])[1]
training.sample <- subset(training.discrete, !old == new)
plot(bnlearn::hc(training.sample, restart=1));title('Hill-Climbing')
plot(bnlearn::hc(training.sample,
                 restart=5));title('Hill-Climbing; Restarts=5')
plot(bnlearn::hc(training.sample,
                 restart=10));title('Hill-Climbing; Restarts=10')
plot(bnlearn::hc(training.sample,
                 restart=20));title('Hill-Climbing; Restarts=20')
plot(bnlearn::hc(training.sample,
                 restart=10, perturb=5));title('Hill-Climbing; Restarts=10, Perturbations=5')
plot(bnlearn::hc(training.sample,
                 restart=20, perturb=10));title('Hill-Climbing; Restarts=20, Perturbations=10')
plot(bnlearn::rsmax2(training.sample));title('Learned by RSMAX2')
plot(bnlearn::mmhc(training.sample));title('Learned by Max-Min Hill Climbing')

mmhc.dag <- bnlearn::mmhc(training.sample)

# We'll reverse the direction of just the pop.density <- new arc
mmhc.dag <- set.arc(mmhc.dag, 'pop.density', 'new')
plot(mmhc.dag); title('Learned by Max-Min Hill Climbing; Revised by Expert')

# 2014-11-23
# When the data are trained on just that subset of pixels which have changed between 2001 and 2006, the graphs that are learned are still dense but the two hybrid algorithms produce a less dense graph that they agree on completely.

# Old land cover less developed than new? (Increasing development intensity)
training.sample <- subset(training.discrete, ((old==0 | old==1) & new==2) |
                            (old==0 & (new==1 | new==2)))
plot(bnlearn::mmhc(training.sample));title('Old < New; Learned by Max-Min Hill Climbing')
plot(bnlearn::rsmax2(training.sample));title('Old < New; Learned by RSMAX2')

# New land cover less developed than old? (Decreasing development intensity)
# There are only 16 such pixels...
dim(subset(training.discrete, ((old==1 | old==2) & new==0) | (new==1 & old==2))[1])

# Now let's think about specifying an "expert" network structure...
spec <- matrix(c('old', 'new', 'old', 'pop.density', 'old', 'rec.area.proximity', 'male.pop', 'married.hholds', 'pop.density', 'new', 'med.hhold.income', 'pop.density', 'med.hhold.income', 'new', 'married.hholds', 'med.hhold.income', 'married.hholds', 'new', 'rec.area.proximity', 'new', 'rec.area.proximity', 'pop.density', 'rec.area.proximity', 'med.hhold.income'),
               ncol=2, byrow=TRUE)

expert.dag <- empty.graph(names(training.discrete))
arcs(expert.dag) <- spec
plot(expert.dag); title('Specified Network')

#====================
# Parameter Learning

# How do the two learned model structures compare?
score(mmhc.dag, data=training.discrete)
score(expert.dag, data=training.discrete)

score(mmhc.dag, data=training.sample)
score(expert.dag, data=training.sample)

fit.mmhc <- bn.fit(mmhc.dag, data=training.discrete, method='bayes')
fit.expert <- bn.fit(expert.dag, data=training.discrete, method='bayes')

save(mmhc.dag, expert.dag, fit.mmhc, fit.expert, training.discrete, file='rda/graphs.rda')
load(file='rda/graphs.rda')





