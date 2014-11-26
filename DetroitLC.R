library(sp, raster)
library(rgdal)
library(plyr, reshape2)
library(bnlearn)

setwd('/usr/local/dev/rdetroit/')
options(stringsAsFactors=FALSE)

census.vars <- c('SE_T003_001', 'SE_T093_001', 'SE_T155_001', 'SE_T156_001', 'SE_T156_002', 'SE_T004_002', 'SE_T005_002', 'SE_T005_003', 'SE_T014_002', 'SE_T014_003', 'SE_T014_005', 'SE_T020_001', 'SE_T020_002', 'SE_T020_003', 'SE_T020_005', 'SE_T020_006', 'SE_T020_008', 'SE_T020_009', 'SE_T025_001', 'SE_T185_004')
census.2010.vars <- c('SE_T002_002', 'SE_T068_001', 'SE_T069_001', 'SE_T069_002', 'SE_T002_006', 'SE_T058_001', 'SE_T058_002', 'SE_T058_003', 'SE_T058_005', 'SE_T058_006', 'SE_T063_003', 'SE_T063_004', 'SE_T063_006', 'SE_T063_007', 'SE_T063_009', 'SE_T063_011', 'SE_T063_012', 'SE_T063_013', 'SE_T055_001', 'SE_T003_002', 'SE_T003_003', 'SE_T054_002', 'SE_T054_003', 'SE_T054_005', 'SE_T063_016')
acs.vars <- c('SE_T002_002', 'SE_T001_001', 'SE_T004_002', 'SE_T004_003', 'SE_T002_003', 'SE_T057_001', 'SE_T093_001', 'SE_T094_001', 'SE_T094_002', 'SE_T017_001', 'SE_T017_002', 'SE_T017_003', 'SE_T017_005', 'SE_T017_006', 'SE_T017_008', 'SE_T017_009', 'SE_T013_002', 'SE_T013_003', 'SE_T013_005', 'SE_T118_004')

# =======================================
# Reading in census and crosswalk tables
census2000 <- read.csv('census_data/census2000.csv', header=T, skip=1,
                       colClasses=c('Geo_FIPS'='character')) # 2000 Census data
census2010 <- read.csv('census_data/census2010.csv', header=T, skip=1,
                       colClasses=c('Geo_FIPS'='character')) # 2010 Census data
acs2006.2010 <- read.csv('census_data/acs2006-2010.csv', header=T, skip=1,
                         colClasses=c('Geo_FIPS'='character')) # 2006-2010 ACS data
acs2008.2012 <- read.csv('census_data/acs2008-2012.csv', header=T, skip=1,
                         colClasses=c('Geo_FIPS'='character')) # 2008-2012 ACS data
xwalk <- plyr::arrange(read.csv('census_data/crosswalk_2000_2010.csv',
                                colClasses=c('character', 'character')),
                 trtid10) # 2000 to 2010 Crosswalk data
xwalk <- within(xwalk, weight <- as.numeric(weight))

require(plyr)
census2000 <- subset(mutate(census2000, trtid00=Geo_FIPS), select=c('trtid00', census.vars))
census2010 <- subset(census2010, select=c('Geo_FIPS', census.2010.vars))
acs2006.2010 <- subset(acs2006.2010, select=c('Geo_FIPS', acs.vars))
acs2008.2012 <- subset(acs2008.2012, select=c('Geo_FIPS', acs.vars))

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
ggplot(data=melt(census2010, id.vars=c('Geo_FIPS')),
                 mapping=aes(x=value, group=variable)) +
  geom_histogram() +
  facet_wrap(~ variable, scales='free')

# ========================
# Normalizing census data
survey2000 <- with(census2000.as.2010, data.frame(
  FIPS=Geo_FIPS,
  pop.density=SE_T003_001,
  med.hhold.income=SE_T093_001,
  occupied.housing=SE_T156_001 / SE_T155_001, # Occupied housing units norm. by housing unit count
  owner.occupied=SE_T156_002 / SE_T155_001, # Owner-occupied units...by housing unit count
  hholds=SE_T020_001 / SE_T004_002, # Households...by land area
  fam.hholds=SE_T020_002 / SE_T004_002, # Family households...by land area
  married.hholds=SE_T020_003 / SE_T004_002, # Married-couple households...by land area
  lone.male.hholds=SE_T020_005 / SE_T004_002, # Family -holds w/ lone male h-holder...by land area
  lone.female.hholds=SE_T020_006 / SE_T004_002, # Fam. -holds w/ lone female h-holder...land area
  male.nonfam.hholds=SE_T020_008 / SE_T004_002, # Non-fam. -holds w/ male h-holder...by land area
  female.nonfam.hholds=SE_T020_009 / SE_T004_002, # Non-fam. -holds w/ female h-holder...land area
  male.pop=SE_T005_002 / SE_T025_001, # Male population normalized by total population
  female.pop=SE_T005_003 / SE_T025_001, # Female population normalized by total pop.
  white.pop=SE_T014_002 / SE_T025_001, # White population...total pop.
  black.pop=SE_T014_003 / SE_T025_001, # Black population...total pop.
  asian.pop=SE_T014_005 / SE_T025_001, # Asian population...total pop.
  poor.pop=SE_T185_004 / SE_T025_001)) # Population poor or struggling...total pop.
survey2006 <- with(acs2006.2010, data.frame(
  FIPS=Geo_FIPS,
  pop.density=SE_T002_002, # FIXME Oops!
  med.hhold.income=SE_T057_001,
  occupied.housing=SE_T094_001 / SE_T093_001, # Occupied housing units norm. by housing unit count
  owner.occupied=SE_T094_002 / SE_T093_001, # Owner-occupied units norm. by housing unit count
  hholds=SE_T017_001 / SE_T002_003, # Households...by land area
  fam.hholds=SE_T017_002 / SE_T002_003, # Family households...by land area
  married.hholds=SE_T017_003 / SE_T002_003, # Married-couple households...by land area
  lone.male.hholds=SE_T017_005 / SE_T002_003, # Family -holds w/ lone male h-holder...by land area
  lone.female.hholds=SE_T017_006 / SE_T002_003, # Fam. -holds w/ lone female h-holder...land area
  male.nonfam.hholds=SE_T017_008 / SE_T002_003, # Non-fam. -holds w/ male h-holder...by land area
  female.nonfam.hholds=SE_T017_009 / SE_T002_003, # Non-fam. -holds w/ female h-holder...land area
  male.pop=SE_T004_002 / SE_T001_001, # Male population normalized by total population
  female.pop=SE_T004_003 / SE_T001_001, # Female population normalized by total pop.
  white.pop=SE_T013_002 / SE_T001_001, # White population...total pop.
  black.pop=SE_T013_003 / SE_T001_001, # Black population...total pop.
  asian.pop=SE_T013_005 / SE_T001_001, # Asian population...total pop.
  poor.pop=SE_T118_004 / SE_T001_001)) # Population poor or struggling...total pop.
survey2010 <- with(census2010, data.frame(
  FIPS=Geo_FIPS,
  pop.density=SE_T002_002,
  occupied.housing=SE_T069_001 / SE_T068_001, # Occupied housing units norm. by housing unit count
  owner.occupied=SE_T069_002 / SE_T068_001, # Owner-occupied units norm. by housing unit count
  hholds=SE_T058_001 / SE_T002_006, # Households...by land area
  fam.hholds=SE_T058_002 / SE_T002_006, # Family households...by land area
  married.hholds=SE_T058_003 / SE_T002_006, # Married-couple households...by land area
  lone.male.hholds=SE_T058_005 / SE_T002_006, # Family -holds w/ lone male h-holder...by land area
  lone.female.hholds=SE_T058_006 / SE_T002_006, # Fam. -holds w/ lone female h-holder...land area
  male.pop=SE_T003_002 / SE_T055_001, # Male population normalized by total population
  female.pop=SE_T003_003 / SE_T055_001, # Female population normalized by total pop.
  white.pop=SE_T054_002 / SE_T055_001, # White population...total pop.
  black.pop=SE_T054_003 / SE_T055_001, # Black population...total pop.
  asian.pop=SE_T054_005 / SE_T055_001)) # Asian population...total pop.
survey2011 <- with(acs2008.2012, data.frame(
  FIPS=Geo_FIPS,
  pop.density=SE_T002_002,
  med.hhold.income=SE_T057_001,
  occupied.housing=SE_T094_001 / SE_T093_001, # Occupied housing units norm. by housing unit count
  owner.occupied=SE_T094_002 / SE_T093_001, # Owner-occupied units norm. by housing unit count
  hholds=SE_T017_001 / SE_T002_003, # Households...by land area
  fam.hholds=SE_T017_002 / SE_T002_003, # Family households...by land area
  married.hholds=SE_T017_003 / SE_T002_003, # Married-couple households...by land area
  lone.male.hholds=SE_T017_005 / SE_T002_003, # Family -holds w/ lone male h-holder...by land area
  lone.female.hholds=SE_T017_006 / SE_T002_003, # Fam. -holds w/ lone female h-holder...land area
  male.nonfam.hholds=SE_T017_008 / SE_T002_003, # Non-fam. -holds w/ male h-holder...by land area
  female.nonfam.hholds=SE_T017_009 / SE_T002_003, # Non-fam. -holds w/ female h-holder...land area
  male.pop=SE_T004_002 / SE_T001_001, # Male population normalized by total population
  female.pop=SE_T004_003 / SE_T001_001, # Female population normalized by total pop.
  white.pop=SE_T013_002 / SE_T001_001, # White population...total pop.
  black.pop=SE_T013_003 / SE_T001_001, # Black population...total pop.
  asian.pop=SE_T013_005 / SE_T001_001, # Asian population...total pop.
  poor.pop=SE_T118_004 / SE_T001_001)) # Population poor or struggling...total pop.

# Clean-up
remove(acs2006.2010, acs2008.2012, census2000, census2010, census2000.as.2010, temp, xwalk)

# =================================================
# Join census tract shapefiles and census measures
require(rgdal)
tracts <- readOGR('/usr/local/dev/rdetroit/shp/t10_nad83.shp', 't10_nad83')
tracts$FIPS <- tracts$GEOID10
tracts <- subset(tracts, select=c('FIPS'))

require(plyr)
attr2000 <- merge(tracts, survey2000, by='FIPS')
attr2006 <- merge(tracts, survey2006, by='FIPS')
attr2010 <- merge(tracts, survey2010, by='FIPS')
attr2011 <- merge(tracts, survey2011, by='FIPS')

# ===========================================
# Get and reclassify sample land cover layer
file.loc <- '/home/arthur/Workspace/TermProject/'

require(raster)
rast2001 <- raster::raster(paste0(file.loc, 'nlcd2001_nad83.tif'))
rast2006 <- raster::raster(paste0(file.loc, 'nlcd2006_nad83.tif'))
reclass.matrix <- matrix(c(c(0,10,0), c(10,11,NA), c(12,20,0),
                           c(20,23,1), c(23,24,2), c(24,99,0)),
                         byrow=TRUE, ncol=3)
dev2001 <- raster::reclassify(rast2001, reclass.matrix, right=TRUE) # Intervals closed on right
dev2006 <- raster::reclassify(rast2006, reclass.matrix, right=TRUE)

# Recreation and outdoor areas
rec.area.proximity <- raster::raster(paste0(file.loc,
                                            'ancillary/rec+outdoor_nad83_prox_cut.tiff'))

# Distance to primary roads
road.proximity <- raster::raster(paste0(file.loc, 'ancillary/roads_proximity_cut.tiff'))

# =========================================
# Spatially Join Land Cover and Other Data

# Raster and vector layers have just SLIGHTLY different projection definitions...
require(rgdal)
attr2000 <- sp::spTransform(attr2000, raster::crs(dev2001))
attr2006 <- sp::spTransform(attr2006, raster::crs(dev2001))

save(dev2001, dev2006, attr2000, attr2006, file='rda/spatialMeasures.rda')

devp2001 <- SpatialPoints(as.data.frame(dev2001, xy=TRUE)[,1:2],
                          proj4string=crs(attr2000))
devp2006 <- SpatialPoints(as.data.frame(dev2006, xy=TRUE)[,1:2],
                          proj4string=crs(attr2006))

# Extract attributes by their location... This takes a long time (about 4 minutes for each)
attr2000 <- sp::over(devp2001, attr2000)
attr2006 <- sp::over(devp2006, attr2006)

# Sample from the distance to roads layer
road.proximities <- data.frame(road.proximity=extract(road.proximity, devp2001))
rec.proximities <- data.frame(rec.area.proximity=extract(rec.area.proximity, devp2001))

save(road.proximities, rec.proximities, attr2000, attr2006, devp2001, devp2006,
     file='rda/bnData.rda')
load(file='rda/bnData.rda')

# Assume that the rows are in order; we align the land cover pixels with the attributes we just sampled (Naive, but R leaves us with no choice)
require(plyr)
training <- na.omit(cbind(data.frame(old=as.data.frame(dev2001, xy=TRUE)$layer,
                                     new=as.data.frame(dev2006, xy=TRUE)$layer),
                          attr2006, rec.proximities, road.proximities))

# Correct error in type coercion; should be numeric not integer
training$pop.density <- as.numeric(training$pop.density)
training$med.hhold.income <- as.numeric(training$med.hhold.income)
# training <- apply(training, 2, as.numeric)

# How many pixels changed from 2001 to 2006?
dim(training[training$old != training$new,])[1]

save(training, file='rda/training.rda')
load(file='rda/training.rda')

# Clean-up
remove(rrast2000, rast2006, tracts, attr2000, attr2006, dev2001, dev2006,
       devp2001, devp2006, rec.area.proximity, rec.proximities,
       road.proximity, road.proximities)

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

# Cannot discretize on the esalab computer! Don't do it! It always crashes.
training.discrete <- cbind(data.frame(new=lapply(training.data$new, as.factor)),
                           data.frame(old=lapply(training.data$old, as.factor)),
                           bnlearn::discretize(training.data[,3:length(vars)],
                                               breaks=rep(2, length(vars) - 2),
                                               method='quantile'))

save(training.data, training.discrete, file='rda/trainingData.rda')
load(file='rda/trainingData.rda')

remove(cases, training)

# ===================================
# Training the Bayesian Network (BN)

# Creating a random sample...
training.sample <- training.discrete[sample(nrow(training.discrete),
                                        dim(training.discrete)[1]*0.1),]

# It just so happens that the total number of cells is divisible by three; use floor() to be safe
k <- floor(dim(training.sample)[1]/3)
training.sample1 <- training.sample[1:k,]
training.sample2 <- training.sample[(k+1):(2*k),]
training.sample3 <- training.sample[((2*k)+1):(3*k),]

plot(bnlearn::iamb(training.sample1));title('IAMB')
plot(bnlearn::hc(training.sample1));title('Hill-Climbing')
plot(bnlearn::tabu(training.sample1));title('Tabu Scoring')
plot(bnlearn::mmhc(training.sample1));title('Max-Min Hill Climbing')
plot(bnlearn::rsmax2(training.sample1));title('RSMAX2')

# 2014-11-23
# All of the network learning approaches produce a complete or near-complete graph when trained on the full dataset, subset by independent factors, or a random sample thereof. Worse still, no nodes have arcs directed towards new land cover. The complete graph can also be demonstrated to be inferior as it has a higher BIC relative to the more sparse, expert graph.

dim(training.discrete[training.discrete$old != training.discrete$new,])[1]
training.sample0 <- subset(training.discrete, !old == new)
plot(bnlearn::hc(training.sample0, restart=1));title('Hill-Climbing')
plot(bnlearn::hc(training.sample0,
                 restart=5));title('Hill-Climbing; Restarts=5')
plot(bnlearn::hc(training.sample0,
                 restart=10));title('Hill-Climbing; Restarts=10')
plot(bnlearn::hc(training.sample0,
                 restart=20));title('Hill-Climbing; Restarts=20')
plot(bnlearn::hc(training.sample0,
                 restart=10, perturb=5));title('Hill-Climbing; Restarts=10, Perturbations=5')
plot(bnlearn::hc(training.sample0,
                 restart=20, perturb=10));title('Hill-Climbing; Restarts=20, Perturbations=10')
plot(bnlearn::rsmax2(training.sample0));title('Learned by RSMAX2')
plot(bnlearn::mmhc(training.sample0));title('Learned by Max-Min Hill Climbing')

mmhc.dag <- bnlearn::mmhc(training.sample0)

# We'll reverse the direction of just the pop.density <- new arc
mmhc.dag <- set.arc(mmhc.dag, 'pop.density', 'new')
plot(mmhc.dag); title('Learned by Max-Min Hill Climbing; Revised by Expert')

# 2014-11-23
# When the data are trained on just that subset of pixels which have changed between 2001 and 2006, the graphs that are learned are still dense but the two hybrid algorithms produce a less dense graph that they agree on completely.

# Old land cover less developed than new? (Increasing development intensity)
training.sample0 <- subset(training.discrete, ((old==0 | old==1) & new==2) |
                            (old==0 & (new==1 | new==2)))
plot(bnlearn::mmhc(training.sample0));title('Old < New; Learned by Max-Min Hill Climbing')
plot(bnlearn::rsmax2(training.sample0));title('Old < New; Learned by RSMAX2')

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
score(mmhc.dag, data=training.sample3)
score(expert.dag, data=training.sample3)

# Note: A different subset is used to fit parameters
fit.mmhc <- bn.fit(mmhc.dag, data=training.sample2, method='bayes')
fit.expert <- bn.fit(expert.dag, data=training.sample2, method='bayes')

save(mmhc.dag, expert.dag, fit.mmhc, fit.expert, training.discrete, file='rda/graphs.rda')
load(file='rda/graphs.rda')





