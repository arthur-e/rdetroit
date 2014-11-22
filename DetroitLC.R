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

# TEMPORARY: Create a smaller sample
# ext <- bbox(dev2000)
# cropper <- raster::raster(xmn=ext[1], xmx=ext[1] + (ext[3] - ext[1]) * 0.25,
#                           ymn=ext[2], ymx=ext[2] + (ext[4] - ext[2]) * 0.25)
# dev2000 <- raster::crop(dev2000, cropper)
# dev2006 <- raster::crop(dev2006, cropper)

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

save(training, file='rda/training.rda')
load(file='rda/training.rda')

# Correct error in type coercion; should be numeric not integer
training$pop.density <- as.numeric(training$pop.density)
training$med.hhold.income <- as.numeric(training$med.hhold.income)
# training <- apply(training, 2, as.numeric)

# How many pixels changed from 2001 to 2006?
dim(training[training$old != training$new,])[1]

# Clean-up
remove(ext, cropper, rast2000, rast2006, tracts, attr2000, attr2006, dev2000, dev2006,
       devp2000, devp2006, rec.area.proximity, rec.proximities,
       road.proximity, road.proximities)

save(training, file='rda/training.rda')

#=======================
# Discretizing the Data

require(ggplot2)
ggplot(data=melt(training, id.vars='FIPS'), mapping=aes(x=value, group=variable)) +
  geom_histogram() +
  facet_wrap(~ variable, scales='free')

cases <- data.frame(t(combn(setdiff(colnames(training), c('new', 'FIPS')), 2)))

require(plyr)
cases <- ddply(cases, ~ X1 + X2, mutate,
      r.sq=cor.test(training[,X1], training[,X2], method='pearson')$estimate,
      p.value=cor.test(training[,X1], training[,X2], method='pearson')$p.value)

subset(cases, abs(r.sq) > 0.5)
vars <- c('old', 'new', 'road.proximity', 'owner.occupied', 'rec.area.proximity',

summary(training.data)
#bnlearn::discretize(training.data, breaks=c(3, 3, 2,

# ===================================
# Training the Bayesian Network (BN)

# I removed the "owner.occupied" variable because in IAMB structure learning it caused a massive number of interconnections between nodes to be formed.

vars <- c('old', 'new', 'pop.density', 'med.hhold.income', 'occupied.housing',
          'fam.hholds', 'poor.pop', 'rec.area.proximity', 'road.proximity')

# 2014-11-18
# IAMB and GS produced the same graph; MMHC and RSMAX2 produced another, same graph. Hill Climbing and Tabu Search each produced a different graph from all the other methods.
# Random restart tests with the Hill Climbing algorithm suggest the directionality between fam.hholds and both poor.pop and cover is highly uncertain. The alternatives (poor.pop -> fam.hholds -> cover) and (poor.pop <- fam.hholds <- cover) are also disputed (and the only dispute) between the Tabu Search and Hill Climbing methods, respectively. All the hybrid and constraint-based methods learned a (fam.hholds -> poor.pop) relationship.

# 2014-11-18
# When using only 2000-2001 or 2006 data to train the network, the structure learned with IAMB or MMHC is very similar between 2000-2001 and 2006. The only difference in the learned network structures is in that of the IAMB method, which found some connection between cover and both fam.hholds and occupied.housing in 2000-2001 but not in 2006. Also, MMHC found directionality between all nodes in both years while IAMB could not determine directionality.

# 2014-11-21
# The network learning algorithms suggested that only road.proximity, rec.area.proximity, and med.hhold.income were consistently connected to old and new land cover. I believe that population density has to be a big factor so I'm keeping it in.

vars <- c('old', 'new', 'road.proximity', 'rec.area.proximity', 'med.hhold.income', 'pop.density')
plot(bnlearn::hc(training[,(names(training) %in% vars)], restart=1));title('Hill-Climbing')
plot(bnlearn::hc(training[,(names(training) %in% vars)],
                 restart=5));title('Hill-Climbing; Restarts=5')
plot(bnlearn::hc(training[,(names(training) %in% vars)],
                 restart=10));title('Hill-Climbing; Restarts=10')
plot(bnlearn::hc(training[,(names(training) %in% vars)],
                 restart=20));title('Hill-Climbing; Restarts=20')
plot(bnlearn::hc(training[,(names(training) %in% vars)],
                 restart=10, perturb=5));title('Hill-Climbing; Restarts=10, Perturbations=5')
plot(bnlearn::hc(training[,(names(training) %in% vars)],
                 restart=20, perturb=10));title('Hill-Climbing; Restarts=20, Perturbations=10')

plot(bnlearn::mmhc(training[,(names(training) %in% vars)]));title('MMHC')
plot(bnlearn::rsmax2(training[,(names(training) %in% vars)]));title('RSMAX2')

# 2014-11-21
# Hill climbing stress tests revealed a complex and unstable structure but the hybrid algorithms agree 100% as to the network structure and both find it complete and totally without any interactions including the pop.density variable.

vars <- c('old', 'new', 'road.proximity', 'rec.area.proximity', 'med.hhold.income')
plot(bnlearn::hc(training[,(names(training) %in% vars)], restart=1));title('Hill-Climbing')
plot(bnlearn::hc(training[,(names(training) %in% vars)],
                 restart=5));title('Hill-Climbing; Restarts=5')
plot(bnlearn::hc(training[,(names(training) %in% vars)],
                 restart=10));title('Hill-Climbing; Restarts=10')
plot(bnlearn::hc(training[,(names(training) %in% vars)],
                 restart=20));title('Hill-Climbing; Restarts=20')
plot(bnlearn::hc(training[,(names(training) %in% vars)],
                 restart=10, perturb=5));title('Hill-Climbing; Restarts=10, Perturbations=5')
plot(bnlearn::hc(training[,(names(training) %in% vars)],
                 restart=20, perturb=10));title('Hill-Climbing; Restarts=20, Perturbations=10')

plot(bnlearn::mmhc(training[,(names(training) %in% vars)]));title('MMHC')
plot(bnlearn::rsmax2(training[,(names(training) %in% vars)]));title('RSMAX2')

# 2014-11-21
# Hill climbing stress tests without the pop.density variable reveal a more consistent and realistic structure. Again, the same network emerged from the two hybrid learning algorithms. I will try two versions of the learned network: A composite of the stochastic hill climbing efforts and the network that emerged from the two hyrbid approaches.

#====================
# Parameter Learning

training.data <- training[,(names(training) %in% vars)]
dag.hc <- empty.graph(vars)
dag.mmhc <- bnlearn::mmhc(training.data)
dag.rsmax2 <- bnlearn::rsmax2(training.data)

# Specify the arcs in the PDAG based on the hill-climbing stress tests
arcs(dag.hc) <- matrix(c('old', 'new', 'old', 'road.proximity', 'old', 'med.hhold.income', 'rec.area.proximity', 'road.proximity', 'road.proximity', 'new', 'med.hhold.income', 'new', 'med.hhold.income', 'road.proximity', 'med.hhold.income', 'rec.area.proximity'),
                        ncol=2, byrow=TRUE, dimnames = list(NULL, c("from", "to")))

plot(dag.hc); title('Composite Network from Hill-Climbing Tests')
plot(dag.mmhc); title('Network Learned by Hybrid Constraint and Scoring Algorithms')

# How do the two learned model structures compare?
score(dag.hc, data=training.data)
score(dag.mmhc, data=training.data)

fitted.hc <- bn.fit(dag.hc, data=training.data)
fitted.mmhc <- bn.fit(dag.mmhc, data=training.data)





