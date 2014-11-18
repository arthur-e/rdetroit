library(sp, raster, rgdal, plyr, reshape2, bnlearn)

setwd('/home/arthur/Downloads')
options(stringsAsFactors=FALSE)

census.vars <- c('SE_T003_001', 'SE_T093_001', 'SE_T155_001', 'SE_T156_001', 'SE_T156_002', 'SE_T004_002', 'SE_T005_002', 'SE_T005_003', 'SE_T014_002', 'SE_T014_003', 'SE_T014_005', 'SE_T020_001', 'SE_T020_002', 'SE_T020_003', 'SE_T020_005', 'SE_T020_006', 'SE_T020_008', 'SE_T020_009', 'SE_T025_001', 'SE_T185_004')
acs.vars <- c('SE_T002_001', 'SE_T001_001', 'SE_T004_002', 'SE_T004_003', 'SE_T002_003', 'SE_T057_001', 'SE_T093_001', 'SE_T094_001', 'SE_T094_002', 'SE_T017_001', 'SE_T017_002', 'SE_T017_003', 'SE_T017_005', 'SE_T017_006', 'SE_T017_008', 'SE_T017_009', 'SE_T013_002', 'SE_T013_003', 'SE_T013_005', 'SE_T118_004')

# =======================================
# Reading in census and crosswalk tables
census2000 <- read.csv('census2000.csv', header=T, skip=1,
                       colClasses=c('Geo_FIPS'='character')) # 2000 Census data
acs2006.2010 <- read.csv('acs2006-2010.csv', header=T, skip=1,
                         colClasses=c('Geo_FIPS'='character')) # 2006-2010 ACS data
tracts <- union(census2000$Census.Tract, acs2006.2010$Census.Tract)
xwalk <- plyr::arrange(read.csv('crosswalk_2000_2010.csv', colClasses=c('character', 'character')),
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
census2000.as.2010 <- within(dcast(temp, trtid10 ~ variable, fun.aggregate=sum, value.var='value'),
                             Geo_FIPS <- trtid10)

# For some reason, a couple of tracts in the ACS are not in the 2010 interpolation of the 2000 census
disjoint.tracts <- union(setdiff(census2000.as.2010$Geo_FIPS, acs2006.2010$Geo_FIPS),
                         setdiff(acs2006.2010$Geo_FIPS, census2000.as.2010$Geo_FIPS))

acs2006.2010 <- subset(acs2006.2010, !Geo_FIPS %in% disjoint.tracts)
census2000.as.2010 <- subset(census2000.as.2010, !Geo_FIPS %in% disjoint.tracts)

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
tracts <- readOGR('/usr/local/dev/rdetroit/shp/t10.shp', 't10')
tracts$FIPS <- tracts$GEOID10
tracts <- subset(tracts, select=c('FIPS'))

require(plyr)
attr2000 <- merge(tracts, survey2000, by='FIPS')
attr2006 <- merge(tracts, survey2006, by='FIPS')

# ===========================================
# Get and reclassify sample land cover layer
file.loc <- '/home/arthur/Workspace/TermProject/'

require(raster)
rast2000 <- raster::raster(paste0(file.loc, 'nlcd2001.tif'))
rast2006 <- raster::raster(paste0(file.loc, 'nlcd2006.tif'))
reclass.matrix <- matrix(c(c(0,10,0), c(10,11,NA), c(12,20,0),
                           c(20,23,1), c(23,24,2), c(24,99,0)),
                         byrow=TRUE, ncol=3)
dev2000 <- raster::reclassify(rast2000, reclass.matrix, right=TRUE) # Intervals closed on right
dev2006 <- raster::reclassify(rast2006, reclass.matrix, right=TRUE)

# TEMPORARY: Create a smaller sample
ext <- bbox(dev2000)
dev2000 <- raster::crop(dev2000,
                        raster::raster(xmn=ext[1], xmx=ext[1] + (ext[3] - ext[1]) * 0.25,
                                       ymn=ext[2], ymx=ext[2] + (ext[4] - ext[2]) * 0.25))
dev2006 <- raster::crop(dev2006,
                        raster::raster(xmn=ext[1], xmx=ext[1] + (ext[3] - ext[1]) * 0.25,
                                       ymn=ext[2], ymx=ext[2] + (ext[4] - ext[2]) * 0.25))

# Match the projection of the land cover layer
attr2000 <- sp::spTransform(attr2000, raster::crs(dev2000))
attr2006 <- sp::spTransform(attr2006, raster::crs(dev2006))

# "Sample" the census data by the land cover grid; of course, due to package limitations the raster's value is not included in this "spatial join"
devp2000 <- raster::rasterToPoints(dev2000, spatial=TRUE)
devp2006 <- raster::rasterToPoints(dev2006, spatial=TRUE)
attr2000 <- sp::over(devp2000, attr2000)
attr2006 <- sp::over(devp2006, attr2006)

# Assume that the rows are in order; we align the land cover pixels with the attributes we just sampled
# (Naive, but R leaves us with no choice)
require(plyr)
train.2000 <- na.omit(cbind(data.frame(cover=devp2000$layer), attr2000))
train.2006 <- na.omit(cbind(data.frame(cover=devp2006$layer), attr2006))

# Clean-up
remove(ext, rast2000, rast2006, tracts, attr2000, attr2006, dev2000, dev2006, devp2000, devp2006)

# Consider 2000 and 2006 observations simultaneously
train.combined <- rbind(train.2000, train.2006)

# ===================================
# Training the Bayesian Network (BN)

# I removed the "owner.occupied" variable because in IAMB structure learning on the combined training data, this caused arcs in the v-structure to be oriented in opposite directions

require(bnlearn)
vars <- c('cover', 'pop.density', 'med.hhold.income', 'occupied.housing', 'fam.hholds', 'poor.pop')
pdag.iamb <- bnlearn::iamb(train.combined[,(names(train.combined) %in% vars)])


