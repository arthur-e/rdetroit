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
  T003_001=SE_T003_001,
  T093_001=SE_T093_001,
  T156_001=SE_T156_001 / SE_T155_001, # Occupied housing units normalized by housing unit count
  T156_002=SE_T156_002 / SE_T155_001, # Owner-occupied units normalized by housing unit count
  T020_001=SE_T020_001 / SE_T004_002, # Households...by land area
  T020_002=SE_T020_002 / SE_T004_002, # Family households...by land area
  T020_003=SE_T020_003 / SE_T004_002, # Married-couple households...by land area
  T020_005=SE_T020_005 / SE_T004_002, # Family households with lone male householder...by land area
  T020_006=SE_T020_006 / SE_T004_002, # Family households with lone female householder...land area
  T020_008=SE_T020_008 / SE_T004_002, # Non-family households with male householder...by land area
  T020_009=SE_T020_009 / SE_T004_002, # Non-family households with female householder...land area
  T005_002=SE_T005_002 / SE_T025_001, # Male population normalized by total population
  T005_003=SE_T005_003 / SE_T025_001, # Female population normalized by total pop.
  T014_002=SE_T014_002 / SE_T025_001, # White population...total pop.
  T014_003=SE_T014_003 / SE_T025_001, # Black population...total pop.
  T014_005=SE_T014_005 / SE_T025_001, # Asian population...total pop.
  T185_004=SE_T185_004 / SE_T025_001)) # Population poor or struggling...total pop.
survey2006 <- with(acs2006.2010, data.frame(
  FIPS=Geo_FIPS,
  T002_001=SE_T002_001,
  T057_001=SE_T057_001,
  T094_001=SE_T094_001 / SE_T093_001, # Occupied housing units normalized by housing unit count
  T094_002=SE_T094_002 / SE_T093_001, # Owner-occupied units normalized by housing unit count
  T017_001=SE_T017_001 / SE_T002_003, # Households...by land area
  T017_002=SE_T017_002 / SE_T002_003, # Family households...by land area
  T017_003=SE_T017_003 / SE_T002_003, # Married-couple households...by land area
  T017_005=SE_T017_005 / SE_T002_003, # Family households with lone male householder...by land area
  T017_006=SE_T017_006 / SE_T002_003, # Family households with lone female householder...land area
  T017_008=SE_T017_008 / SE_T002_003, # Non-family households with male householder...by land area
  T017_009=SE_T017_009 / SE_T002_003, # Non-family households with female householder...land area
  T004_002=SE_T004_002 / SE_T001_001, # Male population normalized by total population
  T004_003=SE_T004_003 / SE_T001_001, # Female population normalized by total pop.
  T013_002=SE_T013_002 / SE_T001_001, # White population...total pop.
  T013_003=SE_T013_003 / SE_T001_001, # Black population...total pop.
  T013_005=SE_T013_005 / SE_T001_001, # Asian population...total pop.
  T118_004=SE_T118_004 / SE_T001_001)) # Population poor or struggling...total pop.

# Clean-up
acs2006.2010 <- NULL
census2000 <- NULL
census2000.as.2010 <- NULL
temp <- NULL

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
rast <- raster::raster(paste0(file.loc, 'nlcd2001_sample.tif'))
reclass.matrix <- matrix(c(c(0,10,0), c(10,11,NA), c(12,20,0),
                           c(20,23,1), c(23,24,2), c(24,99,0)),
                         byrow=TRUE, ncol=3)
dev <- raster::reclassify(rast, reclass.matrix, right=TRUE) # Intervals closed on right

# Create a smaller sample
ext <- bbox(dev)
dev2 <- raster::crop(dev, raster::raster(xmn=ext[1], xmx=ext[1] + (ext[3] - ext[1]) * 0.25,
                                         ymn=ext[2], ymx=ext[2] + (ext[4] - ext[2]) * 0.25))

# Match the projection of the land cover layer
attr2000 <- spTransform(attr2000, raster::crs(dev2))
attr2006 <- spTransform(attr2006, raster::crs(dev2))

# "Sample" the census data by the land cover grid; of course, due to package limitations the raster's value is not included in this "spatial join"
devp <- raster::rasterToPoints(dev2, spatial=TRUE)
t2000 <- sp::over(devp, attr2000)
t2006 <- sp::over(devp, attr2006)

# Assume that the rows are in order; we align the land cover pixels with the attributes we just sampled
# (Naive, but R leaves us with no choice)
require(plyr)
cover2000 <- cbind(data.frame(cover=devp$layer), t2000)
cover2006 <- cbind(data.frame(cover=devp$layer), t2006)

# ===================================
# Training the Bayesian Network (BN)

# Too many parameters makes for slow learning; to begin with, let's just try estimating cover from population density and median household income

require(bnlearn)
# Start the clock!
ptm <- proc.time()
pdag2000 = bnlearn::fast.iamb(cover2000[,(names(cover2000) %in% c('cover', 'T003_001', 'T093_001'))])
# Stop the clock
elapsed <- proc.time() - ptm; elapsed


