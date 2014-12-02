library(sp, raster)
library(rgdal)
library(plyr, reshape2)
library(bnlearn)

setwd('/usr/local/dev/rdetroit/')
options(stringsAsFactors=FALSE)

###################
# Data preparation

file.loc <- '/home/arthur/Workspace/TermProject/'
counties <- readOGR(paste0(file.loc, 'ancillary/co26_d00_select_nad83.shp'),
                    'co26_d00_select_nad83')
counties <- spTransform(counties, crs(dev2001))

dev2001 <- aggregate(dev2001, fact=10, fun=modal)
dev2006 <- aggregate(dev2006, fact=10, fun=modal)
dev2011 <- aggregate(dev2011, fact=10, fun=modal)
save(dev2001, dev2006, dev2011, file='rda/caAggregates.rda')

load(file='rda/spatialMeasures.rda')
load(file='rda/caAggregates.rda') # Replace dev2001 and dev2006 with aggregated