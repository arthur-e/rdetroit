# Acquire and subset census tract boundaries
mkdir -p /usr/local/dev/rdetroit/shp && cd /usr/local/dev/rdetroit/shp
wget ftp://ftp.semcog.org/outgoing/web/demographic/regtrct00.zip
wget ftp://ftp.semcog.org/outgoing/web/demographic/regtrct10.zip
unzip regtrct00.zip
unzip regtrct10.zip
ogr2ogr -f "ESRI Shapefile" -where "COUNTY=99 OR COUNTY=125 OR COUNTY=163" t10.shp regtrct10.shp
ogr2ogr -f "ESRI Shapefile" -where "COUNTY=99 OR COUNTY=125 OR COUNTY=163" t00.shp regtrc00.shp
rm *.zip
rm regtrct10.*
rm regtrc00.*
ogr2ogr -f "ESRI Shapefile" -t_srs "EPSG:26917" /usr/local/dev/rdetroit/shp/t10_nad83.shp /usr/local/dev/rdetroit/shp/t10.shp

# Define common projection
FILE_LOC=/home/arthur/Workspace/TermProject/
gdalwarp -t_srs "EPSG:26917" -cutline /usr/local/dev/rdetroit/shp/t10_nad83.shp -cl "t10_nad83" -crop_to_cutline $FILE_LOC/nlcd2001.tif $FILE_LOC/nlcd2001_nad83.tif
gdalwarp -t_srs "EPSG:26917" -cutline /usr/local/dev/rdetroit/shp/t10_nad83.shp -cl "t10_nad83" -crop_to_cutline $FILE_LOC/nlcd2006.tif $FILE_LOC/nlcd2006_nad83.tif
gdalwarp -t_srs "EPSG:26917" -cutline /usr/local/dev/rdetroit/shp/t10_nad83.shp -cl "t10_nad83" -crop_to_cutline $FILE_LOC/nlcd2011.tif $FILE_LOC/nlcd2011_nad83.tif

wget http://www2.census.gov/geo/tiger/PREVGENZ/co/co00shp/co26_d00_shp.zip
unzip co26_d00_shp.zip
ogr2ogr -f "ESRI Shapefile" -s_srs "EPSG:4269" -t_srs "EPSG:4269" -where "COUNTY IN ('099', '125', '163')" $FILE_LOC/ancillary/co26_d00_select.shp $FILE_LOC/ancillary/co26_d00.shp
ogr2ogr -f "ESRI Shapefile" -t_srs "EPSG:26917" $FILE_LOC/ancillary/co26_d00_select_nad83.shp $FILE_LOC/ancillary/co26_d00_select.shp

echo "Calculating distance to outdoor areas..."
FILE_LOC=/home/arthur/Workspace/TermProject/ancillary

wget ftp://ftp.semcog.org/outgoing/web/landuse/regrec.zip
unzip regrec.zip

# Project the Shapefile
ogr2ogr -f "ESRI Shapefile" -t_srs "EPSG:26917" $FILE_LOC/rec+outdoor_nad83.shp $FILE_LOC/regrec.shp

# Rasterize, calculate proximity, and cut to the census tracts extent
gdal_rasterize -tr 30 30 -init 0 -burn 1 $FILE_LOC/rec+outdoor_nad83.shp $FILE_LOC/rec+outdoor_nad83.tiff
gdal_proximity.py -values 1 $FILE_LOC/rec+outdoor_nad83.tiff $FILE_LOC/rec+outdoor_nad83_prox.tiff
gdalwarp -t_srs "EPSG:26917" -cutline /usr/local/dev/rdetroit/shp/t10_nad83.shp -cl "t10_nad83" -crop_to_cutline $FILE_LOC/rec+outdoor_nad83_prox.tiff $FILE_LOC/rec+outdoor_nad83_prox_cut.tiff

echo "Calculating distance to roads..."
# Filter road types to primary roads and clip the roads layer to a WKT bounding box, setting the output to GCS WGS84 so that a WKT bounding box is acceptable
ogr2ogr -skipfailures -f "ESRI Shapefile" -where "NFC IN (1,2,3)" -clipdst "POLYGON((-82.35 42.95,-82.35 41.75,-84.11 41.75,-84.11 42.95,-82.35 42.95))" -t_srs "EPSG:4326" $FILE_LOC/semich_roads_wgs84.shp $FILE_LOC/allroads_miv14a.shp

# Transforming back into UTM (linear units must be meters for next step)
ogr2ogr -f "ESRI Shapefile" -t_srs "EPSG:26917" $FILE_LOC/semich_roads_nad83.shp $FILE_LOC/semich_roads_wgs84.shp

# Rasterizing the roads layer and then making a proximity map
gdal_rasterize -tr 30 30 -init 0 -burn 1 $FILE_LOC/semich_roads_nad83.shp $FILE_LOC/roads.tiff
gdal_proximity.py $FILE_LOC/roads.tiff $FILE_LOC/roads_proximity.tiff -values 1

gdalwarp -t_srs "EPSG:26917" -cutline /usr/local/dev/rdetroit/shp/t10_nad83.shp -cl "t10_nad83" -crop_to_cutline $FILE_LOC/roads_proximity.tiff $FILE_LOC/roads_proximity_cut.tiff
