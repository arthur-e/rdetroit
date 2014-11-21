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

