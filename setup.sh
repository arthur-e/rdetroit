# Acquire and subset census tract boundaries
wget ftp://ftp.semcog.org/outgoing/web/demographic/regtrct00.zip
wget ftp://ftp.semcog.org/outgoing/web/demographic/regtrct10.zip
unzip regtrct00.zip
unzip regtrct10.zip
ogr2ogr -f "ESRI Shapefile" -where "COUNTY=99 OR COUNTY=125 OR COUNTY=163" t10.shp regtrct10.shp
ogr2ogr -f "ESRI Shapefile" -where "COUNTY=99 OR COUNTY=125 OR COUNTY=163" t00.shp regtrc00.shp
rm *.zip
rm regtrct10.*
rm regtrc00.*

# Spatially join census tract boundaries with census measures
ogr2ogr -sql "SELECT t10.GEOID10 AS GEOID, t.* FROM t10 LEFT JOIN '/usr/local/dev/rdetroit/csv/t.csv'.t ON t10.GEOID10 = t.FIPS" /usr/local/dev/rdetroit/shp/tracts2010_joined.shp /usr/local/dev/rdetroit/shp/t10.shp

