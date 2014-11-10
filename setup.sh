wget ftp://ftp.semcog.org/outgoing/web/demographic/regtrct00.zip
wget ftp://ftp.semcog.org/outgoing/web/demographic/regtrct10.zip
unzip regtrct00.zip
unzip regtrct10.zip
ogr2ogr -f "ESRI Shapefile" -where "COUNTY=99 OR COUNTY=125 OR COUNTY=163" tracts2010.shp regtrct10.shp
ogr2ogr -f "ESRI Shapefile" -where "COUNTY=99 OR COUNTY=125 OR COUNTY=163" tracts2000.shp regtrc00.shp
rm *.zip
rm regtrct10.*
rm regtrc00.*
