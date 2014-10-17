library(maptools)
library(rgdal)

#--------------------------------------------------------
#  Read in a fire
#--------------------------------------------------------

dsn<-'/media/Elements/postfire_emissions/fires'
ogrList<-ogrListLayers(dsn)
layer<-ogrList[12] #BABOON

fire <- readOGR(dsn=dsn, layer="BABOON")



#--------------------------------------------------------
#  Create kml files
#--------------------------------------------------------
r1<-raster('/home/natalie/postfire_emissions/BABOON_dust.tif', 1000)
r1.p<-projectRaster(r1, crs="+proj=longlat +datum=WGS84", method='ngb')
KML(r1.p, file='baboon_dust.kml')

r.dem<-raster('/home/natalie/postfire_emissions/BABOON.tif')
r.dem.p<-projectRaster(r.dem, crs="+proj=longlat +datum=WGS84", method='ngb')
KML(r.dem.p, file='baboon_dem.kml', overwrite=TRUE)

#must be WGS84 CRS
#only writes first polygon
proj4string(fire)
fire.ll <- spTransform(fire, CRS("+proj=longlat +ellps=WGS84"))

kml<-kmlPolygon(obj=fire.ll, kmlfile="perimeter.kml",
name="R Polygon", description="", col=NULL, visibility=1, lwd=1, border=1,
kmlname="baboon_perimeter", kmldescription="")
