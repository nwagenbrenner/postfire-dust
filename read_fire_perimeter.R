library(maptools)
library(rgdal)

#--------------------------------------------------------
#  Read in shapefile
#--------------------------------------------------------

dsn<-'/media/Elements/postfire_emissions'
ogrListLayers(dsn)
ogrInfo(dsn=dsn, layer="mtbs84_2012_omernikLvl3_intersect")

fire <- readOGR(dsn=dsn, layer="mtbs84_2012_omernikLvl3_intersect")

#--------------------------------------------------------
#  Subsetting steps
#--------------------------------------------------------

l3<-c("Central Basin and Range", "Northern Basin and Range", "Snake River Plain")
sub<-subset(fire, subset=(LEVEL3_NAM %in% (l3))) # level III ecoregions only
sub<-subset(sub, subset=(Year==2012)) # year=2012
sub<-subset(sub, subset=(FireType == 'WF')) # get rid of Rx fires

#--------------------------------------------------------
#  Create western US map with fire perimeters
#--------------------------------------------------------

proj4string(sub)
sub.ll84 <- spTransform(sub, CRS("+proj=longlat +ellps=WGS84"))

domain<-map("state", regions = c("idaho","Montana","Wyoming","oregon", 
            "washington", "california", "Nevada", "utah", "colorado", 
            "new mexico", "arizona"), plot = FALSE, fill = TRUE)
IDs<-sub("^idaho,", "", domain$names)
domain_sp<-map2SpatialPolygons(domain, IDs, CRS("+proj=longlat"))
plot(domain_sp, axes = TRUE)

# add to plot from above
plot(sub.ll84, add = TRUE, col = 'red', lt=1,)


#--------------------------------------------------------
#  Create kml file with fire perimeters
#--------------------------------------------------------

#must be WGS84 CRS
#only writes first polygon --> need to combine
kml<-kmlPolygon(obj=sub.ll84, kmlfile="perimeter.kml",
name="R Polygon", description="", col=NULL, visibility=1, lwd=1, border=1,
kmlname="2012_fire_perimeters", kmldescription="")




