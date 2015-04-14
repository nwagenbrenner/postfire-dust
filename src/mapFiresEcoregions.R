library(maptools)
library(rgdal)
library(maps)

#--------------------------------------------------------
#  Read in ecoregions shapefile
#--------------------------------------------------------

dsn<-'/media/Elements/postfire_emissions'
ogrListLayers(dsn)
ogrInfo(dsn=dsn, layer="us_eco_l3")

regions <- readOGR(dsn=dsn, layer="us_eco_l3")
regions$US_L3NAME<-as.character(regions$US_L3NAME)

#--------------------------------------------------------
#  Subsetting
#--------------------------------------------------------

l3<-c("Central Basin and Range", "Northern Basin and Range", "Snake River Plain")
sub<-subset(regions, subset=(US_L3NAME %in% (l3))) # level III ecoregions only
sub$US_L3NAME<-as.factor(sub$US_L3NAME)

#--------------------------------------------------------
#  Create western US map with ecoregions
#--------------------------------------------------------
proj4string(sub)
sub.ll84 <- spTransform(sub, CRS("+proj=longlat +ellps=WGS84"))

domain<-map("state", regions = c("idaho","Montana","Wyoming","oregon", 
            "washington", "california", "Nevada", "utah", "colorado", 
            "new mexico", "arizona"), plot = FALSE, fill = TRUE)
IDs<-sub("^idaho,", "", domain$names)
domain_sp<-map2SpatialPolygons(domain, IDs, CRS("+proj=longlat"))

#--------------------------------------------------------
#  Read in the fires 
#--------------------------------------------------------
fire <- readOGR(dsn=dsn, layer="mtbs84_2012_omernikLvl3_intersect")

#subset
l3<-c("Central Basin and Range", "Northern Basin and Range", "Snake River Plain")
sub<-subset(fire, subset=(LEVEL3_NAM %in% (l3))) # level III ecoregions only
sub<-subset(sub, subset=(Year==2012)) # year=2012
sub<-subset(sub, subset=(FireType == 'WF')) # get rid of Rx fires

#aggregate perimeters for smae fire 
temp <- unionSpatialPolygons(sub, IDs = sub$Fire_Name)
sub_df <- as(sub, "data.frame")[!duplicated(sub$Fire_Name),]
row.names(sub_df) <- sub_df$Fire_Name
sub <- SpatialPolygonsDataFrame(temp, sub_df)

#reproject
proj4string(sub)
fires.ll84 <- spTransform(sub, CRS("+proj=longlat +ellps=WGS84"))

longdraw<-subset(fires.ll84, subset=(Fire_Name=='LONG DRAW')) # year=2012
fires.ll84<-subset(fires.ll84, subset=(Fire_Name!='LONG DRAW')) # year=2012


#--------------------------------------------------------
#  Plot with spplot()
#--------------------------------------------------------
#make the layout
fires_sp<-list("sp.polygons", fires.ll84, col = 'red', lt=1)
states_sp<-list("sp.polygons", domain_sp, lwd=0.6, col="grey50")
longdraw_sp<-list("sp.polygons", longdraw, col = 'blue', lt=1)

#add lat/lon text 
#latlonlines<-gridlines(domain_sp, easts=-125.0:-108.0)
#latlonlinesLayout<-list("sp.lines", latlonlines,lty=2,col="black")
#latlontext<-gridat(latlonlines)
#split for better use of positioning
#latlontextE<-latlontext[latlontext$pos==1,]
#latlontextN<-latlontext[latlontext$pos==2,]

#latlontextLayoutE<-list("sp.text",coordinates(latlontextE),
#        parse(text=as.character(latlontextE$labels)),offset=latlontextE$offset[1],
#        pos=1,col="black")
#latlontextLayoutN<-list("sp.text",coordinates(latlontextN),
#        parse(text=as.character(latlontextN$labels)),offset=latlontextN$offset[1],
#        pos=1,col="black")


sp_layout<-list(states_sp, fires_sp, longdraw_sp)

spplot(sub.ll84["US_L3NAME"],
       col = sub.ll84$US_L3NAME, 
       #col.regions=c("grey","yellow","lightblue"),
       #col.regions=c("#999999","#33CC00","#0099FF"),
       col.regions=c("#999999","#999999","#999999"),
       lt=0,
       xlim=c(-125.0,-108.0), 
       ylim=c(31.0,49.5), 
       sp.layout=sp_layout)

#--------------------------------------------------------
#  Plot with base graphics 
#--------------------------------------------------------
#plot(domain_sp, axes = TRUE)

# add to plot from above
#spplot(sub.ll84["US_L3NAME"], col = sub.ll84$US_L3NAME, lt=0)
#plot(sub.ll84, add=TRUE, col = sub.ll84$US_L3NAME, lt=0, alpha=0.3)
#plot(sub.ll84, col = c('lightblue', 'pink', 'yellow'), lt=0)
#plot(sub.ll84, col = 'lightblue', lt=3)
#plot(domain_sp, add=TRUE, axes = TRUE)

# add to plot from above using base plot()
#plot(fires.ll84, add = TRUE, col = 'red', lt=1,)






