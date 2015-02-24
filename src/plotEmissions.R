library(maptools)
library(rgdal)
library(raster)

#--------------------------------------------------------
#  Read in a fire
#--------------------------------------------------------

dsn<-'/media/Elements/postfire_emissions/fires'
ogrList<-ogrListLayers(dsn)

fire <- readOGR(dsn=dsn, layer="LONG DRAW")

# dust grid
r1<-raster('/home/natalie/src/windninja/build/src/cli/LONG DRAW_220_13_484m_dust.asc')
#r1.p<-projectRaster(r1, crs="+proj=longlat +datum=WGS84", method='ngb')

#--------------------------------------------------------
#  Plot with spplot()
#--------------------------------------------------------
domain<-map("state", regions = c("idaho","oregon", 
            "california", "Nevada"), plot = FALSE, fill = TRUE)
IDs<-sub("^idaho,", "", domain$names)
domain_sp<-map2SpatialPolygons(domain, IDs, CRS("+proj=longlat"))

#project to r1 crs
domain_sp <- spTransform(domain_sp, r1@crs)

#make the layout
states_sp<-list("sp.polygons", domain_sp, lwd=0.6, col="grey50")

scale<-list("SpatialPolygonsRescale",layout.scale.bar(),
        offset=c(460000, 4670000), scale=20000, fill=c("transparent", "black"))
txt1<-list("sp.text", c(460000, 4673000), "0")
txt2<-list("sp.text", c(480000, 4673000), "20 km")
north<-list("SpatialPolygonsRescale", layout.north.arrow(),
        offset=c(420000, 4715000), scale=4500)
sp_layout<-list(states_sp,scale,txt1,txt2)


spplot(r1$LONG_DRAW_220_13_484m_dust,
       #col.regions=colorRampPalette(c('gray80', 'blue', 'red')),
       col.regions=colorRampPalette(c('darkblue', 'gray80','red')),
       #col.regions=heat.colors,
       #col.regions=terrain.colors,
       #col = r1$LONG_DRAW_220_13_484m_dust, 
       #col.regions=c("grey","yellow","lightblue"),
       #col.regions=c("#999999","#33CC00","#0099FF"),
       #lt=0,
       xlim=c(415000,485000), 
       ylim=c(4660000, 4730000), 
       sp.layout=sp_layout)


spplot(r1)






