library(raster)

setwd('/mnt/output/postfire_emissions/output/longdrawCMAQ/')
r.cmaq<-raster('pm10_2012-08-06-23GMT.asc')
crs(r.cmaq)<-'+proj=lcc +lat_1=30 +lat_2=60 +lat_0=49 +lon_0=-121 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs'

#--------------------------------------------------------
#  Read in the fire
#--------------------------------------------------------

dsn<-'/media/Elements/postfire_emissions/fires'
ogrList<-ogrListLayers(dsn)
fire <- readOGR(dsn=dsn, layer="LONG DRAW")

# dust grid
r.fire<-raster('/home/natalie/src/windninja/build/src/cli/LONG DRAW_220_13_484m_dust.asc')

r.fire<-projectRaster(r.fire, crs="+proj=longlat +datum=WGS84", method='ngb')
r.cmaq<-projectRaster(r.cmaq, crs="+proj=longlat +datum=WGS84", method='ngb')
#r.cmaq<-projectRaster(r.cmaq, crs@r.fire)

#--------------------------------------------------------
#  Plot with spplot()
#--------------------------------------------------------
domain<-map("state", regions = c("idaho","oregon", 
            "california", "Nevada"), plot = FALSE, fill = TRUE)
IDs<-sub("^idaho,", "", domain$names)
domain_sp<-map2SpatialPolygons(domain, IDs, CRS("+proj=longlat"))

#project to cmaq crs
#domain_sp <- spTransform(domain_sp, r.cmaq@crs)

#make the layout
states_sp<-list("sp.polygons", domain_sp, lwd=0.6, col="grey50")

sp_layout<-list(states_sp)

spplot(r.cmaq$pm10_2012.08.06.23GMT,
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


