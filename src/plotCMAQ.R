library(raster)

#--------------------------------------------------------
#  Read in the fire
#--------------------------------------------------------

dsn<-'/media/Elements/postfire_emissions/fires'
ogrList<-ogrListLayers(dsn)
fire <- readOGR(dsn=dsn, layer="LONG DRAW")
fire <- spTransform(fire, CRS("+proj=longlat +ellps=WGS84"))

# dust grid
#r.fire<-raster('/home/natalie/src/windninja/build/src/cli/LONG DRAW_220_13_484m_dust.asc')
#r.fire<-projectRaster(r.fire, crs="+proj=longlat +datum=WGS84", method='ngb')

#--------------------------------------------------------
#  Read in cmaq plume
#--------------------------------------------------------
#setwd('/mnt/output/postfire_emissions/output/longdrawCMAQ/')
r.cmaq<-raster('/mnt/output/postfire_emissions/output/longdrawCMAQ/pm10_2012-08-05-08GMT.asc')
#r.cmaq<-raster('/mnt/output/postfire_emissions/output/longdrawCMAQ/pm10_2012-08-07-01GMT.asc')
crs(r.cmaq)<-'+proj=lcc +lat_1=30 +lat_2=60 +lat_0=49 +lon_0=-121 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs'
r.cmaq<-projectRaster(r.cmaq, crs="+proj=longlat +datum=WGS84", method='ngb')

#--------------------------------------------------------
#  Plot with spplot()
#--------------------------------------------------------
domain<-map("state", regions = c("idaho","oregon","washington",
            "montana","california", "Nevada"), plot = FALSE, fill = TRUE)
IDs<-sub("^idaho,", "", domain$names)
domain_sp<-map2SpatialPolygons(domain, IDs, CRS("+proj=longlat"))

#make the layout
states_sp<-list("sp.polygons", domain_sp, lwd=0.6, col="grey50", first=FALSE)
fire_sp<-list("sp.polygons", fire, col = 'black', lt=1, first=FALSE)
txt1<-list("sp.text", c(-116.9000, 43.6167), "Boise")
txt2<-list("sp.text", c(-118.4250, 47.6589), "Spokane")
txt3<-list("sp.text", c(-115.0117, 46.8625), "Missoula")

lat<-c(43.6167, 47.6589, 46.8625)
lon<-c(-116.2000,-117.4250,-114.0117)
pts<-cbind(lat,lon)
pts<-as.data.frame(pts)
coordinates(pts) <- ~lon+lat

towns<-list("sp.points", pts, pch=20,cex=1,col="black", first=FALSE)

sp_layout<-list(states_sp, fire_sp, txt1, txt2,txt3,towns)


#at<-c(1,3,5,10,30,50,100,300,500,1000,3000,5000,10000,130000)
at<-c(1,3,5,10,30,50,100,300,500,1000,3000,8000)
#at<-c(0,3)
colorkey<-list(labels=list(at=at,labels = as.character(at)))

#meuse.grid$distf = factor(findInterval 
#(meuse.grid$dist,c(0,0.1,0.25,1)),labels=c("0-0.1", "0.1-0.25", "0.25-1")) 
#spplot(meuse.grid["distf"],col.regions=bpy.colors(3)) 

#sp.cmaq<-rasterToPolygons(r.cmaq)
#sp.cmaq$pmf=factor(findInterval(sp.cmaq$pm10_2012.08.07.05GMT,c(1,50,100,130000)),
#    labels=c("1","50","100","130000")) 


spplot(r.cmaq,
       at=at,
       colorkey=colorkey,
       col.regions=colorRampPalette(c('white', 'lightblue','#0099FF','yellow','red')),
       xlim=c(-125.0,-110.5), 
       ylim=c(41.0,49.5), 
       sp.layout=sp_layout)




