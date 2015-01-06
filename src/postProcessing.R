library(maptools)
library(rgdal)
library(stringr)
library(raster)

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
kmlname="longcanyon_perimeter", kmldescription="")

#--------------------------------------------------------
#  Create raster bricks
#--------------------------------------------------------
d<-raster('/home/natalie/postfire_emissions/BABOON_dust.tif', 480)
d.hourly<-d*res(d)[1]^2*60*60 #mg/m2/s to total (for hour) mg per pixel
d.total<-sum(values(d.hourly), na.rm=TRUE) #total mg for band

dust<-brick('/home/natalie/postfire_emissions/BABOON_dust.tif')
dust.sum<-sum(dust) #add up all values per pixel
dust.sum.hourly<-dust.sum*res(dust.sum)[1]^2*60*60 #mg/m2/s to total (for hour) mg per pixel
dust.total<-sum(values(dust.sum.hourly), na.rm=TRUE) #total mg for band
dust.total<-dust.total/1000/1000 # mg to kg (total within burn perimeter)


#---- iterate over fires --------------------------------
setwd('/mnt/output/postfire_emissions/output/30day_output2save/dust')
fires<-system('ls', intern=TRUE)

dust.master <- data.frame(rbind(rep(NA,5)))
names(dust.master)<-c('name', 'area_m2', 'start_month', 'pm10_kg', 'pm10_kgperm2')


for(i in 1:length(fires)){
    firename<-gsub("_dust.tif","", fires[i], fixed=TRUE)
    firename<-checkSpaces(firename)
    fire <- readOGR(dsn=dsn, layer=firename)

    dust<-brick(fires[i])
    dust.sum<-sum(dust) #add up all values per pixel
    dust.sum.hourly<-dust.sum*res(dust.sum)[1]^2*60*60 #mg/m2/s to total (for hour) mg per pixel
    dust.total<-sum(values(dust.sum.hourly), na.rm=TRUE) #total mg for band
    dust.total<-dust.total/1000/1000 # mg to kg (total within burn perimeter)
    dust.kgperm2<-dust.total/fire$Area # kg/m2 (averaged over burned area)

    dust.entry<-as.data.frame(cbind(as.character(fire$Fire_Name), fire$Area, fire$StartMonth, dust.total, dust.kgperm2))    
    names(dust.entry)<-c('name', 'area_m2', 'start_month', 'pm10_kg', 'pm10_kgperm2')
    
    dust.master<-rbind(dust.master, dust.entry)
}
d<-na.omit(dust.master)
d$pm10_kg<-as.numeric(d$pm10_kg)
d$pm10_kgperm2<-as.numeric(d$pm10_kgperm2)
d$area_m2<-as.numeric(d$area_m2)



#--------------------------------------------------------
#  plots
#--------------------------------------------------------
library(ggplot2)
l<-d$name
ll<-d$name[1]
for(i in seq(1, length(l), by=5)){    
    ll<-append(ll,l[i])    
}

p<-ggplot(d, aes(x=name, y=pm10_kg, colour=start_month)) +
    geom_bar(stat="identity", position="dodge") +
    xlab("Fire") + ylab("PM10 (kg)") +
    scale_x_discrete(breaks = ll) + 
    ggtitle("PM10 Emissions for 30 days Post-fire") +
    #facet_wrap( ~ fcastType, ncol=1)
    theme(axis.text.x = element_text(angle = 45))

p<-ggplot(d, aes(x=area_m2, y=pm10_kg, colour=start_month)) +
    geom_point(shape=19, size=2.5, alpha = 0.7) +
    xlab("Area (m2)") + ylab("PM10 (kg)") +
    #scale_x_continuous(breaks = ll) + 
    ggtitle("PM10 Emissions for 30 days Post-fire") +
    #facet_wrap( ~ fcastType, ncol=1)
    theme(axis.text.x = element_text(angle = 45))







checkSpaces<-function(firename){
    if(firename == 'AR'){
        firename <- 'A R'
    }
    else if(firename == 'CAVECANYON'){
        firename <- 'CAVE CANYON'
    }
    else if(firename == 'ANTELOPECOMPLEX'){
        firename <- 'ANTELOPE COMPLEX'
    }
    else if(firename == 'DANNERLOOP2'){
        firename <- 'DANNER LOOP 2'
    }
    else if(firename == 'SIDCROSSING'){
        firename <- 'SID CROSSING'
    }
    else if(firename == 'BALANCEDROAD'){
        firename <- 'BALANCED ROAD'
    }
    else if(firename == 'BANANALAKE'){
        firename <- 'BANANA LAKE'
    }
    else if(firename == 'BLUEMOUNTAIN'){
        firename <- 'BLUE MOUNTAIN'
    }
    else if(firename == 'BROWNSGULCH'){
        firename <- 'BROWNS GULCH'
    }
    else if(firename == 'CARTERSPRINGS'){
        firename <- 'CARTER SPRINGS'
    }
    else if(firename == 'CAULDERCREEK'){
        firename <- 'CAULDER CREEK'
    }
    else if(firename == 'CEDARBUTTEWSA'){
        firename <- 'CEDAR BUTTE WSA'
    }
    else if(firename == 'CLAYSPRINGS'){
        firename <- 'CLAY SPRINGS'
    }
    else if(firename == 'CONSHEA'){
        firename <- 'CON SHEA'
    }
    else if(firename == 'CONNORCREEK'){
        firename <- 'CONNOR CREEK'
    }
    else if(firename == 'COX\'SWELL'){
        firename <- 'COX\'S WELL'
    }
    else if(firename == 'COYOTEPOINT'){
        firename <- 'COYOTE POINT'
    }
    else if(firename == 'DALLASCANYON'){
        firename <- 'DALLAS CANYON'
    }
    else if(firename == 'DEERHOLLOW'){
        firename <- 'DEER HOLLOW'
    }
    else if(firename == 'DELFIRE'){
        firename <- 'DEL FIRE'
    }
    else if(firename == 'DIAMONDRANCH'){
        firename <- 'DIAMOND RANCH'
    }
    else if(firename == 'EASTROCK'){
        firename <- 'EAST ROCK'
    }
    else if(firename == 'FLATTOP2'){
        firename <- 'FLAT TOP 2'
    }
    else if(firename == 'FOURTANKS'){
        firename <- 'FOUR TANKS'
    }
    else if(firename == 'HIGHWAY20'){
        firename <- 'HIGHWAY 20'
    }
    else if(firename == 'HIGHWAY30'){
        firename <- 'HIGHWAY 30'
    }
    else if(firename == 'HOTWELL'){
        firename <- 'HOT WELL'
    }
    else if(firename == 'INDIANCREEK'){
        firename <- 'INDIAN CREEK'
    }
    else if(firename == 'INTERSTATE84MM177'){
        firename <- 'INTERSTATE 84 MM177'
    }
    else if(firename == 'JUNIPERCREEK'){
        firename <- 'JUNIPER CREEK'
    }
    else if(firename == 'KINYONROAD'){
        firename <- 'KINYON ROAD'
    }
    else if(firename == 'LITTLEPINE'){
        firename <- 'LITTLE PINE'
    }
    else if(firename == 'LONGCANYON'){
        firename <- 'LONG CANYON'
    }
    else if(firename == 'LONGDRAW'){
        firename <- 'LONG DRAW'
    }
    else if(firename == 'MILLERHOMESTEAD'){
        firename <- 'MILLER HOMESTEAD'
    }
    else if(firename == 'MM66I84'){
        firename <- 'MM66 I84'
    }
    else if(firename == 'MM86WESTBOUNDI84'){
        firename <- 'MM86 WESTBOUND I84'
    }
    else if(firename == 'NEWHARMONY'){
        firename <- 'NEW HARMONY'
    }
    else if(firename == 'NORTHSCHELL'){
        firename <- 'NORTH SCHELL'
    }
    else if(firename == 'NOTCHBUTTE'){
        firename <- 'NOTCH BUTTE'
    }
    else if(firename == 'OPHIRCREEK'){
        firename <- 'OPHIR CREEK'
    }
    else if(firename == 'RABBITSPRINGS'){
        firename <- 'RABBIT SPRINGS'
    }
    else if(firename == 'RADARHILL'){
        firename <- 'RADAR HILL'
    }
    else if(firename == 'REDBUTTE'){
        firename <- 'RED BUTTE'
    }
    else if(firename == 'RIDGETOP'){
        firename <- 'RIDGE TOP'
    }
    else if(firename == 'ROOSTERROCK'){
        firename <- 'ROOSTER ROCK'
    }
    else if(firename == 'SECEDAR'){
        firename <- 'SE CEDAR'
    }
    else if(firename == 'SHALEBUTTE'){
        firename <- 'SHALE BUTTE'
    }
    else if(firename == 'SHANTYCANYON'){
        firename <- 'SHANTY CANYON'
    }
    else if(firename == 'SOUTHINDIAN'){
        firename <- 'SOUTH INDIAN'
    }
    else if(firename == 'TENMILECOMPLEX'){
        firename <- 'TEN MILE COMPLEX'
    }
    else if(firename == 'TRAPPERCABIN'){
        firename <- 'TRAPPER CABIN'
    }
    else if(firename == 'WASHOEDRIVE'){
        firename <- 'WASHOE DRIVE'
    }
    else if(firename == 'WATERTOWER'){
        firename <- 'WATER TOWER'
    }
    else if(firename == 'WESTTWINPEAK'){
        firename <- 'WEST TWIN PEAK'
    }
    else if(firename == 'WHITEROCK'){
        firename <- 'WHITE ROCK'
    }
    return(firename)
}







