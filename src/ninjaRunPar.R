library(maptools)
library(rgdal)
library(stringr)
library(doParallel)

load("wxFileLists.RData") #load wx files

#--------------------------------------------------------
#  Read in list of shapefiles
#--------------------------------------------------------

dsn<-'/media/Elements/postfire_emissions/fires'
ogrList<-ogrListLayers(dsn)

#--------------------------------------------------------
#  Iterate over shapefiles in parallel
#--------------------------------------------------------

cl<-makeCluster(4)
registerDoParallel(cl)
#foreach(i=1:length(ogrList)) %dopar% simulateDust(i)
foreach(i=100:length(ogrList)) %dopar% simulateDust(i)
stopCluster(cl)

#--------------------------------------------------------
#  Do the work
#--------------------------------------------------------
simulateDust<-function(i){
    #load libs here for doParallel
    library(maptools)
    library(rgdal)
    library(stringr)

    load("/media/My Passport/postfire_emissions/output/parallel_output/wxFileLists.RData") #load wx files

    wx<-get(paste0("wxFileList_", i))
    
    fire <- readOGR(dsn=dsn, layer=ogrList[i])

    month <- fire$StartMonth
    day <- fire$StartDay
    
    for(j in 1:length(wx)){
    #for(j in 1:1440){ #first 30 days
        fcastName <- wx[j]
        fireNameNoSpaces<-gsub(" ","", fire$Fire_Name, fixed=TRUE)
     
        cfg<-paste0(fireNameNoSpaces,".cfg")
        cat("num_threads = 1\n", file=cfg)
        cat("vegetation = grass\n", file=cfg, append=TRUE)
        cat("time_zone = auto-detect\n", file=cfg, append=TRUE)
        cat("initialization_method = wxModelInitialization\n", file=cfg, append=TRUE)
        cat(paste0("forecast_filename = ", fcastName, "\n"), file=cfg, append=TRUE)
        cat("mesh_choice = fine\n", file=cfg, append=TRUE)
        cat("diurnal_winds = true\n", file=cfg, append=TRUE)
        cat("output_wind_height = 10\n",file=cfg, append=TRUE)
        cat("units_output_wind_height = m\n", file=cfg, append=TRUE)
        cat("output_speed_units = mps\n", file=cfg, append=TRUE)
        cat("compute_emissions = true\n", file=cfg, append=TRUE)
        cat("compute_friction_velocity = true\n", file=cfg, append=TRUE)
        cat(paste0("fire_perimeter_file = /media/Elements/postfire_emissions/fires/", fire$Fire_Name, ".shp\n"), file=cfg, append=TRUE) 
        cat("write_multiband_geotiff_output = true\n", file=cfg, append=TRUE)    
        cat(paste0("geotiff_file = ", fireNameNoSpaces, ".tif\n"), file=cfg, append=TRUE)
    
        system(paste("/home/natalie/src/windninja/build/src/cli/WindNinja_cli", 
                paste0(fireNameNoSpaces,".cfg")), intern=FALSE, wait=TRUE)        

    }
}





