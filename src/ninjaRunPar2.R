library(maptools)
library(rgdal)
library(stringr)
library(doParallel)

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
#foreach(i=1:120) %dopar% writeCfg(i)
foreach(i=1:length(cfg)) %dopar% simulateDust(i)
stopCluster(cl)

#--------------------------------------------------------
#  Do the work
#--------------------------------------------------------
i=1
writeCfg<-function(i){
    if(i == 82){
        #missing info for North Schell fire        
        next 
    }
    #load libs here for doParallel
    library(maptools)
    library(rgdal)
    library(stringr)
    
    fire <- readOGR(dsn=dsn, layer=ogrList[i])

    month <- fire$StartMonth
    day <- fire$StartDay

    #path to nam_i.csv files
    #full year    
    fcastName <- paste0('/media/Elements/postfire_emissions/wx_lists/nam_', i, '.csv')
    #30 day
    #fcastName <- paste0('/media/Elements/postfire_emissions/wx_lists/30day/nam_', i, '.csv')

    fireNameNoSpaces<-gsub(" ","", fire$Fire_Name, fixed=TRUE)
     
    cfg<-paste0(fireNameNoSpaces,".cfg")
    cat("num_threads = 1\n", file=cfg)
    cat("vegetation = grass\n", file=cfg, append=TRUE)
    cat("time_zone = auto-detect\n", file=cfg, append=TRUE)
    cat("initialization_method = wxModelInitialization\n", file=cfg, append=TRUE)
    cat(paste0("forecast_filename = ", fcastName, "\n"), file=cfg, append=TRUE)
    cat("mesh_choice = coarse\n", file=cfg, append=TRUE)
    cat("diurnal_winds = true\n", file=cfg, append=TRUE)
    cat("output_wind_height = 10\n",file=cfg, append=TRUE)
    cat("units_output_wind_height = m\n", file=cfg, append=TRUE)
    cat("output_speed_units = mps\n", file=cfg, append=TRUE)
    cat("compute_emissions = true\n", file=cfg, append=TRUE)
    cat("compute_friction_velocity = true\n", file=cfg, append=TRUE)
    cat(paste0("fire_perimeter_file = /media/Elements/postfire_emissions/fires/", fire$Fire_Name, ".shp\n"), file=cfg, append=TRUE) 
    cat("write_multiband_geotiff_output = true\n", file=cfg, append=TRUE)    
    cat(paste0("geotiff_file = ", fireNameNoSpaces, ".tif\n"), file=cfg, append=TRUE)

    #system(paste("/home/natalie/src/windninja/build/src/cli/WindNinja_cli", 
                #paste0(fireNameNoSpaces,".cfg")), intern=FALSE, wait=TRUE)
}    

cfg<-system("ls", intern=TRUE)
simulateDust<-function(i){
    for(i in 1:length(cfg)){
        system(paste("/home/natalie/src/windninja/build/src/cli/WindNinja_cli", 
                cfg[i]), intern=FALSE, wait=TRUE)
    }
}           

cfgs<-system("ls", intern=TRUE)
for(i in 1:length(cfgs)){
    if(i==2){
        next
    }
    if(i==6){
        next
    }
    system(paste("/home/natalie/src/windninja/build/src/cli/WindNinja_cli", cfgs[i]), intern=FALSE, wait=TRUE)
}

"NINJA_FILL_DEM_NO_DATA=YES"

setwd('/mnt/output/postfire_emissions/output/30day_files2run/')
system(paste("/home/natalie/src/windninja/build/src/cli/WindNinja_cli", "HOMER.cfg"), intern=FALSE, wait=TRUE)









