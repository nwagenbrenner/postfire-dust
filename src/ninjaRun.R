library(maptools)
library(rgdal)

#--------------------------------------------------------
#  Read in list of shapefiles
#--------------------------------------------------------

dsn<-'/media/Elements/postfire_emissions/fires'
ogrList<-ogrListLayers(dsn)
#ogrInfo(dsn=dsn, layer=ogrList[1])

#--------------------------------------------------------
#  Iterate over shapefiles
#--------------------------------------------------------

#for(i in 1:length(ogrList)){
for(f in 13:13){
    fire <- readOGR(dsn=dsn, layer=ogrList[i])

    month <- fire$StartMonth
    day <- fire$StartDay

    #simulate first month
    for(d in day:31){
        for(cycle in seq(0,18, by=6)){
            for(h in 1:6){             
                wxFile<-buildFilename(month, d, cycle, h)
                #print(wxFile)
                runWN(ogrList[i], wxFile)
            }
        }
    }

    #simulate the rest of the months
    for(i in month+1:12){
        for(j in 1:31){ 
            #print(paste0(i,j))
        }
    }    
}

runWN <- function(fire, fcastName){
    writeCfg(fire, fcastName)
    system(paste("/home/natalie/src/windninja/build/src/cli/WindNinja_cli", 
                "windninja.cfg"), intern=FALSE, wait=TRUE)
}

writeCfg <- function(fire, fcastName){
    cfg<-"windninja.cfg"
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
    cat(paste0("fire_perimeter_file = /media/Elements/postfire_emissions/fires/", fire, ".shp\n"), file=cfg, append=TRUE) 
    cat("write_multiband_geotiff_output = true\n", file=cfg, append=TRUE)
    cat(paste0("geotiff_file = ", fire, ".tif\n"), file=cfg, append=TRUE)
}
     


buildFilename<-function(month, day, cycle, hour){
    if(nchar(toString(month)) == 1){
        month <- paste0('0', month)
    }
    if(nchar(toString(day)) == 1){    
        day <- paste0('0', day)        
    }
    if(nchar(toString(cycle)) == 1){
        cycle <- paste0('0', cycle)    
    }
    if(nchar(toString(hour)) == 1){
        hour <- paste0('00', hour)    
    }
    else if(nchar(toString(hour)) == 2){
        hour <- paste0('0', hour)
    }

    fcast <- paste0('/media/Elements/postfire_emissions/NAM/', month, '12/nam_218_2012', month)
    fcast <- paste0(fcast, day)
    fcast <- paste0(fcast, cycle, '.g2.tar')
    fcast <- paste0(fcast, '/nam_218_2012', month, day, '_', cycle, '00_', hour, '.grb2')

    return(fcast)
}

#xmin<-fire@bbox[1] 
#ymin<-fire@bbox[2] 
#xmax<-fire@bbox[3] 
#ymin<-fire@bbox[4]





