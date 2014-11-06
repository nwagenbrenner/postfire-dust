library(maptools)
library(rgdal)
library(stringr)
library(doParallel)

#--------------------------------------------------------
#  Read in list of shapefiles
#--------------------------------------------------------

dsn<-'/media/Elements/postfire_emissions/fires'
ogrList<-ogrListLayers(dsn)
#ogrInfo(dsn=dsn, layer=ogrList[1])

#--------------------------------------------------------
#  Iterate over shapefiles in parallel
#--------------------------------------------------------

cl<-makeCluster(4)
registerDoParallel(cl)
foreach(i=1:length(ogrList)) %dopar% simulateDust(i)
#foreach(i=1:4) %dopar% simulateDust(i)
stopCluster(cl)


#--------------------------------------------------------
#  Do the work
#--------------------------------------------------------
simulateDust<-function(i){
#for(i in 1:length(ogrList)){
#for(f in 1:1){
    #load libs here for doParallel
    library(maptools)
    library(rgdal)
    library(stringr)

    fire <- readOGR(dsn=dsn, layer=ogrList[i])

    month <- fire$StartMonth
    day <- fire$StartDay

    #just do fires through 08/12
    #need to change forecast path for wx files after 08/12
    if(month > 8){ 
        next
    }

    #simulate first month starting day after fire (01:00 UTC/19:00 MDT)
    for(d in (day+1):31){ 
        for(cycle in seq(0,18, by=6)){
            for(h in 1:6){
                wxFile<-buildFilename(month, d, cycle, h)             
                if(checkForWxFile(wxFile)){           
                    runWN(ogrList[i], wxFile)
                }                
            }
        }
    }
    #simulate the rest of the months
    #right now, just through 08/12
    #will need to change path to wx files after 08/12
    if(month<8){
        for(i in (month+1):8){
            for(d in 1:31){ 
                for(cycle in seq(0,18, by=6)){
                    for(h in 1:6){             
                        wxFile<-buildFilename(month, d, cycle, h)
                        if(checkForWxFile(wxFile)){           
                            runWN(ogrList[i], wxFile)
                        } 
                    }
                }               
            }
        }
    }
}

runWN <- function(fire, fcastName){
    writeCfg(fire, fcastName)
    fireNameNoSpaces<-gsub(" ","", fire , fixed=TRUE) 
    system(paste("/home/natalie/src/windninja/build/src/cli/WindNinja_cli", 
                paste0(fireNameNoSpaces,".cfg")), intern=FALSE, wait=TRUE)
}

writeCfg <- function(fire, fcastName){   
    fireNameNoSpaces<-gsub(" ","", fire , fixed=TRUE)     
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
    cat(paste0("fire_perimeter_file = /media/Elements/postfire_emissions/fires/", fire, ".shp\n"), file=cfg, append=TRUE) 
    cat("write_multiband_geotiff_output = true\n", file=cfg, append=TRUE)    
    cat(paste0("geotiff_file = ", fireNameNoSpaces, ".tif\n"), file=cfg, append=TRUE)
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

checkForWxFile<-function(wxFile){
    #check if file exists in archive               
    pos<-str_locate(wxFile, ".tar")                
    tFile<-str_sub(wxFile, 1, pos[2])
    #first check if the tar file exists
    pos<-str_locate(wxFile, "/nam_")
    pathToTar<-str_sub(wxFile, 1, pos[1])                
    tFileList<-list.files(pathToTar)
    pos<-str_locate(tFile, "nam_")
    tFileBaseName<-str_sub(tFile, pos[1])                
    if(tFileBaseName %in% tFileList){                    
        fileList<-untar(tarfile=tFile, list = TRUE)
        pos<-str_locate(wxFile, ".tar")  
        grb2File<-str_sub(wxFile, pos[2]+2)
    }
    else{
        return(FALSE)
    }
    if(grb2File %in% fileList){                   
        return(TRUE)
    }
    else{
        return(FALSE)
    }
}

#xmin<-fire@bbox[1] 
#ymin<-fire@bbox[2] 
#xmax<-fire@bbox[3] 
#ymin<-fire@bbox[4]





