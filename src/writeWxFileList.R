library(rgdal)
library(stringr)

load("/media/Elements/postfire_emissions/wx_lists/wxFileLists.RData") #load wx files

#--------------------------------------------------------
#  Read in list of shapefiles
#--------------------------------------------------------

dsn<-'/media/Elements/postfire_emissions/fires'
ogrList<-ogrListLayers(dsn)


#--------------------------------------------------------
#  account for rain/snow
#--------------------------------------------------------
#nam_file <- '/vsitar//media/Elements/postfire_emissions/NAM/0812/nam_218_2012080918.g2.tar/nam_218_20120809_1800_001.grb2'
getDatetime<-function(nam_file){
    gdalinfo<-'/home/natalie/src/gdal_1.9.1/build/bin/./gdalinfo' 
    info <- system(paste0(gdalinfo, " ", "/vsitar/", nam_file), intern=TRUE, wait=TRUE, ignore.stderr = TRUE)
    start<-str_locate(info[37], "=  ")
    stop<-str_locate(info[37], " sec")
    dt <- str_sub(info[37], start[2], stop[1])
    dt <- as.POSIXct(as.numeric(dt), origin = "1970-01-01") #get local time (MDT)

    return(dt)
}

checkForPrecip<-function(nam_file, lon, lat){
    gdallocationinfo<-'/home/natalie/src/gdal_1.9.1/build/bin/./gdallocationinfo -valonly -b' 

    #snow    
    band <- '361 -wgs84'
    precip <- system(paste(gdallocationinfo, band, nam_file, lon, lat), intern=TRUE, wait=TRUE, ignore.stderr = TRUE)

    if(length(precip)==0){ #if can't read the file
        return(3)
    }
    else if(precip != 0){
        return(2) 
    }
    else{ # check for rain
        band <-  '11 -wgs84'
        precip <- system(paste(gdallocationinfo, band, nam_file, lon, lat), intern=TRUE, wait=TRUE, ignore.stderr = TRUE)          
        if(precip != 0){
            return(1)
        }
        else{
            return(0)
        }
    }
}

#--------------------------------------------------------
#  write nam_i.csv files
#--------------------------------------------------------
#30 days
#for(i in 1:120){
#    if(i==82){
#        next
#    }    
#    wx<-get(paste0("wxFileList_", i))
#    wx<-wx[1:720]
#    path<-'/media/Elements/postfire_emissions/wx_lists/30day/'
#    write.table(wx, paste0(path, 'nam_', i, '.csv'), row.names=FALSE, col.names=FALSE, quote=FALSE)
#}
 

#full year
#for(i in 1:length(ogrList)){
#    if(i==82){
#        next
#    } 
#    wx<-get(paste0("wxFileList_", i))
#    path<-'/media/Elements/postfire_emissions/wx_lists/'
#    write.table(wx, paste0(path, 'nam_', i, '.csv'), row.names=FALSE, col.names=FALSE, quote=FALSE)
#}

#full year, accounting for rain and snow
for(i in 1:length(wxFileList)){
    if(i==82){
        next
    } 
    if(i==85){
        next
    } 
    wx<-get(paste0("wxFileList_", i))
    path<-'/media/Elements/postfire_emissions/wx_lists/annual/'
    
    wx_new <- ''
    j <<- 1

    fire <- readOGR(dsn=dsn, layer=ogrList[i])
    proj4string(fire)
    fire.ll84 <- spTransform(fire, CRS("+proj=longlat +ellps=WGS84"))
    center<-fire.ll84@polygons[[1]]@labpt
    lon <- fire.ll84@polygons[[1]]@labpt[1]
    lat <- fire.ll84@polygons[[1]]@labpt[2]   
    
    while(j < length(wx)+1){
        print(paste0('j = ', j))
        t1 <- getDatetime(wx[j])
        if(is.na(t1)){ #if can't read the grib file
            j <- j + 1            
            next        
        }
        precip <- checkForPrecip(paste0('/vsitar/', wx[j]), lon, lat)
        print(paste0('wx[j]  = ', wx[j]))
        print(paste0('precip = ', precip))
        
        if(precip == 3){ #if can't read the grib file
            j <- j + 1
            next 
        }
        else if(precip == 2){ #snow
            pos<-str_locate(wx[j], "/NAM/")
            month<-str_sub(wx[j], pos[2]+1, pos[2]+2) 
            if(as.numeric(month) > 7){ # if it's fall/winter snow                      
                break #move to next fire
            }
            else{
                j <- j + 24 #if it's spring snow, treat as rain
            }
        }
        else if(precip == 1){ #rain
            #move out 24 hrs of no precip
            jj <- 1
            dt <- 0          
            while(dt < 24){
                j <- j + 1
                precip <- checkForPrecip(paste0('/vsitar/', wx[j]), lon, lat)
                if(precip == 0){
                    t2 <- getDatetime(wx[j])
                    if(is.na(t2)){ #if can't read the grib file
                        t2 <- t1        
                    }
                    dt<-difftime(t2, t1, units='hours')
                    #jj <- jj + 1
                }
                else{
                    if(precip == 2){ #snow
                        pos<-str_locate(wx[j], "/NAM/")
                        month<-str_sub(wx[j], pos[2]+1, pos[2]+2) 
                        if(as.numeric(month) > 7){ # if it's fall/winter snow                      
                            break #move to next fire
                        }
                    }
                    else{
                        t1 <- getDatetime(wx[j])
                        if(is.na(t1)){ #if can't read the grib file
                            while(is.na(t1)){ #move to a grib file we can read
                                j <- j + 1
                                t1 <- getDatetime(wx[j])  
                            }    
                        }
                        dt <- 0
                        #jj <- 1
                    }
                }
            }
        }
        else{
            wx_new<-append(wx_new, wx[j])
            j <- j + 1
        }
    }
    #print('Done with current wx file.')
    #print(paste0('wx_new[3] = ', wx_new[3])) 
    #print(paste0('path = ', paste0(path, 'nam_', i, '.csv')))
    wx_new<-wx_new[2:length(wx_new)] #omit first blank line
    write.table(wx_new, paste0(path, 'nam_', i, '.csv'), row.names=FALSE, col.names=FALSE, quote=FALSE)  
}









