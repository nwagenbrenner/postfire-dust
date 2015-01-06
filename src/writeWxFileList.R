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

checkForPrecip<-function(nam_file){
    gdallocationinfo<-'/home/natalie/src/gdal_1.9.1/build/bin/./gdallocationinfo -valonly -b' 
    lon <- '-113.33'
    lat <- '45.81'

    #snow    
    band <- '361 -wgs84'
    precip <- system(paste(gdallocationinfo, band, nam_file, lon, lat), intern=TRUE, wait=TRUE, ignore.stderr = TRUE)

    if(length(precip)==0){ #if can't read the file
        return(3)
    }
  
    if(precip != 0){
            return(2)
    }
    
    #rain
    else{
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
for(i in 1:120){
    if(i==82){
        next
    }    
    wx<-get(paste0("wxFileList_", i))
    wx<-wx[1:720]
    path<-'/media/Elements/postfire_emissions/wx_lists/30day/'
    write.table(wx, paste0(path, 'nam_', i, '.csv'), row.names=FALSE, col.names=FALSE, quote=FALSE)
}
 

#full year
for(i in 1:length(ogrList)){
    if(i==82){
        next
    } 
    wx<-get(paste0("wxFileList_", i))
    path<-'/media/Elements/postfire_emissions/wx_lists/'
    write.table(wx, paste0(path, 'nam_', i, '.csv'), row.names=FALSE, col.names=FALSE, quote=FALSE)
}

#full year, accounting for rain and snow
for(i in 2:length(wxFileList)){
#for(i in 1:1){
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
    
    #for(j in 1:length(wx)){
    #for(j in 1:290){
    while(j < length(wx)+1){
        #print(paste0('j = ', j))
        precip <- checkForPrecip(paste0('/vsitar/', wx[j]))
        #print(paste0('wx[j]  = ', wx[j]))
        #print(paste0('precip = ', precip))
        
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
            j <- j + 24 #move ahead 24 hrs
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









