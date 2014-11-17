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
gdallocationinfo<-'/home/natalie/src/gdal_1.9.1/build/bin/./gdallocationinfo -valonly -b'
band <-  '34 -wgs84'
nam_file <- '/vsitar//media/Elements/postfire_emissions/NAM/0812/nam_218_2012080918.g2.tar/nam_218_20120809_1800_006.grb2'
lon <- '-113.33'
lat <- '45.81'

system(paste(gdallocationinfo, band, nam_file, lon, lat), intern=FALSE, wait=TRUE)

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

