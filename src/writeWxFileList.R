library(rgdal)
library(stringr)

load("/media/Elements/postfire_emissions/wx_lists/wxFileLists.RData") #load wx files

#--------------------------------------------------------
#  Read in list of shapefiles
#--------------------------------------------------------

dsn<-'/media/Elements/postfire_emissions/fires'
ogrList<-ogrListLayers(dsn)

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
    wx<-get(paste0("wxFileList_", i))
    path<-'/media/Elements/postfire_emissions/wx_lists/'
    write.table(wx, paste0(path, 'nam_', i, '.csv'), row.names=FALSE, col.names=FALSE, quote=FALSE)
}
