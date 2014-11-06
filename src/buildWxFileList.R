library(maptools)
library(rgdal)
library(stringr)

#save the created lists as an R object
save(list=(ls()[18-137]), file="wxFileLists.RData")

for(i in 1:length(ogrList)){

    fire <- readOGR(dsn=dsn, layer=ogrList[i])

    month <- fire$StartMonth
    day <- fire$StartDay

    for(d in (day+1):31){ 
        for(cycle in seq(0,18, by=6)){
            for(h in 1:6){ls
                wxFile<-buildFilename(month, d, cycle, h)
                if(checkForWxFile(wxFile)){ 
                    if(d == (day+1)){
                        wxFileList<-wxFile
                    }
                    else{
                        wxFileList<-append(wxFileList, wxFile)
                    }
                }                        
            }
        }
    }

    if(month > 11){
        assign(paste0("wxFileList_", i), wxFileList) 
        next
    }

    for(m in (month+1):12){
        for(d in 1:31){ 
            for(cycle in seq(0,18, by=6)){
                for(h in 1:6){             
                    wxFile<-buildFilename(m, d, cycle, h)
                    if(checkForWxFile(wxFile)){ 
                        wxFileList<-append(wxFileList, wxFile)
                    }
                }               
            }
        }
    }

    print(i)
    assign(paste0("wxFileList_", i), wxFileList)
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

    #change forecast path for wx files after august
    if(as.numeric(month) > 8){ 
        #path<-'/media/My\ Passport_/NAM/'
        path<-'/mnt/wx/NAM/'        
    }
    else{
        path<-'/media/Elements/postfire_emissions/NAM/'
    }

    fcast <- paste0(path, month, '12/nam_218_2012', month)
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


