library(maptools)
library(rgdal)
library(stringr)

for(i in 1:length(ogrList)){

    fire <- readOGR(dsn=dsn, layer=ogrList[i])

    month <- fire$StartMonth
    day <- fire$StartDay

    #just do fires through august
    #need to change forecast path for wx files after august
    if(month > 8){ 
        next
    }

    for(d in (day+1):31){ 
        for(cycle in seq(0,18, by=6)){
            for(h in 1:6){
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

    if(month > 7){
        assign(paste0("wxFileList_", i), wxFileList) 
        next
    }

    for(m in (month+1):8){
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

