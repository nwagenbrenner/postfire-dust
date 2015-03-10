library(raster)

#---- iterate over fires (annual)--------------------------------
setwd('/mnt/output/postfire_emissions/output/annual_output2save/')
#get list of fire names
fires<-system('ls batch_1/dust', intern=TRUE)

f1<-'batch_1/'
f2<-'batch_2/'
f3<-'batch_3/'
f4<-'batch_4/'
f5<-'batch_5/'
f6<-'batch_6/'
f7<-'batch_7/'
f8<-'batch_8/'
f9<-'batch_9/'
f10<-'batch_10/'
f11<-'batch_11/'

trim.trailing <- function (x) sub("\\s+$", "", x)

#for(i in 1:length(fires)){
for(i in 1:2){  
    wind<-c() # list for this fire 
    min<-c() # list of mins
    max<-c()
    avg<-c()
    for(j in 1:11){ # loop over all batches for this fire
        ss<-paste0(get(paste0("f",j)),fires[i])
        substr(ss, nchar(ss)-8+1, nchar(ss)) <- "spd.tif "
        ss<-trim.trailing(ss)
        print(paste0("ss = ", ss))
        if(file.exists(ss)){
            assign( paste0("spd",j), brick(ss) )
            # extract each raster in brick and compute min, max, avg speed           
            for(k in 1:nlayers(get(paste0("spd",j)))){            
                r<-raster(get(paste0("spd",j)), k)
                min<-c(min, min(na.omit(values(r))))
                max<-c(max, max(na.omit(values(r))))  
                avg<-c(avg, mean(na.omit(values(r))))
                print(paste0('min, max, avg =', min,", ", max, ", ", avg))              
            }
        }
    }
    #combine min, max, avg for this fire and save it
    wind<-cbind(min, max, avg)
    paste0( "writing: ", paste0("/home/natalie/Desktop/",fires[i],"_windHrs.RData") )
    save(wind, file=paste0("/home/natalie/Desktop/",fires[i],"_windHrs.RData"))
}



