#---- iterate over fires (annual)--------------------------------
setwd('/mnt/output/postfire_emissions/output/annual_output2save/')
#get list of fire names
fires<-system('ls batch_1/dust', intern=TRUE)

f1<-'batch_1/dust/'
f2<-'batch_2/dust/'
f3<-'batch_3/dust/'
f4<-'batch_4/dust/'
f5<-'batch_5/dust/'
f6<-'batch_6/dust/'
f7<-'batch_7/dust/'
f8<-'batch_8/dust/'
f9<-'batch_9/dust/'
f10<-'batch_10/dust/'
f11<-'batch_11/dust/'

#l<-c()
#dust_1<-brick('batch_1/dust/20-MILE_dust.tif')
#for(j in 1:nlayers(dust_1)){
#    r<-raster(dust_1, j)
#    l<-c(l, any(na.omit(values(r)) > 0.0))
#}
#emittingHrs<-sum(l)

emittingHrsAllFires<-c()
for(i in 3:length(fires)){
#for(i in 1:2){
    #firename<-gsub("_dust.tif","", fires[i], fixed=TRUE)    
    l<-c() # list of T/F indicating if any cells > 0.0 
    for(j in 1:11){ # loop over all batches for this fire
        if(file.exists(paste0(get(paste0("f",j)),fires[i]))){
            assign( paste0("dust",j), (brick(paste0(get(paste0("f",j)),fires[i]))) )
            # extract each raster in brick and check if any cells > 0.0            
            for(k in 1:nlayers(get(paste0("dust",j)))){            
                r<-raster(get(paste0("dust",j)), k)
                l<-c(l, any(na.omit(values(r)) > 0.0))            
            }
        }
    }
    #sum up hrs with cells > 0.0 for this fire
    emittingHrs<-sum(l)
    emittingHrsAllFires<-c(emittingHrsAllFires, emittingHrs)
}



