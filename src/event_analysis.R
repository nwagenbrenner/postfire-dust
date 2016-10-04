library(raster)

#--------------------------------------------------------
#  read dust files
#--------------------------------------------------------
r1<-brick('batch_1/dust/MILLERHOMESTEAD_dust.tif')
r2<-brick('batch_2/dust/MILLERHOMESTEAD_dust.tif')
r3<-brick('batch_3/dust/MILLERHOMESTEAD_dust.tif')
r4<-brick('batch_4/dust/MILLERHOMESTEAD_dust.tif')

#--------------------------------------------------------
#  locate rasters with max emissions
#--------------------------------------------------------
for(i in 1:nlayers(r1)){
    rsub<-subset(r1,i)
    if(i==1){
        s<-as.data.frame(cellStats(rsub, 'max'))
        colnames(s)<-'max'
    }
    else{
        s<-rbind(s, cellStats(rsub, 'max'))
    }
}
r1.max<-s #load each one into a separate dataframe

#add the row as a column
r1.max<-cbind(r1.max,1:nrow(r1.max))
colnames(r1.max)<-c("max","layer")
r1.max.ordered<-r1.max[order(-r1.max$max),] 

r2.max<-cbind(r2.max,1:nrow(r2.max))
colnames(r2.max)<-c("max","layer")
r2.max.ordered<-r2.max[order(-r2.max$max),] 

r3.max<-cbind(r3.max,1:nrow(r3.max))
colnames(r3.max)<-c("max","layer")
r3.max.ordered<-r3.max[order(-r3.max$max),] 

r4.max<-cbind(r4.max,1:nrow(r4.max))
colnames(r4.max)<-c("max","layer")
r4.max.ordered<-r4.max[order(-r4.max$max),] 

#--------------------------------------------------------
#  get the actual time of an event
#--------------------------------------------------------
starttime<-"2012-Aug-03 20:00:00 PDT" 

t<-as.POSIXct(strptime(starttime, "%Y-%b-%d %H:%M:%S"))
nhours<-551
tt<-t + (nhours*60*60) #add nhours to start time of simulation
