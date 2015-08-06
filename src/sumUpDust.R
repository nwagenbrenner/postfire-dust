

#---- iterate over fires (annual)--------------------------------
#setwd('/mnt/output/postfire_emissions/output/annual_output2save_lowerustar/')
setwd('/mnt/output/postfire_emissions/output/annual_output2save_higherustar/')
fires<-system('ls batch_1/dust', intern=TRUE)

dust.master <- data.frame(rbind(rep(NA,5)))
names(dust.master)<-c('name', 'area_m2', 'start_month', 'pm10_kg', 'pm10_kgperm2')

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

for(i in 1:length(fires)){
    firename<-gsub("_dust.tif","", fires[i], fixed=TRUE)
    firename<-checkSpaces(firename)
    fire <- readOGR(dsn=dsn, layer=firename)
    l<-c()
    for(j in 1:11){
        if(file.exists(paste0(get(paste0("f",j)),fires[i]))){
            assign( paste0("dust",j), (brick(paste0(get(paste0("f",j)),fires[i]))) )
            
            #add up all values per pixel
            assign( paste0("dust.sum", j), sum(get(paste0("dust", j))) )
            #make a vector of dust.sumi filenames for access below
            l<-c(l, paste0("dust.sum", j))     
        }
    }

    if(length(l) == 4){    

        dust.sum1<-sum(get(l[1]))
        dust.sum2<-sum(get(l[2]))
        dust.sum3<-sum(get(l[3]))
        dust.sum4<-sum(get(l[4]))

        dust.sum<-dust.sum1+dust.sum2+dust.sum3+dust.sum4
    }
    else if(length(l) == 11){

        dust.sum1<-sum(get(l[1]))
        dust.sum2<-sum(get(l[2]))
        dust.sum3<-sum(get(l[3]))
        dust.sum4<-sum(get(l[4]))
        dust.sum5<-sum(get(l[5]))
        dust.sum6<-sum(get(l[6]))
        dust.sum7<-sum(get(l[7]))
        dust.sum8<-sum(get(l[8]))
        dust.sum9<-sum(get(l[9]))
        dust.sum10<-sum(get(l[10]))
        dust.sum11<-sum(get(l[11]))

        dust.sum<-dust.sum1+dust.sum2+dust.sum3+dust.sum4+
                    dust.sum5+dust.sum6+dust.sum7+dust.sum8+dust.sum9+
                    dust.sum10+dust.sum11
    }
    rm(l)

    dust.sum.hourly<-dust.sum*res(dust.sum)[1]^2*60*60 #mg/m2/s to total mg per pixel
    dust.total<-sum(values(dust.sum.hourly), na.rm=TRUE) #total mg for for fire
    dust.total<-dust.total/1000/1000 # mg to kg (total within burn perimeter)
    dust.kgperm2<-dust.total/fire$Area # kg/m2 (averaged over burned area)

    dust.entry<-as.data.frame(cbind(as.character(fire$Fire_Name), fire$Area, fire$StartMonth, dust.total, dust.kgperm2))    
    names(dust.entry)<-c('name', 'area_m2', 'start_month', 'pm10_kg', 'pm10_kgperm2')
    
    dust.master<-rbind(dust.master, dust.entry)
}

d<-na.omit(dust.master)
d$pm10_kg<-as.numeric(d$pm10_kg)
d$pm10_kgperm2<-as.numeric(d$pm10_kgperm2)
d$area_m2<-as.numeric(d$area_m2)



