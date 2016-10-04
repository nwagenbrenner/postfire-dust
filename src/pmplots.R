library(maptools)
library(rgdal)
library(raster)
library(lubridate)


load('/mnt/output/postfire_emissions/output/annual_output2save/dustHOMER_correctOrder.Rdata')
load('/mnt/output/postfire_emissions/output/annual_output2save/emittingHrs.RData')

fires<-system("ls /mnt/output/postfire_emissions/output/annual_output2save/batch_1/dust/", intern=TRUE)
dsn<-'/media/Elements/postfire_emissions/fires'

for(i in 1:length(fires)){
    firename<-gsub("_dust.tif","", fires[i], fixed=TRUE)
    firename<-checkSpaces(firename)
    fire <- readOGR(dsn=dsn, layer=firename)    
    d$start_day[i]<-fire$StartDay
}


start_time<-ymd_hms(paste0('2012-',d$start_month,'-',d$start_day,' 12:00:00'))
end_time <- ymd_hms("2012-12-31 12:00:00")
time_diff<- end_time - start_time
time_diff<-as.numeric(time_diff*24)
percentTimeEmitting<-emittingHrs/time_diff*100
d<-cbind(d,percentTimeEmitting)

d$start_month<-as.numeric(d$start_month)
area_ha<-d$area_m2/10000
d<-cbind(d,area_ha)
pm10_tg<-d$pm10_kg/1E9
d<-cbind(d,pm10_tg)
d<-cbind(d,emittingHrs)
#percent<-d$percenTimeEmitting #??
#d<-cbind(d,percent)

ramp<-colorRampPalette(c('darkblue', 'gray80', 'red'))

max_val <- max(abs(d$percent))
min_val<- min(abs(d$percent))
values <- seq(min_val, max_val, length = 4)

season<-c()
for(i in 1:length(d$name)){
    if(d$start_month[i] %in% c(12, 1, 2)){
        season<-c(season,"DJF")
    }
    else if(d$start_month[i] %in% c(3,4,5)){
        season<-c(season,"MAM")
    }
    else if(d$start_month[i] %in% c(6,7,8)){
        season<-c(season,"JJA")
    }
    else{
        season<-c(season,"SON")
    }
}

d<-cbind(d,season)


p<-ggplot(d, aes(x=area_ha, y=pm10_tg, size=emittingHrs, color=as.factor(season))) +
    geom_point(shape=19, alpha = 0.7) +
    scale_size_continuous(breaks=c(1000,2000,3000), 
                          labels=c("1000","2000","3000"),
                          name  ="Emitting hours") +
    xlab("Fire Size (ha)") + ylab("Total PM10 (Tg)") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)) +
    theme(axis.title = element_text(size = 16)) +
    #scale_size_continuous(breaks=c(1000,50000,100000,200000), 
    #                      labels=c("1000","50000","100000","200000"),
    #                      name  ="Fire Area\n(ha)") +
    theme(legend.title = element_text(size=12)) +
    theme(legend.text = element_text(size = 12)) +
    scale_colour_manual(values = c("darkblue", "orange", "darkgreen", "grey30"),
                        name  ="Start month") +
    scale_y_log10(limits=c(1e-03, 1e+01),breaks=c(0.001,0.01,0.1,1.0,10),labels=c("0.001","0.01","0.1","1.0","10")) +
    scale_x_log10(limits=c(100,1e+6), breaks=c(100,1000,10000,100000,1000000),
                    labels=c("100","1000", "1000","100000","1000000")) 
    #stat_smooth(method="lm", se=FALSE,color="black")

p<-ggplot(d, aes(x=area_ha, y=pm10_tg, size=percentTimeEmitting, color=as.factor(season))) +
    geom_point(shape=19, alpha = 0.7) +
    scale_size_continuous(breaks=c(25,50,75), 
                          labels=c("25%","50%","75%"),
                          name  ="% Time Emitting") +
    xlab("Fire Size (ha)") + ylab("Total PM10 (Tg)") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)) +
    theme(axis.title = element_text(size = 16)) +
    
    theme(legend.title = element_text(size=12)) +
    theme(legend.text = element_text(size = 12)) +
    scale_colour_manual(values = c("darkblue", "orange", "darkgreen", "grey30"),
                        name  ="Start month") +
    scale_y_log10(limits=c(1e-03, 1e+01),breaks=c(0.001,0.01,0.1,1.0,10),labels=c("0.001","0.01","0.1","1.0","10")) +
    scale_x_log10(limits=c(100,1e+6), breaks=c(100,1000,10000,100000,1000000),
                    labels=c("100","1000", "1000","100000","1000000")) +
    theme(axis.text.x = element_text(vjust=-0.4)) +
    theme(axis.title.x = element_text(vjust=-0.4))
    #stat_smooth(method="lm", se=FALSE,color="black")



#=================================================================================
#              OLD PLOTS WITH ECOREGIONS
#=================================================================================


library(maptools)
library(rgdal)
library(raster)

#load('/mnt/output/postfire_emissions/output/annual_output2save/dust.RData')
load('/mnt/output/postfire_emissions/output/annual_output2save/dustHOMERAdded.RData')
load('/mnt/output/postfire_emissions/output/annual_output2save/emittingHrs.RData')

#--------------------------------------------------------
#  add L3 ecoregion to df
#--------------------------------------------------------
dsn<-'/media/Elements/postfire_emissions/fires'
ogrList<-ogrListLayers(dsn)

fetchL3<-function(firename){
    fire <- readOGR(dsn=dsn, layer=firename)
    e<-as.character(fire$LEVEL3_NAM)
    return(e)
}

ecoregion<-NA
for(i in 1:length(d$name)){
    e<-fetchL3(d$name[i])
    print(e)
    ecoregion<-rbind(ecoregion, e)
}


er<-as.data.frame(na.omit(ecoregion))
colnames(er)<-"ecoregion"
d<-cbind(d,er)


#test
#emittingHrs<-rnorm(length(d$name),180)
#d<-cbind(d,er,emittingHrs)

#--------------------------------------------------------
#  re-ordering
#--------------------------------------------------------
#order by pm10 
dd<-d[with(d, order(-pm10_kg)), ]

d$start_month<-as.numeric(d$start_month)
dd<-d[with(d, order(start_month)), ]


#--------------------------------------------------------
#  plots
#--------------------------------------------------------
library(ggplot2)

#l<-d$name
#ll<-d$name[1]

#ll<-dd$name[1]
#for(i in seq(1, length(l), by=5)){    
#    ll<-append(ll,l[i])    
#}

area_ha<-d$area_m2/10000
d<-cbind(d,area_ha)

pm10_tg<-d$pm10_kg/1E9
d<-cbind(d,pm10_tg)

d<-cbind(d,emittingHrs)

p<-ggplot(dd, aes(x=name, y=pm10_tg, color=start_month)) +
    geom_bar(stat="identity", position="dodge") +
    xlab("Fire") + ylab("PM10 (Tg)") +
    scale_x_discrete(breaks = ll) + 
    theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)) +
    theme(axis.title = element_text(size = 16)) +
    theme(axis.text.x = element_text(angle = 45))

p<-ggplot(d, aes(x=area_ha, y=pm10_tg, color=factor(ecoregion))) +
    geom_point(shape=19, size=2.5, alpha = 0.7) +
    xlab("Burned Area (ha)") + ylab("Total PM10 (Tg)") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)) +
    theme(axis.title = element_text(size = 16)) +
    #theme(axis.title.x = element_text(vjust=0.1)) +
    #theme(axis.title.y = element_text(vjust=0.1)) +
    annotate("text", x = 1850, y = 4.28, label = "Long Draw", size = 5) +
    annotate("text", x = 1550, y = 3.0, label = "Holloway", size = 5) +
    annotate("text", x = 1450, y = 4.0, label = "Rush", size = 5) +
    scale_colour_manual(values = c("#999999", "#33CC00", "#0099FF"), guide=FALSE) 
    #theme(axis.text.x = element_text(angle = 45))

#log-log plot 
season<-c()
for(i in 1:length(d$name)){
    if(d$start_month[i] %in% c(12, 1, 2)){
        season<-c(season,"DJF")
    }
    else if(d$start_month[i] %in% c(3,4,5)){
        season<-c(season,"MAM")
    }
    else if(d$start_month[i] %in% c(6,7,8)){
        season<-c(season,"JJA")
    }
    else{
        season<-c(season,"SON")
    }
}

d<-cbind(d,season)


p<-ggplot(d, aes(x=area_ha, y=pm10_tg, size=emittingHrs, color=as.factor(season))) +
    geom_point(shape=19, alpha = 0.7) +
    scale_size_continuous(breaks=c(1000,2000,3000), 
                          labels=c("1000","2000","3000"),
                          name  ="Emitting hours") +
    xlab("Fire Size (ha)") + ylab("Total PM10 (Tg)") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)) +
    theme(axis.title = element_text(size = 16)) +
    #scale_size_continuous(breaks=c(1000,50000,100000,200000), 
    #                      labels=c("1000","50000","100000","200000"),
    #                      name  ="Fire Area\n(ha)") +
    theme(legend.title = element_text(size=12)) +
    theme(legend.text = element_text(size = 12)) +
    scale_colour_manual(values = c("darkblue", "orange", "darkgreen", "grey30"),
                        name  ="Start month") +
    scale_y_log10(limits=c(1e-03, 1e+01),breaks=c(0.001,0.01,0.1,1.0,10),labels=c("0.001","0.01","0.1","1.0","10")) +
    scale_x_log10(limits=c(100,1e+6), breaks=c(100,1000,10000,100000,1000000),
                    labels=c("100","1000", "1000","100000","1000000")) 
    #stat_smooth(method="lm", se=FALSE,color="black")

#add in wind speed frequency info
d<-cbind(d,percent)

p<-ggplot(d, aes(x=emittingHrs, y=pm10_tg, size=area_ha, color=percent)) +
    geom_point(shape=19, alpha = 0.7) +
    xlab("Number of hours with emissions") + ylab("Total PM10 (Tg)") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)) +
    theme(axis.title = element_text(size = 16)) +
    annotate("text", x = 1850, y = 4.28, label = "Long Draw", size = 5) +
    annotate("text", x = 1550, y = 3.0, label = "Holloway", size = 5) +
    annotate("text", x = 1450, y = 4.0, label = "Rush", size = 5) +
    scale_color_gradient2(low="red",mid="grey70",high="blue",name="% > 6 m/s") +
    #scale_colour_gradientn(colours = ramp(100),breaks = c(min_val:max_val),name="% > 6 m/s")+
    scale_size_continuous(breaks=c(1000,50000,100000,200000), 
                          labels=c("1000","50000","100000","200000"),
                          name  ="Fire Area\n(ha)") +
    theme(legend.title = element_text(size=12)) +
    theme(legend.text = element_text(size = 12))



