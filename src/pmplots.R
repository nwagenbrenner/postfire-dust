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
    annotate("text", x = 195000, y = 4.25, label = "Long Draw", size = 5) +
    annotate("text", x = 166000, y = 3.0, label = "Holloway", size = 5) +
    annotate("text", x = 97000, y = 4.0, label = "Rush", size = 5) +
    scale_colour_manual(values = c("#999999", "#33CC00", "#0099FF"), guide=FALSE) 
    #theme(axis.text.x = element_text(angle = 45))


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



