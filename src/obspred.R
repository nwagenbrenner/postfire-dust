library(scales)

csv<-'/home/natalie/postfire_emissions/obsvspred.csv'
pm<-read.table(csv,header=TRUE,sep=",")

pm[,"datetime"] <- as.POSIXct(strptime(pm[,"datetime"], '%m/%d/%Y %H:%M'))

#--------------------------------------------------------
#  obs vs pred
#--------------------------------------------------------
#reorder factors
levels(pm$location)[levels(pm$location)=="boise"] <- "Boise"
levels(pm$location)[levels(pm$location)=="nampa"] <- "Nampa"
levels(pm$type)[levels(pm$type)=="mod"] <- "Modeled"
levels(pm$type)[levels(pm$type)=="obs"] <- "Observed"
pm$typeOrdered <- factor(pm$type, levels=c("Observed", "Modeled"))

#Create a custom color scale
myColors <- c("#0099FF","grey50")
names(myColors) <- levels(pm$location)

p<-ggplot(pm, aes(x=datetime, y=pm10, linetype=typeOrdered, shape=typeOrdered)) +
    xlab("Time") + ylab(expression(paste("PM"[10],"  (\u03BCg/","m"^{3},")"))) +  
    geom_point(size=3) +
    geom_line(guide=FALSE) +
    theme_bw() + 
    theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)) +
    theme(axis.title = element_text(size = 16)) +
    theme(axis.text.x = element_text(angle = 45)) +
    theme(axis.text.x = element_text(vjust=0.6)) +
    theme(axis.title.x = element_text(vjust=-0.4)) +
    scale_x_datetime(breaks = date_breaks("3 hour"),minor_breaks=date_breaks("1 hour"),labels=date_format("%H:00")) +
    theme(legend.title=element_blank()) +
    theme(legend.text=element_text(size=12)) +
    facet_grid(location ~ .) +
    theme(strip.text.y = element_text(size = 12, colour = "black"))

#-------------------------------------------------------------
# accumulated surface loadings
#-------------------------------------------------------------

boise_mod<-subset(pm, subset=(type=='Modeled' & location=='Boise'))
boise_obs<-subset(pm, subset=(type=='Observed' & location=='Boise')) 

sum(na.omit(boise_mod$pm10))/sum(boise_obs$pm10) #0.68

nampa_mod<-subset(pm, subset=(type=='Modeled' & location=='Nampa'))
nampa_obs<-subset(pm, subset=(type=='Observed' & location=='Nampa')) 

sum(na.omit(nampa_mod$pm10))/sum(na.omit(nampa_obs$pm10)) #1.53

#accumulated surface loadings
boise_modeled = 8928
boise_observed = 13155.5
(boise_observed - boise_modeled) / boise_observed = 0.321

nampa_modeled = 15888
nampa_observed = 10368
(nampa_modeled - nampa_observed) / nampa_observed = 0.53

#read in cmaq-modeled concentrations without dust
csv<-'/home/natalie/postfire_emissions/observed_pm/PM10_using_ap4_nohaboob_boise.txt'
boise_nodust<-read.table(csv,header=TRUE,sep=",")
colnames(boise_nodust)<-c("datetime", "pm10")

csv<-'/home/natalie/postfire_emissions/observed_pm/PM10_using_ap4_nohaboob_nampa.txt'
nampa_nodust<-read.table(csv,header=TRUE,sep=",")
colnames(nampa_nodust)<-c("datetime", "pm10")

sum(nampa_nodust$pm10) #109.4
sum(boise_nodust$pm10) #140.5

#boise: 8928/140.5 = 64
#nampa: 15888/109.4 = 145













