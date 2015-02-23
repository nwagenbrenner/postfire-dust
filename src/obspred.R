
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



p<-ggplot(pm, aes(x=datetime, y=pm10, color=location, linetype=typeOrdered, shape=location)) +
    xlab("Date") + ylab("PM10 (ug/m3)") +  
    geom_point() +
    geom_line(guide=FALSE) +
    theme_bw() + 
    theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)) +
    theme(axis.title = element_text(size = 16)) +
    theme(axis.text.x = element_text(angle = 45)) +
    theme(axis.text.x = element_text(vjust=0.6)) +
    theme(axis.title.x = element_text(vjust=-0.4)) +
    scale_x_datetime(breaks = date_breaks("3 hour"),minor_breaks=date_breaks("1 hour"),labels=date_format("%H:00"))


