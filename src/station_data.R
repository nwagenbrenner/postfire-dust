station<-read.table('observed_pm/AQS_comparisons/KJER.csv', sep=",",skip=8) 
colnames(station)<-c('ID','datetime','temp','speed','direction','gust')

station[,"datetime"] <- as.character(station[,"datetime"])
station[,"datetime"] <- as.POSIXct(strptime(station[,"datetime"], '%M/%d/%Y %H:%M'))
#station<-subset(station, subset=(as.POSIXlt(datetime)$mon+1==10))

#station<-subset(station, subset=(datetime > "2012-10-02" & datetime < "2012-10-05"))

p<-ggplot(station, aes(x=datetime, y=direction)) +
    geom_point(shape=19, size=1.5, alpha = 1) +
    geom_line() +
    xlab("Time") + ylab("Direction") +
    theme_bw() +
    theme(axis.text = element_text(size = 16)) +
    ggtitle("KJER")


pm<-read.table('observed_pm/AQS_comparisons/notacity_hourlypm10.csv', sep=",",skip=8) 
colnames(pm)<-c('datetime','pm10')

pm[,"datetime"] <- as.character(pm[,"datetime"])
pm[,"datetime"] <- as.POSIXct(strptime(pm[,"datetime"], '%M/%d/%Y %H:%M'))
#station<-subset(station, subset=(as.POSIXlt(datetime)$mon+1==10))

#station<-subset(station, subset=(datetime > "2012-10-02" & datetime < "2012-10-05"))

p<-ggplot(pm, aes(x=datetime, y=pm10)) +
    geom_point(shape=19, size=1.5, alpha = 1) +
    geom_line() +
    xlab("Time") + ylab("Observed PM (ug/m3)") +
    theme_bw() +
    theme(axis.text = element_text(size = 16)) +
    ggtitle("PM10 at Ballard Road")

