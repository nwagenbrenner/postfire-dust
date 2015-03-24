setwd('/home/natalie/postfire_emissions/windspeed_rdata')
load('/mnt/output/postfire_emissions/output/annual_output2save/dustHOMERadded.Rdata')

locations<-system('ls', intern=TRUE)
percentGreaterThan10<-c()

for(i in 1:length(locations)){
    load(locations[i])
    w<-as.data.frame(wind)
    s<-subset(w,subset=(max>10.0))
    p<-length(s$max)/length(w$max) * 100
    percentGreaterThan10<-c(percentGreaterThan10, p)
}
