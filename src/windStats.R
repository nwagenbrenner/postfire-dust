setwd('/home/natalie/postfire_emissions/windspeed_rdata')
load('/mnt/output/postfire_emissions/output/annual_output2save/dustHOMERadded.Rdata')

locations<-system('ls', intern=TRUE)
percent<-c()

for(i in 1:length(locations)){
    load(locations[i])
    w<-as.data.frame(wind)
    s<-subset(w,subset=(max>10.0))
    p<-length(s$max)/length(w$max) * 100
    percent<-c(percent, p)
}

#-----------------------------
#  compute resolution stats
#-----------------------------
setwd('/mnt/output/postfire_emissions/output/annual_output2save/batch_1/dust')
#get list of fire names
fires<-system('ls', intern=TRUE)

res<-c()
for(i in 1:length(fires)){ 
    assign( "b", (brick(fires[i])) )
    print(paste0("reading ", fires[i]))
    res<-c(res,res(b)[1])
}

