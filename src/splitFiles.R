
for(f in 1:120){
    w<-read.table(paste0('master_list/nam_', f, '.csv'))
    colnames(w)<-('file')
    l<-as.integer(length(w$file)/4)
    #split each list into 4 lists
    for(i in 1:4){
        start<-1 + (i-1)*l
        if(i==4){
            end<-length(w$file)
        }    
        else{
            end<-start - 1 + l
        }
        s<-w$file[start:end]
        #print(paste0("wx_1/nam_1_", i))
        write.table(s, paste0("wx_", i, "/nam_", f, "_", i, ".csv"), row.names=FALSE, col.names=FALSE, quote=FALSE)
        rm(s)
    }
}
     






