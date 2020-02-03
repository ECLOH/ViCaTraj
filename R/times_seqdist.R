#' @export

times.dist<-function(amplit=seq(from = 0, to=5000, by=200)){#}, nb.sequences=10000){
  if(amplit[1]==0){amplit[1]<-1}
lapply(X =amplit , FUN = function(i){
  if(i==0){i<-1}
  Sys.time()->t1
  seqdist(trajs[1:i , ], method = "OM", sm = "TRATE")->temp
  Sys.time()->t2
  as.numeric(difftime(time1 = t2, time2 = t1, units = "secs")*1000)
})->times.dist
names(times.dist)<-amplit
data.frame(do.call("rbind", times.dist))->df
df$amplit<-as.numeric(as.character(row.names(df)))
names(df)<-c("time", "amplit")
return(df)
}