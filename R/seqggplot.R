# En cours
seqggplot<-function(TYPE="d", objseq = actcal.seq){
  
  if(sum(class(objseq)!=c("stslist", "data.frame"))==0){
    #p<-
      seqggplot.internal(objseq.r1 = objseq, TYPE.r1=TYPE)
  } else {
    if(class(objseq)=="list"){
     # p<-
     lapply(objseq , function(obji){
        seqggplot.internal(objseq.r1 = obji, TYPE.r1=TYPE)
      })
    } else {
      NULL
    }
  }
  #return(p)
}
