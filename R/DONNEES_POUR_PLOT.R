DONNEES_POUR_PLOT<-function(TYPE=as.character(input$plottype), objseq=seq.select1(), ...  ){
  if(sum(class(objseq)!=c("stslist", "data.frame"))==0){
    DONNEES_POUR_PLOT.internal(TYPE.r1=TYPE, objseq.r1=objseq, ... )->res
  } else {
    if(class(objseq)=="list"){
      print(length(objseq))
    lapply(1:length(objseq), function(i){
      DONNEES_POUR_PLOT.internal(TYPE.r1=TYPE, objseq.r1=objseq[[i]], ... )->df
      if(is.null(names(objseq))){
        nam<-i
        } else {
          nam<-names(objseq)[i]
        }
      df$MODALITE<-nam
      return(df)
    })->resi
    do.call("rbind", resi)->res
    }
  }
  return(res)
}
  

