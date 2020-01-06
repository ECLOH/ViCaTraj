#' @export

DONNEES_POUR_PLOT<-function(TYPE=as.character(input$plottype), objseq=seq.select1(), 
                            col.selected = NULL, pmin.sup=0.05, STR.SUBS=NULL,  grup.var, ...  ){
  if(TYPE=="flux"&length(col.selected)==1){
    stop("ATTENTION : veuillez sélectionner au moins deux moments pour le calcul du graphique de flux")
  }
  if(sum(class(objseq)!=c("stslist", "data.frame"))==0){
    DONNEES_POUR_PLOT.internal(TYPE.r1=TYPE, objseq.r1=objseq, col.selected.r1=col.selected, pmin.sup1 = pmin.sup, STR.SUBS.1 = STR.SUBS, ... )->res
  } else {
    if(class(objseq)=="list"){
      print(length(objseq))
    lapply(1:length(objseq), function(i){
      DONNEES_POUR_PLOT.internal(TYPE.r1=TYPE, objseq.r1=objseq[[i]], col.selected.r1=col.selected, pmin.sup1 = pmin.sup, STR.SUBS.1 = STR.SUBS,... )->df
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
  

