#' @export

DONNEES_POUR_PLOT<-function(TYPE=as.character(input$plottype), objseq=seq.select1(), 
                            col.selected = NULL, pmin.sup=0.05, STR.SUBS=NULL,  grup.var, max.k=2,
                            PAS.temps=NULL, TIME.varying=FALSE, Pourc.eff=FALSE, Sens=FALSE , ...  ){
  if(TYPE=="flux"&length(col.selected)==1){
    stop("ATTENTION : veuillez sélectionner au moins deux moments pour le calcul du graphique de flux")
  }
  if(sum(class(objseq)!=c("stslist", "data.frame"))==0){
    DONNEES_POUR_PLOT.internal(TYPE.r1=TYPE, objseq.r1=objseq, col.selected.r1=col.selected, pmin.sup1 = pmin.sup, STR.SUBS.1 = STR.SUBS, max.k1=max.k,
                               PAS.temps1=PAS.temps, TIME.varying1=TIME.varying, Pourc.eff1=Pourc.eff, Sens1=Sens, ... )->res
  } else {
    if(class(objseq)=="list"){
      print(length(objseq))
    lapply(1:length(objseq), function(i){
      DONNEES_POUR_PLOT.internal(TYPE.r1=TYPE, objseq.r1=objseq[[i]], col.selected.r1=col.selected, pmin.sup1 = pmin.sup, STR.SUBS.1 = STR.SUBS, max.k1=max.k, PAS.temps1=PAS.temps, TIME.varying1=TIME.varying, Pourc.eff1=Pourc.eff, Sens1=Sens, ... )->df
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
  

