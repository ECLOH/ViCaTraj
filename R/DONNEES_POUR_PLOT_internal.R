DONNEES_POUR_PLOT.internal<-function(TYPE.r1=TYPE, objseq.r1=objseq, arrondi=2){
  
  tye<-TYPE.r1
  
  as.data.frame(matrix(data = c(
    "d", "seqstatd(objseq.r1)->objdat;round(objdat$Frequencies*100, arrondi)",
    "f", "seqtab(objseq.r1)->objdat;round(attributes(objdat)$freq, arrondi)",
    "I", "seqtab(objseq.r1)->objdat;round(attributes(objdat)$freq, arrondi)",
    "Ht", "seqstatd(objseq.r1)->objdat;objdat$Entropy",
    "ms", "seqmodst(objseq.r1)",
    "mt", "round(seqmeant(objseq.r1), arrondi)",
    "r", "seqrep(objseq.r1)"
  ), ncol=2, byrow = TRUE), stringsAsFactors = FALSE)->df
  
  names(df)<-c("TYPE" , "EXPR")
  
  eval(parse(text =   df[df$TYPE==tye , ]$EXPR ) )->res
  
  return(res)
}