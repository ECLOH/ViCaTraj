DONNEES_POUR_PLOT<-function(type=as.character(input$plottype), objseq=seq.select1(), arrondi=2  ){
  tye<-type
  as.data.frame(matrix(data = c(
"d", "seqstatd(objseq)->objdat;round(objdat$Frequencies*100, arrondi)",
"f", "seqtab(objseq)->objdat;round(attributes(objdat)$freq, arrondi)",
"I", "seqtab(objseq)->objdat;round(attributes(objdat)$freq, arrondi)",
"Ht", "seqstatd(objseq)->objdat;objdat$Entropy",
"ms", "seqmodst(objseq)",
"mt", "round(seqmeant(objseq), arrondi)",
"r", "seqrep(objseq)"
), ncol=2, byrow = TRUE), stringsAsFactors = FALSE)->df
  names(df)<-c("TYPE" , "EXPR")
  eval(parse(text =   df[df$TYPE==tye , ]$EXPR ) )->res
  return(res)
}
