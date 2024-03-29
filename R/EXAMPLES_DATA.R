#' @export
EXAMPLES_DATA<-function(){
  library(TraMineR)
  data(biofam)
  names(biofam)[names(biofam)%in%paste("a", 15:30, sep = "")]->namevar
  names(biofam)[!names(biofam)%in%namevar]->compvar
  lapply(namevar, function(ni){
    biofam[ , c(compvar, ni)]->df.namevar
    names(df.namevar)[names(df.namevar)==ni]<-"VARIABLE"
    df.namevar
  })->EXAMPLE_LIST
  names(EXAMPLE_LIST)<-namevar
  #####
  biofam[!duplicated(biofam$idhous)&!is.na(biofam$idhous) , ]->datforseq
  seqdef(data = datforseq[ , namevar], id = datforseq$idhous)->SEQ
  list("Table.unique"=biofam)->COMP
  list("SEQ"=SEQ, "COMP"=COMP)->EXAMPLE.OBJET.SEQ
  ####
  biofam->EXEMPLE.CSV.FILE
  ####
  list("EXAMPLE_LIST"=EXAMPLE_LIST, "EXAMPLE.OBJET.SEQ"=EXAMPLE.OBJET.SEQ, "EXEMPLE.CSV.FILE"=EXEMPLE.CSV.FILE)->res
  return(res)
}
