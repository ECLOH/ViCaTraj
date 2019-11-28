CreaVar_list<-function(data.list=csv.list, 
                       varname="ETATDOSRSA_TEXT", 
                       recodes=list("newcode"=c("oldcode", "oldcole")), 
                       new_varname="ETATDOSRSA_simplifie_1"){
  transfofacto<-function(var, codage) {
    codes <- codage
    res <- as.character(var)
    for(code in names(codes)) {
      res[res %in% codes[[code]]] <- code
    }
    factor(res)
  }
  lapply(1:length(csv.list), function(i){
    csv.list[[i]]->df.i
    print(i)
    names(df.i)->oldnames
    df.i<-cbind(df.i, transfofacto(var = df.i[ , varname], codage = recodes))
    names(df.i)<-c(oldnames, new_varname)
    return(df.i)
  })->resli
  names(resli)<-names(csv.list)
  return(resli)
}
  