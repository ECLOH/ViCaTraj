#' Fonction col_flux  
#' @description  Cette fonction permet d'avoir une légende universelle pour les graphiques de flux
#' @param data 
#' @param seq.data 
#'
#' @return un vecteur avec les différentes valeurs des couleurs et dont le nom des lignes est un level 
#' @export
col_flux<-function(data, seq.data, palette="Paired"){
  # MESSAGE DE ELIE: je me permet de modifier la fonction, car cette fonction se base sur les données d'origine, 
  # et ne permet pas de prendre en compte les eventuels recodages des gaps, left et right missing values. 
  
  #colnames(seq.data)->col_nom
  #which(colnames(data) %in% col_nom)->col_contrat
  #vect_levels<-NULL
  #for (i in col_contrat){
  #  rbind(vect_levels,cbind(levels(as.factor(data[,i]) )))->vect_levels
  #}
  #nom<-unique(vect_levels)
  alphabet(seq.data)->alfabet
  df<-brewer.pal(length(alfabet), palette)
  names(df)<-alfabet
  #df<-brewer.pal(length(nom), "Paired")
  #names(df)<-nom
  return(df)
}
