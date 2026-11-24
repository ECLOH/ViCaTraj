
#' @author Elie.
#' @title La fonction "func_clean_char"
#' @description  l'objectif est de remplacer les NA inclus dans les variables character et factor par un character ("données non dispo")
#' @param data un objet data.table
#' @return un data.table
#' @export
func_clean_char <- function(data){
  cols_to_convert <- names(data)[sapply(data, is.factor)] 
  data[, (cols_to_convert) := lapply(.SD, as.character), .SDcols = cols_to_convert] 
  cols_to_replace <- names(data)[sapply(data, is.character)] 
  data[, (cols_to_replace) := lapply(.SD, function(x) { 
  x[is.na(x)] <- "donnée non dispo" 
  return(x) 
}), .SDcols = cols_to_replace] 
return(data)
}