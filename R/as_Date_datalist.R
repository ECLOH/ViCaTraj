#' @title   func_asDate_datalist_fast
#' @description  "2021-12" -> Date format (01-12-2021)  
#' @family {database}
#' @param vec vecteur des valeurs de date dans datalist
#' @export
func_asDate_datalist_fast <- function(vec) { 
  # Ajoute "-01" à chaque élément pour obtenir "YYYY-MM-01" 
  vec <- paste0(vec, "-01") 
  # Conversion directe en Date (format ISO reconnu par as.Date) 
  as.Date(vec) 
} 


#' @title   as.Date_datalist
#' @description  "2021-12" -> Date format (01-12-2021)  
#' @param vec vecteur des valeurs de date dans datalist
#' @export
as.Date_datalist<-function(vec){
  vec<-as.character(vec)
  res<-strsplit(x = vec, split = "-")
  res<-sapply(res, function(i){
   paste0("01-", i[2], "-", i[1]) 
  })
  res<-floor_date(as.Date(res, format="%d-%m-%Y"), unit = "month")
  return(res)
}
