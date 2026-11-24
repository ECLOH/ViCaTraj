#' @author Elie.
#' @title La fonction "func_asDate"
#' @description  l'objectif est de détecter des formats de date et de convertir la chaine de caractêre en format Date. Adapté pour les donées individuelles PDIE
#' @param vec vecteur de date (peut s'utiliser dans un mutate
#' @return un vecteur de date
#' @export
func_asDate <- function(vec) {
  erco<-c()
  vecres <- sapply(vec, USE.NAMES = T, FUN = function(x) {
    if (
       nchar(x) == 10 &
        grepl(pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}", x, perl = F)
    ) {
      resx <- as.Date(x)
      #print("1-->")
      #print(resx)

    } else {
      if (
        nchar(x) == 10 &
          grepl(pattern = "[0-9]{2}/[0-9]{2}/[0-9]{4}", x)
      ) {
        resx <- as.Date(x, format = "%d/%m/%Y")

        #print("2-->")
        #print(resx)
      } else {
        if(!grepl(pattern = "[A-z]", x) & grepl(pattern = "[0-9]", x)){
          resx <- as.Date(as.numeric(x))
          #print("3-->")
          #print(resx)
        } else {
          erco <- c(erco, 1)
          resx <- NA_Date_
        }
      }
    }
    return(resx)
  })
  vecres <- as.Date(vecres)
  #print(vecres)
  vecres_df <- data.frame(row.names = 1:length(vecres),
    origin = names(vecres),
    value = vecres
  )
  if(
    nrow(vecres_df |> filter(is.na(value)&!is.na(origin)))
  ){
    message("Certains valeurs non reconnues")
    print(vecres_df |> filter(is.na(value)&!is.na(origin)) |> slice_sample(n = 10))
  }
  names(vecres) <- NULL
  return(vecres)
}