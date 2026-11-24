#' @author Elie..
#' @title La fonction "NVX_scale"
#' @description  lent : utilise zoo::rollapply(). Permet de dire (T/F) si à la ligne l'individu est entrant. (format "long").
#' @param x df au format long
#' @param scale 12 Nb de mois précédents (i.e. lignes précédentes) != state pour être considéré comme nouvel entrant. 
#' @param state "RSA" : état qui décrit le fait d'être dans le dispo  
#' @return le df x
#' @export
NVX_scale<-function(x, scale = 12, state = "RSA"){
  # SEE dtpyr
  test<-zoo::rollapply(x, 
                 width = list(c(-scale:-1)), 
                 FUN = function(x){sum(x== state)<1}, fill = NA)
  res <- (test & x==state)
  return(res)
}
#' @author Elie..
#' @title La fonction "SOR_scale"
#' @description  voir NVS_scale
#' @return le df x
#' @export
SOR_scale<-function(x, scale = 12, state = "RSA"){
  # SEE dtpyr
  test<-zoo::rollapply(x, 
                       width = list(c(1:scale)), 
                       FUN = function(x){sum(x==state)<1}, fill = NA)
  res <- (test & x==state)
  return(res)
}
#' @author Elie..
#' @title La fonction "ctrl_window"
#' @return vecteur booléen
#' @export
ctrl_window<-function(var, window = 1:6, state="RSA", Fill=NA){
  if(length(window)==1){
    res <- data.table::shift(x = {{var}}, 
                      n = window)
    res <- if_else(is.na(res)|res!=state, FALSE, TRUE)
  } else {
  message("ctrl_window: arrange() a été appelée ?")
    res <- data.table::shift(x = {{var}}, 
                      n = window) |> 
    lapply(function(x){x==state}) |> 
    bind_cols() |> 
    mutate(TEST = rowSums(across(everything()), 
                          na.rm=TRUE )==max(window)) |> 
    pull(TEST)
  }
  return(res)
}


#' Calcule un indicateur d'état à partir de colonnes temporelles
#'
#' Pour chaque ligne, compte le nombre de colonnes temporelles dont la valeur
#' est égale à \code{state}. Avec \code{type = "test"}, renvoie plutôt un
#' indicateur logique indiquant si toutes les colonnes temporelles sont égales
#' à \code{state}.
#'
#' Si \code{cols.time} est numérique et contient une valeur inférieure ou égale
#' à 1, ou supérieure au nombre de colonnes de \code{data.wide}, la colonne
#' calculée est remplie de valeurs manquantes.
#'
#' @param data.wide Un tableau de données au format large.
#' @param cols.time Colonnes contenant les mesures temporelles. Peut être une
#'   expression de sélection tidyselect ou un vecteur d'indices numériques.
#' @param col.id Colonne identifiant les lignes, à conserver dans le résultat.
#' @param col.new Nom de la colonne à créer pour stocker le résultat.
#' @param state Valeur recherchée dans les colonnes temporelles. Par défaut,
#'   \code{"RSA"}.
#' @param type Type de résultat. Avec \code{"test"}, renvoie \code{TRUE} si
#'   toutes les colonnes sélectionnées correspondent à \code{state}, et
#'   \code{FALSE} sinon. Toute autre valeur renvoie le nombre de
#'   correspondances.
#'
#' @return Un tableau contenant la colonne identifiante \code{col.id} et la
#'   colonne calculée \code{col.new}.
#'
#' @importFrom dplyr across mutate select
#' @importFrom dplyr %>%
#' @export
anx_wide <- function(data.wide, cols.time, col.id, col.new, state = "RSA", type = "test"){
  #print(length(cols.time))
  if(is.numeric(cols.time) & ( (sum(cols.time<=1)>=1) | (sum(cols.time>ncol(data.wide))>=1)) ){
    res<- data.wide |> 
      mutate("{{col.new}}" :=  NA) |> 
      select({{ col.id }}, {{ col.new }} )
  } else {
  res <- data.wide |> 
    # "{{col_new}}" := {{ col_var }} / sum({{ col_var }}) * 100
    mutate("{{col.new}}" := rowSums(across({{ cols.time }}, ~.x==state), na.rm=T)) |> 
    select({{ col.id }}, {{ col.new }} )
  if(type == "test"){
    res <- res |> 
    mutate(across({{col.new}}, ~ .x == length(cols.time)))
  }
  }
  return(res)
}



#' @author Elie..
#' @title La fonction "test_anx"
#' @export
test_anx <- function(var, state="RSA"){
  var<-c(NA, var)
  res.0<-sapply(length((var)):1, function(i){
    vec<- (var[i:1]!="RSA") | is.na(var)[i:1]
    if(!is.na(var[i]) & var[i]==state){
    res<-min(which(vec), na.rm=TRUE) -1
    if(is.infinite(res)){
      res<-0
    }
    } else {res<-NA}
    if(is.infinite(res)){
      res<-0
    }
    return((res))
  })
  
  res.0<-rev(res.0)[-1]
  return(res.0)
}
  
  
  
#   if(length(window)==1){
#     res <- data.table::shift(x = {{var}}, 
#                              n = window)
#     res <- if_else(is.na(res)|res!=state, FALSE, TRUE)
#   } else {
#     message("ctrl_window: arrange() a été appelée ?")
#     res <- data.table::shift(x = {{var}}, 
#                              n = window) |> 
#       lapply(function(x){x==state}) |> 
#       bind_cols() |> 
#       mutate(TEST = rowSums(across(everything()), 
#                             na.rm=TRUE )==max(window)) |> 
#       pull(TEST)
#   }
#   return(res)
# }
