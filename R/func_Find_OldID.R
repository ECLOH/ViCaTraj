#' Identifier les personnes dont la dernière date observée est ancienne
#'
#' Calcule, pour chaque identifiant, la dernière date observée dans les données,
#' puis conserve les identifiants dont cette date est antérieure à une date
#' seuil. La date seuil est obtenue en retranchant `nb_keep` années à la date
#' du jour.
#'
#' Cette fonction ne supprime aucune ligne des données d'origine. Le jeu fourni
#' doit être préalablement filtré pour ne contenir que les observations à
#' prendre en compte dans la recherche.
#'
#' Pour un data.frame, le résultat est calculé en mémoire et renvoyé sous forme
#' de tableau R. Pour un objet Arrow pris en charge, les opérations restent
#' différées et le résultat n'est pas collecté en mémoire.
#'
#' La colonne `DATE` doit contenir des valeurs compatibles avec la conversion
#' effectuée par `as.Date(paste0(DATE, "-01"))`, par exemple des chaînes au
#' format `"AAAA-MM"`.
#'
#' @param data Un data.frame ou un objet Arrow pris en charge, contenant au
#'   minimum les colonnes `Personne.Identification.NIR` et `DATE`.
#' @param nb_keep Nombre d'années retranchées à la date du jour pour calculer la
#'   date seuil. Par défaut, `6`.
#'
#' @return Un tableau agrégé contenant `Personne.Identification.NIR` et
#'   `derniere_date`, filtré pour ne conserver que les identifiants dont la
#'   dernière date est antérieure à la date seuil. Le résultat reste différé
#'   lorsque `data` est un objet Arrow.
#'
#' @family dataclean
#' @author Elie
#' @export

func_Find_OldID <- function(data = ds, 
                            nb_keep  = 6, 
                            ID, 
                            date){
  require(arrow)
  require(dplyr)

date_limite <- as.Date(Sys.Date()) - lubridate::years(nb_keep)
print(date_limite)
if(!inherits(data |> pull( {{date}} ), "Date")){
  stop("{{date}} doit être de type 'Date'")
}

if (is.data.frame(data)) {
  message("data est un data.frame => renvoie un data.frame")
} else {
  if (
    inherits(data, "Dataset") ||
      inherits(data, "arrow_dplyr_query")
  ) {
    message(
      "data est un objet Arrow => renvoie une requête Arrow non collectée"
    )
  } else {
    if (
      inherits(data, "dtplyr_step_call") ||
        inherits(data, "dtplyr_step")
    ) {
      message(
        "data est un objet `dtplyr_step` => renvoie une requête dtplyr non collectée"
      )
    } else {
      stop("Type de data non pris en charge")
    }
  }
}
  
  data |>
  #mutate(DATE = as.Date(paste0(DATE, "-01"))) |> 
  group_by({{ID}}) |>
  summarise(derniere_date = max({{date}}, na.rm = TRUE)) |>
  ungroup() |> 
  filter(derniere_date < date_limite)
  
}