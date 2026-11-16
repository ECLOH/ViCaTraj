#' Recoder les états de dossier RSA
#'
#' Convertit les codes d'état d'un dossier RSA en catégories textuelles :
#' les codes `"1"`, `"5"` et `"6"` deviennent `"NO.RSA"`, les codes `"3"` et
#' `"4"` deviennent `"SUSPENDU"`, et le code `"2"` devient `"RSA"`.
#' Les valeurs manquantes et les codes non reconnus donnent `NA`.
#'
#' @family {database}
#' @param x Vecteur contenant les codes d'état du dossier RSA. Les valeurs sont
#'   converties en caractères avant leur recodage.
#'
#' @return Un vecteur de caractères de même longueur que `x`, composé des
#'   catégories `"NO.RSA"`, `"SUSPENDU"`, `"RSA"` ou de valeurs manquantes.
#'
#' @examples
#' \dontrun{
#' seq_long |>
#'   dplyr::mutate(
#'     RSA_simple2 = `create_RSA (Revenu de solidarité active)_simple`(
#'       `PrestationRSA (Revenu de solidarité active).SituationDossierRSA (Revenu de solidarité active).EtatDossierRSA (Revenu de solidarité active).ETATDOSRSA (Revenu de solidarité active)`
#'     )
#'   ) |>
#'   dplyr::group_by(RSA_simple2, RSA_simple) |>
#'   dplyr::summarise(N = dplyr::n(), .groups = "drop")
#' }
#'
#' @export

create_RSA_simple<-function(x){
  x <- as.character(x)
  x <- case_when(
               is.na(x) ~ NA_character_,
               x %in% c("5", "6", "1") ~ "NO.RSA",
               x %in% c("3", "4") ~ "SUSPENDU", 
               x =="2" ~ "RSA",
               TRUE ~ NA_character_
             )
  return(x)
}
