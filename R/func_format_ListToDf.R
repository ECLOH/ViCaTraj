#' Convertir une liste aplatie en data frame
#'
#' Transforme une liste en un data frame comportant une seule ligne.
#' Les noms des éléments de la liste sont utilisés comme noms de colonnes.
#' Les noms de colonnes dupliqués sont rendus uniques.
#'
#' Certains groupes de variables liés aux ressources mensuelles sont
#' regroupés dans une seule colonne à l'aide de [tidyr::unite()].
#' La fonction conserve ensuite uniquement les colonnes correspondant aux
#' données administratives, à l'identification des personnes et aux
#' informations relatives au calcul du droit au RSA.
#' @family {database}
#' @param list.for.i Une liste nommée à aplatir. Les éléments doivent pouvoir
#'   être convertis en vecteur par [base::unlist()]. Typiquement le résultat de [ViCaTraj::func_rename_node_personne()]
#'
#' @return Un [data.frame] contenant une seule ligne. Les colonnes sont
#'   sélectionnées et, pour certaines variables de ressources, regroupées
#'   selon les règles définies dans la fonction.
#'
#' @details
#' La fonction réalise les opérations suivantes :
#'
#' \enumerate{
#'   \item aplatit la liste avec [base::unlist()] ;
#'   \item crée un tableau à une ligne ;
#'   \item rend les noms de colonnes uniques ;
#'   \item regroupe certaines variables `NATRESS` avec [tidyr::unite()] ;
#'   \item conserve un sous-ensemble de colonnes à l'aide de [dplyr::select()].
#' }
#'
#' Les colonnes regroupées sont séparées par la chaîne `" _ "`.
#'
#' @examples
#' exemple <- list(
#'   DonneesAdministratives = list(
#'     Identifiant = "123"
#'   ),
#'   Personne_DEM = list(
#'     Identification = list(
#'       Nom = "Dupont"
#'     )
#'   )
#' )
#'
#' resultat <- func_format_ListToDf(exemple)
#' resultat
#'
#' @seealso
#' [ViCaTraj::func_rename_node_personne()],
#' [base::unlist()], [base::as.data.frame()], [tidyr::unite()],
#' [dplyr::select()], [dplyr::starts_with()]
#'
#' @importFrom tidyr unite
#' @importFrom dplyr select starts_with any_of
#'
#' @export
func_format_ListToDf <- function(list.for.i) {
  flat_list <- unlist(list.for.i)
  vector.names.i <- names(flat_list)
  la.mat <- matrix(flat_list, nrow = 1)
  la.df_c <- as.data.frame(la.mat, stringsAsFactors = FALSE)
  names(la.df_c) <- vector.names.i
  names(la.df_c) <- make.unique(names(la.df_c), sep = "_")
  if (
    sum(grepl(
      "Personne_DEM.Ressources.RessourcesMensuelles.DetailRessourcesMensuelles.NATRESS",
      names(la.df_c)
    )) >
      0
  ) {
    la.df_c <- la.df_c |>
      tidyr::unite(
        col = "Personne_DEM.Ressources.NATRESS",
        starts_with(
          "Personne_DEM.Ressources.RessourcesMensuelles.DetailRessourcesMensuelles.NATRESS"
        ),
        na.rm = TRUE,
        remove = TRUE,
        sep = " _ "
      )
  }

  if (
    sum(grepl(
      "Personne_CJT.Ressources.RessourcesMensuelles.DetailRessourcesMensuelles.NATRESS",
      names(la.df_c)
    )) >
      0
  ) {
    la.df_c <- la.df_c |>
      tidyr::unite(
        col = "Personne_CJT.Ressources.NATRESS",
        dplyr::starts_with(
          "Personne_CJT.Ressources.RessourcesMensuelles.DetailRessourcesMensuelles.NATRESS"
        ),
        na.rm = TRUE,
        remove = TRUE,
        sep = " _ "
      )
  }
  la.df_c <- la.df_c |>
    dplyr::select(c(
      dplyr::starts_with("DonneesAdministratives."),
      dplyr::starts_with("IdentificationRSA."),
      dplyr::starts_with(
        'PrestationRSA.DetailDroitRSA.TroncCommunDroitRSA'
      ),
      dplyr::starts_with("Personne_DEM.Identification"),
      dplyr::starts_with("Personne_CJT.Identification"),
      dplyr::starts_with(
        "PrestationRSA.DetailDroitRSA.DetailCalculDroitRSA"
      ),
      dplyr::starts_with(
        "PrestationRSA.DetailDroitRSA.MontantsCalculDroitRSA"
      ),
      dplyr::starts_with(
        "PrestationRSA.DetailDroitRSA.TroncCommunDroitRSA"
      ),
      dplyr::starts_with("PrestationRSA.SituationDossierRSA."),
      any_of(c(
        "Personne_DEM.Ressources.NATRESS",
        "Personne_CJT.Ressources.NATRESS",
        "Personne_DEM.MontantCalculDroitRSAPersonne.TOPPERSDRODEVORSA",
        "Personne_CJT.MontantCalculDroitRSAPersonne.TOPPERSDRODEVORSA",
        "Personne_CJT.Activite.NATCONTRTRA",
        "Personne_DEM.Activite.NATCONTRTRA",
        "Personne_DEM.Ressources.GeneraliteRessourcesTrimestre.TOPRESSNUL",
        "Personne_CJT.Ressources.GeneraliteRessourcesTrimestre.TOPRESSNUL"
      ))
    ))
  return(la.df_c)
}
