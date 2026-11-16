#' Renomme et nettoie les nœuds relatifs aux personnes et aux adresses
#'
#' Cette fonction traite une liste contenant notamment des nœuds
#' \code{Personne}, \code{Prestations} et \code{Adresse}.
#' Elle effectue les opérations suivantes :
#' \itemize{
#'   \item{renommage des nœuds \code{Personne} avec un indice temporaire ;}
#'   \item{renommage des nœuds \code{Prestations} selon la nature de la prestation ;}
#'   \item{suppression des suffixes numériques temporaires ;}
#'   \item{renommage des personnes selon leur rôle ;}
#'   \item{suppression des adresses dont le rang est différent de \code{"01"} ;}
#'   \item{nettoyage des noms des nœuds d'adresses.}
#' }
#'
#' Les personnes ayant le rôle \code{"DEM"} ou \code{"CJT"} sont renommées
#' sans indice supplémentaire. Les autres personnes reçoivent un indice
#' permettant de distinguer plusieurs personnes ayant le même rôle.
#' @family {database}
#' @param list.for.i Liste R contenant les données à traiter. Elle doit
#'   notamment contenir les éléments \code{DonneesAdministratives} et les
#'   nœuds relatifs aux personnes.
#'
#' @return Une liste R de même structure générale que \code{list.for.i},
#'   contenant les noms de nœuds renommés et les adresses non retenues
#'   supprimées.
#'
#' @details
#' Les nœuds nommés \code{Personne} sont temporairement renommés sous la forme
#' \code{Personne_i}. Les nœuds \code{Prestations} sont ensuite renommés sous
#' la forme \code{Presta_NATPREST}, où \code{NATPREST} correspond à la nature
#' de la prestation.
#'
#' Dans le bloc \code{DonneesAdministratives}, seules les adresses dont
#' \code{TroncCommunAdresse$RGADR} vaut \code{"01"} sont conservées.
#'
#' @examples
#' \dontrun{
#' resultat <- func_rename_node_personne(list.for.i)
#' }
#'
#' @export
func_rename_node_personne <- function(list.for.i) {
  # GERER PERSONNES
  noms <- names(list.for.i)
  positions <- which(noms == "Personne")
  noms[positions] <- paste0("Personne_", positions)
  names(list.for.i) <- noms
  ## FOR LOOP : Parcours de tous les éléments de list.for.i dont le nom commence par "Personne_"
  for (pos in positions) {
    ### 1. Ajout du suffixe _i à "Prestations"
    noms_R2_presta <- names(list.for.i[[pos]])
    positions_R2_presta <- which(noms_R2_presta == "Prestations")
    ### 2. Remplacement "Prestations" par "Presta_<NATPREST>"
    for (i in positions_R2_presta) {
      presta <- list.for.i[[pos]][[i]]$NATPREST[[1]]
      # rolepers <- list.for.i[[pers]][[i]]$ROLEPERS[[1]] # Si besoin
      noms_R2_presta[[i]] <- paste0("Presta_", presta)
    }
    names(list.for.i[[pos]]) <- noms_R2_presta
  }
  ## FOR LOOP : changement de "Personne_" par le rôle de la personne
  mi <- 0
  for (i in positions) {
    rolepers <- list.for.i[[i]]$Presta_RSA$ROLEPERS[[1]]
    if (rolepers != "DEM" & rolepers != "CJT") {
      mi <- mi + 1
      newname <- paste0("Personne_", rolepers, "_", mi)
    } else {
      newname <- paste0("Personne_", rolepers)
    }
    names(list.for.i)[[i]] <- newname
  }
  # GERER ADRESSES
  noms_admin <- names(list.for.i$DonneesAdministratives)
  position_adresse <- which(noms_admin == "Adresse")
  for (i in position_adresse) {
    rang_addr <- list.for.i$DonneesAdministratives[[
      i
    ]]$TroncCommunAdresse$RGADR[[1]]
    if (is.null(rang_addr) | (!is.null(rang_addr) & rang_addr != "01")) {
      list.for.i$DonneesAdministratives[[i]] <- list()
    }
  }
  list.for.i$DonneesAdministratives <- list.for.i$DonneesAdministratives[
    lengths(list.for.i$DonneesAdministratives) > 0
  ]

  return(list.for.i)
}
