#' @author Elie.
#' @title La fonction "XML_TO_PARQUET"
#' @description  l'objectif de cette fonction est d'ajouter à la base de données parquet les nouvelles données RSA.
#' ATTENTION : cette fonction ne supprime pas les ID perdus de vue epuis + 6 ans.
#' ATTENTION : la BDD est groupée avec DATE et INSTITUTION
#' @family {database}
#' @param chemin.xml chemin du xml
#' @param Parquet_path chemin vers la BDD parquet
#' @param name.vec vecteur des noms de variable à conserver
#' @param date.chr string de date (au format : "2023-12")
#' @return rien: la fonction charge le .xml, converti en data.frame et ajoute la data.frame à la BD parquet.
#' @export
func_XML_TO_PARQUET <- function(
  chemin.xml = "",
  Parquet_path = "BDD/datalist",
  name.vec = bdd_names$value,
  date.chr = empl,
  existing_data_behavior.fc = "error"
) {
  options(stringsAsFactors = FALSE)
  require(xml2)
  require(tidyverse)
  require(arrow)
  require(data.table)
  # Chargement du .xml et transfo en liste
  message(paste("Fichier à importer : ", chemin.xml))
  message("Import du xml et nettoyage...")
  xml2::read_xml(x = chemin.xml) -> xml.data #"")->xml.data
  xml2::xml_remove(
    xml2::xml_find_all(
      xml.data,
      paste0(
        "/Racine/InfosFoyerRSA/*[",
        "not(self::Personne) and ",
        "not(self::PrestationRSA) and",
        "not(self::DonneesAdministratives) and",
        "not(self::IdentificationRSA) and",
        "not(self::DonneesAdministratives) and",
        "not(self::PrestationRSA/DetailDroitRSA/TroncCommunDroitRSA) and",
        "not(self::PrestationRSA/DetailDroitRSA/DetailCalculDroitRSA) and",
        "not(self::PrestationRSA/DetailDroitRSA/MontantsCalculDroitRSA) and",
        "not(self::PrestationRSA/SituationDossierRSA) and",
        "not(self::PrestationRSA/DetailDroitRSA/DetailCalculDroitRSA)",
        "]"
      )
    )
  )
  message("...fait")
  message("Debut de la conversion en liste....(30-45 mn env)")
  Sys.time() -> heure.debut
  message(heure.debut)
  xml_remove(xml_find_all(xml.data, "//FOYER/Personne/NoeudIndésirable"))
  as_list(xml.data) -> xml.list ## 30mn
  #xml.list <- stream_xml_to_list(chemin.xml, record_tag = "InfosFoyerRSA")
  Sys.time() -> heure.fin
  message(heure.fin)
  message("...fait")
  # Création des data.frames individuelles
  message("Création des data.frames individuelles....(5 mn env)")
  lapply(X = seq_along(xml.list$Racine), FUN = function(I1) {
    if ("Personne" %in% names(xml.list$Racine[[I1]])) {
      xml.list$Racine[[I1]] -> list.for.i
      list.for.i <- func_rename_node_personne(list.for.i)
      la.df_c <- func_format_ListToDf(list.for.i)
      return(la.df_c)
    }
  }) -> list.of.df # 4 mn

  message("....fait")
  message("rbindlist")
  data.table::rbindlist(list.of.df, fill = TRUE) -> final.df
  #dplyr::bind_rows(list.of.df)->final.df
  message("FICHIER_SOURCE_DATE")
  
  final.df <- final.df |>
    #select(any_of(name.vec)) |>
    mutate(
      FICHIER_SOURCE = chemin.xml,
      DATE = creadate_datalist(date.chr)
    )
    
  #ERROR : Can't transform a data frame with duplicate names.
  message("func_transfo...")
  
  final.df <- final.df |>
    func_transfo(SITFAM = sitfam)
  
  message("....fait")
  message(paste0("Save arrow : ajout à ", Parquet_path))
  
  arrow::write_dataset(
    existing_data_behavior = existing_data_behavior.fc,
    dataset = final.df |> group_by(DATE, INSTITUTION),
    path = Parquet_path
  )
  message("....fait")
}
