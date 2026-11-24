#' @author Elie.
#' @title La fonction "XML_TO_PARQUET"
#' @description  l'objectif de cette fonction est d'ajouter à la base de données parquet les nouvelles données RSA.
#' ATTENTION : cette fonction ne supprime pas les ID perdus de vue epuis + 6 ans.
#' ATTENTION : la BDD est groupée avec DATE et INSTITUTION
#' @param chemin.xml chemin du xml
#' @param Parquet_path chemin vers la BDD parquet
#' @param name.vec vecteur des noms de variable à conserver
#' @param date.chr string de date (au format : "2023-12")
#' @return rien: la fonction charge le .xml, converti en data.frame et ajoute la data.frame à la BD parquet.
#' @export
XML_TO_PARQUET <- function(
  chemin.xml = "rsa.RSABEM.00122018.A0110442.xml/rsa.RSABEM.00122018.A0110442.xml",
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
  
  #### Chargement du .xml et transfo en liste
  message(paste("Fichier à importer : ", chemin.xml))
  message("Debut de l'import du xml...")
  read_xml(x = chemin.xml) -> xml.data #"rsa.RSABEM.00122018.A0110442.xml/rsa.RSABEM.00122018.A0110442.xml")->xml.data
  message("...fait")
  message("Debut de la conversion en liste....(30-45 mn env)")
  Sys.time() -> heure.debut
  message(heure.debut)
  as_list(xml.data) -> xml.list ## 30mn
  message("...fait")

  #### 
  names(unlist(xml.list$Racine$InfosFoyerRSA)) -> vector.names
  length(vector.names) -> nb.cols

  ####
  unique(unlist(lapply(xml.list$Racine, FUN = function(j) {
    names(unlist(j))
  }))) -> glo.names
  data.frame(
    matrix(rep(NA, times = length(glo.names)), ncol = length(glo.names))
  ) -> data1
  names(data1) <- glo.names

  ####
  message("Création des data.frames individuelles....(5 mn env)")
  lapply(X = 1:length(xml.list$Racine), FUN = function(i) {
    xml.list$Racine[[i]] -> list.for.i
    names(unlist(list.for.i)) -> vector.names.i
    length(vector.names.i) -> nb.cols.i
    matrix(unlist(list.for.i), ncol = nb.cols.i, byrow = TRUE) -> la.mat
    data.frame(la.mat, stringsAsFactors = FALSE) -> la.df
    names(la.df) <- vector.names.i
    glo.names[glo.names %in% vector.names.i] -> glo.names.i
    la.df[, glo.names.i] -> la.df.last
    row.names(la.df.last) <- i
    la.df.last
  }) -> list.of.df # 4 mn

  message("....fait")

  #save(list.of.df, file="LISTOFDF.RData")
  #lapply(list.of.df, FUN = dim)->list.dims
  #list.of.df<-list.of.df[-c(1)]

  #lapply(1:length(list.of.df2), FUN = function(listi){
  #  list.of.df2[listi][[1]]
  #})->list.of.df3
  message("Combinaison des data.frames individuelles...(5 mn)")
  data.table::rbindlist(list.of.df, fill = TRUE) -> final.df
  #dplyr::bind_rows(list.of.df)->final.df
  final.df <- final.df |>
    select(any_of(name.vec)) |>
    mutate(
      FICHIER_SOURCE = chemin.xml,
      DATE = creadate_datalist(date.chr)
    )
  final.df <- final.df |>
    transfo(SITFAM = sitfam, NATRESS = natress)
  message("....fait")

  message(paste0("Save arrow : ajout à ", Parquet_path))
  arrow::write_dataset(
    existing_data_behavior = existing_data_behavior.fc,
    dataset = final.df |> group_by(DATE, INSTITUTION),
    path = Parquet_path
  )
  message("....fait")
}
