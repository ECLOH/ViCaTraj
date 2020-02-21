#' @export
AJOUT_DATA_TIERS<-function(donnees.tiers="U:/ODE/OBS-EVAL/Données et conventions/DONNEES/ZONAGE/2018/LIENS 2018.xls", 
                           var.origine="DonneesAdministratives.Adresse.AdresseDetailleeFrance.NUMCOM", 
                           var.tiers="IDCOMMUNE",objet.DATAlist=DATAlist2 ){
  
  if(grepl(pattern = "csv", x =donnees.tiers, fixed=TRUE)){
    read.csv2(file = donnees.tiers , header=TRUE)->data.tiers
  } else {
    if(grepl(pattern = "xlsx", x =donnees.tiers, fixed=TRUE)){
      library(readxl)
      read_xlsx(path = donnees.tiers, sheet = 1, col_names = TRUE, skip = 0)->data.tiers
    } else {
      if(grepl(pattern = "xls", x =donnees.tiers, fixed=TRUE)){
        read_xls(path = donnees.tiers, sheet = 1, col_names = TRUE, skip = 0)->data.tiers
      }
    }
  }
  
  lapply(objet.DATAlist, function(dfi){
    dfi.t<-merge(dfi, data.tiers, by.x = var.origine, by.y = var.tiers, all.x=TRUE)
    message("Coucou")
    return(dfi.t)
  })->objet.DATAlist.res
  
  return(objet.DATAlist.res)
}