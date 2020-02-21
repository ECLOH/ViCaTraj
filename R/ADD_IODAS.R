#' @export
ADD_IODAS<-function(path.for.iodas="G:/APPLICATIONS/IODAS/Listes/RSA/Observation", 
                    control.IODAS="ISERE_M_Liste des allocataires par référent avec ou sans CER_", 
                    objet.DATAlist=DATAlist2, ecraser=FALSE){
  library(readxl)
  list.files(path.for.iodas)->fich.iodas
  lapply(fich.iodas, function(file.name){
    paste(path.for.iodas, file.name, sep="/")
  })->list.files
  list.files[grepl(pattern="xlsx", 
                   x = list.files, fixed=TRUE)&grepl(pattern=control.IODAS, 
                                                     x = list.files, fixed=TRUE)]->list.xls.files
  gsub(paste(path.for.iodas, control.IODAS, sep = "/"), "", list.xls.files)->dats.files
  substring(dats.files, 1, 10)->dats.files
  strsplit(dats.files, split = "-")->dats.files
  data.frame(do.call("rbind", dats.files), stringsAsFactors = FALSE)->dats.files
  order(dats.files$X1, dats.files$X2, dats.files$X3)->dats.files$ORDER
  dats.files$KEEP<-sapply(1:nrow(dats.files), FUN = function(i){
    subset(dats.files, dats.files$X1==dats.files$X1[i]&dats.files$X2==dats.files$X2[i])->tempdf
    dats.files$ORDER[i]==max(tempdf$ORDER, na.rm=TRUE)
  })
  unlist(list.xls.files)->vec.xls.files
  dats.files$vec<-vec.xls.files
  #print(dats.files)
  message(paste("Nombre de fichers IODAS xls dans le dossier: ", length(list.xls.files)))
  
  for(ji in 1:nrow(dats.files)){
    fileIODAS<-dats.files$vec[ji]
    if(dats.files$KEEP[ji]==TRUE){
      read_xlsx(path = as.character(fileIODAS), sheet = "version départementale", skip = 2, col_names = TRUE)->dtIODAS
      gsub(paste(path.for.iodas, control.IODAS, sep = "/"), "", fileIODAS)->dats
      substring(text = dats, 1, 10)->dats
      dtIODAS$DATE_IODAS<-dats
      dtIODAS$DATE_IODAS<-as.Date(as.character(dtIODAS$DATE_IODAS), format="%Y-%m-%d")
      dtIODAS$DATE_IODAS<-format(dtIODAS$DATE_IODAS, "%Y-%m")
      sapply(objet.DATAlist, FUN = function(li){unique(li$DATE)==unique(dtIODAS$DATE_IODAS)})->res
      if(sum(res)==1){
        if(ecraser==FALSE){
          if(!"DATE_IODAS"%in%names(objet.DATAlist[res][[1]])){
            objet.DATAlist[res][[1]]<-merge( objet.DATAlist[res][[1]] , dtIODAS, by.x="Personne.Identification.NIR", by.y = "38-identifiant NIR", all.x = TRUE, all.y = FALSE)
            message(paste(fileIODAS, ": données IODAS ajoutées"))
          } else {
            message(paste(fileIODAS, ": IODAS déjà intégré pour ce mois"))
          }
        } else {
          objet.DATAlist[res][[1]]<-objet.DATAlist[res][[1]][ , names(objet.DATAlist[res][[1]])[!names(objet.DATAlist[res][[1]])%in%dtIODAS ]]
          objet.DATAlist[res][[1]]<-merge( objet.DATAlist[res][[1]] , dtIODAS, by.x="Personne.Identification.NIR", by.y = "38-identifiant NIR", all.x = TRUE, all.y = FALSE)
          message(paste(fileIODAS, ": données IODAS ajoutées"))
        }
      } else {
        message(paste(fileIODAS, ": données IODAS (mois) non présentes dans données CAF (mois)"))
      }
    }
  }
  return(objet.DATAlist)
}