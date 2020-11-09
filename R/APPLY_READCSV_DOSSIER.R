#' @author Elie
#' @description  ...
#' @title La fonction "APPLY_READCSV_DOSSIER"
#' @param emplacement.dossier emplacement du dossier. Sans "/" à la fin. 
#' @param pattern.extension pattern qui indique l'extension dans les fichiers
#' @param autre.pattern autre pattern qui permettra de filtrer les fichiers
#' @param liste_existante une liste existe-t-elle déjà? Si non null, alors la fonction va essayer d'identifier les mois non pris en compte dans la liste, lire les fichiers correspondants et les insérer.
#' @param variable_identification_fichiers éventuelle variable dans les data.frames de la liste_existante qui donne les fichiers source. Non obligatoire.
#' @param seq.dates pour éviter que la fonction cré les noms des data.frames ajoutés à partir de leur nom de fichier, on peut spécifier une séquence de date : c("date d'origine", "date d'arrivée", interval de temps), au format suivant : c("01/01/2020", "01/08/2020", "month"). Format de date : "%d/%m/%Y" et interval accepeté par la fonction seq.Date()
#' @param vec.correspondance.names ECRASE 'variable_identification_fichiers' et "seq.dates". Vecteur nommé avec les fichiers à aller chercher (à ajouter), présents dans 'emplacement.dossier' et nommé avec le nom qu'on souhaite leur attribuer dans le datalist final. 
#' @param combine_new si TRUE alors retourne une liste déjà combinée (contient liste_existante et les nouveaux data.frame). Si FALSE ne retourne que la liste des nouveaux data.frames (utile si on veut économiser de la mémoire). On peut faire ensuite: c(liste_existante, nouelle_liste)
#' @return liste de data.frame
#' @export
APPLY_READCSV_DOSSIER<-function(emplacement.dossier="K:/X_APPLIS/EvaluationRSA/FICHIERS_XML_CSV", 
                                pattern.extension=".csv", 
                                autre.pattern="RSABEM",
                                liste_existante=DATAlist, 
                                variable_identification_fichiers="FICHIER_DESTINATION",
                                vec.correspondance.names=c("D2017-01-01"="rsa.RSABEM.00012017.B0403042.csv"),
                              #  common.pattern=TRUE, 
                                seq.dates=c("01/01/2018","01/01/2019","month"), 
                              combine_new=TRUE){

  ####
  list.files(emplacement.dossier)->fichiers
  lapply(fichiers, function(file.name){
    paste(emplacement.dossier, file.name, sep="/")
  })->list.files
  if(!is.null(autre.pattern)){
    list.files[grepl(pattern=pattern.extension, 
                     x = list.files, fixed=TRUE)&grepl(pattern=autre.pattern, 
                                                       x = list.files, fixed=TRUE)]->list.csv.files
  } else {
    list.files[grepl(pattern=pattern.extension, 
                     x = list.files, fixed=TRUE)]->list.csv.files
  }
  message(paste("Nombre de fichers csv dans le dossier: ", length(list.csv.files)))
  unlist(list.csv.files)->vec.csv.files
  ####
  if(!is.null(liste_existante)){
    if(!is.null(variable_identification_fichiers)&inherits(x = variable_identification_fichiers, what = "character")){
      message("coucou29")
      v.fich<-sapply(liste_existante, function(x){
        unique(x[ , variable_identification_fichiers])
      })
      v.fich<-sapply(v.fich, function(x){
        if(grepl(pattern = "/", x = x, fixed = TRUE)){
          strsplit(x = x, split = "/")[[1]]->fisel
          fisel[[length(fisel)]]
        } else {x} 
      })
      print("coucou39")
      print(v.fich)
      list.csv.files<-list.csv.files[!gsub(pattern = paste0(emplacement.dossier, "/"), replacement = "", x =list.csv.files)%in%v.fich]
      print("coucou49")
      print(list.csv.files)
    }
    if(!is.null(vec.correspondance.names)&length(list.csv.files)>0){
      list.csv.files<-unname(vec.correspondance.names)
    }
    message(paste("Nombre de fichers csv à ajouter à 'liste_existante': ", length(list.csv.files)))
    message(paste("'liste des fichiers à ajouter': "))
    print(list.csv.files)
    
  }
  if(length(list.csv.files)<1&!is.null(liste_existante)){
    stop("Pas de .csv à ajouter à 'liste_existante'. Execution arrêtée")
  }
  
  
  ####
  df.RSA.value<-data.frame("TEXT"=c("0"="Nouvelle demande en attente de décision CG pour ouverture du droit",
                                    "1"="Droit refusé", 
                                    "2"="Droit ouvert et versable",
                                    "3"="Droit ouvert et suspendu (le montant du droit est calculable, mais l'existence du droit est remis en cause)",
                                    "4"="Droit ouvert mais versement suspendu (le montant du droit n'est pas calculable)",
                                    "5"="Droit clos",
                                    "6"="Droit clos sur mois antérieur ayant eu un contrôle dans le mois de référence pour une période antérieure"), "MODALITE"=as.character(0:6))
  ###
  donnees.mois<-lapply(1:length(list.csv.files), FUN = function(i){
    list.csv.files[[i]]->listi
    message(paste(i, "/", length(list.csv.files), "---------------",  sep=" "))
    message(paste("FROM :",  listi))
    message("...")
    message(class(listi))
    if(!grepl(pattern = emplacement.dossier, x = listi, fixed = TRUE)){
      listi<-paste0(emplacement.dossier, "/", listi)
    }
    data.table::fread(listi, stringsAsFactors = FALSE)->data.i
    #read.csv(file = listi, header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)->data.i
    #### AJOUT DE DATE ####
    message("DATE...")
    unlist(strsplit(x = unique(data.i$FICHIER_DESTINATION), split = c("."),  fixed = TRUE))[3]->DATE
    paste("01", substr(DATE, 3, 4), substr(DATE, 5, 8), sep="/")->a 
    b<-as.Date(a,format="%d/%m/%Y")
    b<-format(b, "%Y-%m")
    data.i$DATE<-b
    #### AJOUT DE RSA ###
    as.character(data.i$PrestationRSA.SituationDossierRSA.EtatDossierRSA.ETATDOSRSA)->data.i$PrestationRSA.SituationDossierRSA.EtatDossierRSA.ETATDOSRSA
    message("RSA_simple...")
    sapply(1:nrow(data.i), FUN = function(i){
      if(is.na(data.i$PrestationRSA.SituationDossierRSA.EtatDossierRSA.ETATDOSRS[i])){
        return(NA)
      } else {
        if(data.i$PrestationRSA.SituationDossierRSA.EtatDossierRSA.ETATDOSRS[i]%in%c("5", "6", "1")){
          return("NO.RSA")
        } else {
          if(data.i$PrestationRSA.SituationDossierRSA.EtatDossierRSA.ETATDOSRS[i]%in%c("3", "4")){
            return("SUSPENDU")
          } else {
            if(data.i$PrestationRSA.SituationDossierRSA.EtatDossierRSA.ETATDOSRS[i]=="2"){
              return("RSA")
            } else {
              return(NA)
            }
          }
        }
      }
    })->data.i$RSA_simple
    message("ETATDOSRSA_TEXT...")
    sapply(1:nrow(data.i), FUN = function(i){
      as.character(subset(df.RSA.value, df.RSA.value$MODALITE==data.i$PrestationRSA.SituationDossierRSA.EtatDossierRSA.ETATDOSRSA[i])$TEXT)->res
      if(length(res)<1){
        return(NA)
      } else {
        return(res)
      }
    })->data.i$ETATDOSRSA_TEXT
    as.data.frame(data.i)->data.i
    return(data.i)
  })
  if(is.null(vec.correspondance.names)&is.null(seq.dates)){
  gsub(pattern = paste(emplacement.dossier, "/", sep=""), replacement = "", x = vec.csv.files)->vecnames
  unlist(lapply(donnees.mois, function(x){unique(as.character(x$DATE))}))->vecnames
  names(donnees.mois)<-vecnames
  donnees.mois<-donnees.mois[names(donnees.mois)[order(names(donnees.mois))]]
  } else {
    if(!is.null(vec.correspondance.names)){
      names(donnees.mois)<-names(vec.correspondance.names)
    } else {
      if(!is.null(seq.dates)){
        names(donnees.mois)<-seq.Date(from = as.Date(x = seq.dates[1], format="%d/%m/%Y"), 
                                      to = as.Date(x = seq.dates[2], format="%d/%m/%Y"), 
                                      by = seq.dates[3])
      }
    }
  }
  if(sum(is.na(names(donnees.mois)))==length(donnees.mois)){
    gsub(pattern = emplacement.dossier, replacement = "", x = list.csv.files)->vecna
    gsub(pattern = pattern.extension, replacement = "", x = vecna)->vecna
    gsub(pattern = "/", replacement = "", x = vecna)->vecna
    names(donnees.mois)<-vecna
  }
  if(combine_new==TRUE&!is.null(liste_existante)){
    c(liste_existante, donnees.mois)->donnees.mois
  }
  return(donnees.mois)
}