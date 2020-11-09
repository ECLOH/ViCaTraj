library(data.table)
for(i in c("mtcars_01012020.csv", 
"mtcars_01022020.csv", 
"mtcars_01032020.csv", 
"mtcars_01042020.csv", 
"mtcars_01052020.csv", 
"mtcars_01062020.csv")){
  mtcars$FICHIER_DESTINATION<-paste0("/home/mrie/Bureau/GIT_paquets/temporaires_data/CSV_TEST/", i)
  fwrite(mtcars, file = paste0("/home/mrie/Bureau/GIT_paquets/temporaires_data/CSV_TEST/", i))
}
malist<-lapply(list.files("/home/mrie/Bureau/GIT_paquets/temporaires_data/CSV_TEST/"), function(x){
  fread(paste0("/home/mrie/Bureau/GIT_paquets/temporaires_data/CSV_TEST/", x))
})
names(malist)<-paste0("D", c("01012020", "01022020", "01032020", "01042020", "01052020", "01062020"))
rm(i)
rm(mtcars)
malist<-lapply(malist, function(x){as.data.frame(x)})


newlist<-APPLY_READCSV_DOSSIER(emplacement.dossier = "/home/mrie/Bureau/GIT_paquets/temporaires_data/CSV_TEST", pattern.extension = ".csv", autre.pattern = NULL, liste_existante = malist, variable_identification_fichiers = "FICHIER_DESTINATION")

mtcars$FICHIER_DESTINATION<-paste0("/home/mrie/Bureau/GIT_paquets/temporaires_data/CSV_TEST/", "mtcars_01072020.csv")
fwrite(mtcars, file = paste0("/home/mrie/Bureau/GIT_paquets/temporaires_data/CSV_TEST/", "mtcars_01072020.csv"))
mtcars$FICHIER_DESTINATION<-paste0("/home/mrie/Bureau/GIT_paquets/temporaires_data/CSV_TEST/", "mtcars_01082020.csv")
fwrite(mtcars, file = paste0("/home/mrie/Bureau/GIT_paquets/temporaires_data/CSV_TEST/", "mtcars_01082020.csv"))
mtcars$FICHIER_DESTINATION<-paste0("/home/mrie/Bureau/GIT_paquets/temporaires_data/CSV_TEST/", "mtcars_01092020.csv")
fwrite(mtcars, file = paste0("/home/mrie/Bureau/GIT_paquets/temporaires_data/CSV_TEST/", "mtcars_01092020.csv"))

newlist<-APPLY_READCSV_DOSSIER(emplacement.dossier = "/home/mrie/Bureau/GIT_paquets/temporaires_data/CSV_TEST", pattern.extension = ".csv", autre.pattern = NULL, liste_existante = malist, variable_identification_fichiers = "FICHIER_DESTINATION", vec.correspondance.names = NULL, seq.dates=NULL)
names(newlist)


newlist<-APPLY_READCSV_DOSSIER(emplacement.dossier = "/home/mrie/Bureau/GIT_paquets/temporaires_data/CSV_TEST", pattern.extension = ".csv", autre.pattern = NULL, liste_existante = malist, variable_identification_fichiers = "FICHIER_DESTINATION", vec.correspondance.names = NULL, seq.dates=c("01/07/2018", "01/09/2018", "month"))
names(newlist)

newlist<-APPLY_READCSV_DOSSIER(emplacement.dossier = "/home/mrie/Bureau/GIT_paquets/temporaires_data/CSV_TEST", pattern.extension = ".csv", autre.pattern = NULL, liste_existante = malist, variable_identification_fichiers = NULL, 
                               vec.correspondance.names = c("D01072020"="mtcars_01072020.csv", 
                                   "D01082020"="mtcars_01082020.csv", 
                                   "D01092020"="mtcars_01092020.csv"),seq.dates=NULL, combine_new = TRUE)
names(newlist)

newlist<-APPLY_READCSV_DOSSIER(emplacement.dossier = "/home/mrie/Bureau/GIT_paquets/temporaires_data/CSV_TEST", pattern.extension = ".csv", autre.pattern = NULL, liste_existante = malist, variable_identification_fichiers = NULL, vec.correspondance.names = c("D01072020"="mtcars_01072020.csv", 
                                  "D01092020"="mtcars_01092020.csv"), seq.dates=NULL, combine_new = TRUE)
names(newlist)

                               