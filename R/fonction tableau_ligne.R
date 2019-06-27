# http://olivier.godechot.free.fr/hoparticle.php?id_art=465
# https://stackoverflow.com/questions/39757897/using-formattable-in-r-with-dynamic-column-headers

#' Title
#'
#' @param data data frame
#' @param var_grp variable contenant la classification
#' @param var variable étudiée
#'
#' @return un data frame mis en forme
#' @export

tableau_ligne<-function(data,var_grp,var){
  
  if(!(is.data.frame(data))){
    stop(" 'data' doit être un data frame ")
  }
  
  if(!(var_grp %in% colnames(data))){
    stop(" 'var_grp' doit être une variable du jeu de donnees ")
  }
  
  if(!(var %in% colnames(data))){
    stop(" 'var' doit être une variable du jeu de donnees ")
  }
  
  #Tableau des profils lignes
  prop.table(addmargins(table(data[,var_grp],data[,var]),1),1)->tableau
  dim(tableau)[1]->NumGlobal
  rownames(tableau)[NumGlobal]<-"Global"
  
  as.data.frame(as.matrix.data.frame(tableau))->tabDf
  colnames(tabDf)<-colnames(tableau)
  rownames(tabDf)<-rownames(tableau)
  
                                    #### ANCIENNE METHODE ####
  # # On colorie les valeurs supérieur à la valeur dans l'ensmeble du jeu de données
  # # On multiplie par le nombre de modalités etudiées
  # SupGlobalTableau <- rep(list(formatter("span", 
  #                                        style = x ~ style(color = ifelse(x > x[NumGlobal], "seagreen", "black"),"font-weight" = ifelse(x > x[NumGlobal], "bold", NA)))),dim(tableau)[2])
  # 
  # #On affecte le nom des modalités étudiées pour automatiser la fonction formattable
  # names(SupGlobalTableau)<-colnames(tabDf)
  # 
  # tab<-formattable(tabDf, SupGlobalTableau)%>%as.datatable()  #permet de convertir et d'utiliser un renderDataTable dans shiny
  
  #tableau effectif
  addmargins(table(data[,var_grp],data[,var]),1)->TabEff
  dim(TabEff)[1]->NumGlobalE
  rownames(TabEff)[NumGlobalE]<-"Global"
  
  as.data.frame(as.matrix.data.frame(TabEff))->DFTabEff
  colnames(DFTabEff)<-colnames(TabEff)
  rownames(DFTabEff)<-rownames(TabEff)
  
  1.96*sqrt((tabDf*(1-tabDf))/TabEff)->tabValDemiIC
  
  #Pour résoudre le problème des cas où une classe ne contient pas d'individus
  tabValDemiIC[is.na(tabValDemiIC)]<-0
  
  tabDf+tabValDemiIC->BorneSupIC
  tabDf-tabValDemiIC->BorneInfIC
  
  #Crétion d'un data frame contenant les valeurs "S" (ou "I") lorsque la valeur est significativement supérieure (ou inférieure) à la valeur dans le global. Sinon, on met "N".
  DFSignif<-tabDf
  colnames(DFSignif)<-paste0(colnames(tabDf),"2")
  
  for (j in 1:dim(tableau)[2]){
    borneSupG<-BorneSupIC[dim(tableau)[1],j]
    borneInfG<-BorneInfIC[dim(tableau)[1],j]
    valG<-tabDf[dim(tableau)[1],j]
    for (i in 1:(dim(tableau)[1]-1)){
      if (tabDf[i,j]>valG){
        if(BorneInfIC[i,j]>BorneSupIC[dim(tableau)[1],j]){
          DFSignif[i,j]<-"S"
        }else{
          DFSignif[i,j]<-"N"
        }
      }else{
        if (tabDf[i,j]<valG){
          if(BorneInfIC[dim(tableau)[1],j]>BorneSupIC[i,j]){
            DFSignif[i,j]<-"I"
          }else{
            DFSignif[i,j]<-"N"
          }
        }
      }
      
    }
  }
  
  # On s'occupe de  la ligne du global
  DFSignif[NumGlobal,]<-"N"
  
  
  cbind(tabDf,DFSignif)->tab2
  
  
  tab<-datatable(tab2,options = list(
    # On cache les colonnes avec les valeurs "I","S","N"
    columnDefs = list(list(targets = c(1:dim(TabEff)[2])+dim(TabEff)[2], visible = FALSE)))) %>% formatStyle(
      colnames(tab2)[c(1:dim(TabEff)[2])], colnames(tab2)[c(1:dim(TabEff)[2])+dim(TabEff)[2]],
      color = styleEqual(c("I", "S"), c('red', 'green'))
    )%>%formatPercentage(colnames(tab2)[c(1:dim(TabEff)[2])], 2)
  
  return(tab)
}

#' @examples


