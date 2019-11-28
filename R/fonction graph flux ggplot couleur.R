################### Fonction graph de flux par groupe #####################
#' Fonction pour faire des graphiques de flux pour un groupe d'individus donné
#'
#' @param seq_data 
#' @param col_periode 
#' @param group_var 
#'
#' @return
#' @export
#'
graph_flux_grp<-function(seq_data, col_periode, group_var=actcal.seq[ , 1]) {
  library(ggalluvial)
  
  
  # Tester si l'utilisateur rentre le bon format pour les param?tres
  if (!(is.vector(col_periode))){
    stop(" 'col_periode' doit être un vecteur ")
  }
  if (length(col_periode)<2){
    stop(" 'col_periode' doit avoir au moins 2 éléments ")
  }
  if (any(!(col_periode %in% colnames(seq_data)))){
    stop(" 'col_periode' doit contenir des varibles de la séquence ")
  }
  
  ##### Couleur ######
  col_flux(data,seq_data)->df_col
  
  
  ####  ####
  data.frame(seq_data)->data_select
  names(data_select)<-colnames(seq_data)
  
  data_select$ID<-row.names(seq_data)
  if(is.null(group_var)){
    data_select$level<-"Ensemble"
    } else {data_select$level<-group_var}
  
  data_select2<-tidyr::gather(data_select[,c(col_periode,"ID", "level")],"Temps","Etats",-ID, -level)
  
  subset(data_select2, data_select2$Temps%in%col_periode)->data_select2
  
  
  data_select2$freq<-1
  data_select2 %>% group_by(level, Temps,Etats) %>% summarise(nb=sum(freq))->dt1
  dt1 %>% group_by(level, Temps) %>% mutate(prop=sprintf("%.0f %%",(nb/sum(nb))*100))->dt2
  
  dt2[dt2$prop== "0 %","prop"]<-""
  dt2[dt2$prop== "1 %","prop"]<-""
  dt2[dt2$prop== "2 %","prop"]<-""
  
  left_join(data_select2,dt2,by=c("Temps","Etats", "level"))->dt3
  
  dt3$Temps<-factor(dt3$Temps, levels = col_periode, ordered = TRUE)
  
  # if(merge_mods==TRUE){
  #   dt3$level<-as.character(dt3$level)
  #   dt3$level[dt3$level!="HORS SELECTION"]<-"SELECTION"
  # }
  
  #### plot ####
  if (length(unique(data_select2$Etats))<=12){
    #La fonction mutate permet de trier selon l'ordre temporelle de la séquence
    g<-ggplot(data=dt3, mapping = aes(x = Temps, stratum = Etats, alluvium = ID,fill = Etats,label=prop)) +
      geom_lode() + geom_flow() +
      geom_stratum() +
      geom_text(stat = "stratum",size=3)+
      labs(fill = "Légende")+
      scale_fill_manual(values = df_col)+ #change les couleurs utilis?es pour le graphique
      #• ggtitle(paste("Graphique de flux du groupe",label_grp,"avec",nb_ligne,"individus."))+
      scale_y_continuous(breaks=NULL)+ # enlever l'axe des y
      theme(panel.background = element_rect(fill = "white", colour="white"), #fond blanc du graphique
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5)) +# centre le titre
      facet_wrap(.~level, scales = "free_y")
  } else{
    g<-ggplot(data=dt3, aes(x = Temps, stratum = Etats, alluvium = dt3[,name_id],fill = Etats,label=prop)) +
      geom_lode() + geom_flow() +
      geom_stratum() +
      geom_text(stat = "stratum",size=3)+
      labs(fill = "Légende")+
      ggtitle(paste("Graphique de flux du groupe",label_grp,"avec",nb_ligne,"individus."))+
      scale_y_continuous(breaks=NULL)+ # enlever l'axe des y
      theme(panel.background = element_rect(fill = "white", colour="white"), #fond blanc du graphique
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5)) # centre le titre
  }
  return(g)
}
  