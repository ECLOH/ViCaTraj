#' @export

seqggplot.internal<-function(objseq.r1 = objseq, TYPE.r1=TYPE, grup_var.r1=NULL, 
                             col_selected.r1=col.selected, merge_mods.r1, 
                             pmin.sup1=0.05, STR.SUBS.1=NULL){
  library(dplyr)
  attributes(objseq.r1)$cpal->col.i
  names(col.i)<-attributes(objseq.r1)$alphabet
  
  attributes(objseq.r1)$names->name_time
  attributes(objseq.r1)$alphabet->labels_order
  
  if(is.null(grup_var.r1)){
  DONNEES_POUR_PLOT(TYPE=TYPE.r1, objseq = objseq.r1, arrondi=2, pmin.sup=pmin.sup1, STR.SUBS=STR.SUBS.1)->dats
  as.data.frame(dats)->dats
  dats$level<-"Ensemble"
  dats$ID<-row.names(dats)
  } else {
    if(is.factor(grup_var.r1)){
      grup_var.r1<-as.character(grup_var.r1)
    }
    
    lapply(1:length(unique(grup_var.r1)), FUN = function(i){
      objseq.r1[grup_var.r1==unique(grup_var.r1)[i] , ]->objseq.r1.i
      objseq.r1.i[!is.na(objseq.r1.i[ , 1]) , ]->objseq.r1.i
      DONNEES_POUR_PLOT(TYPE=TYPE.r1, objseq = objseq.r1.i, arrondi=2,pmin.sup=pmin.sup1, STR.SUBS=STR.SUBS.1)->dats
      as.data.frame(dats)->dats
      dats$level<-unique(grup_var.r1)[i]
      dats$ID<-row.names(dats)
      
      return(dats)
    })->lidat
    do.call("rbind", lidat)->dats
    
  }
  print(head(dats, 50))
  
  if(TYPE.r1=="d"){
    names(dats)[names(dats)!="level"&names(dats)!="ID"]->namvar
    reshape(dats, direction='long', varying = list("VAR"=namvar), times = namvar)->ggdats
    names(ggdats)<-c("level", "ID", "time", "VAR", "id")
    ggdats$time<-factor(ggdats$time, levels = name_time, ordered = TRUE )
    ggdats$ID<-factor(ggdats$ID, levels = labels_order, ordered = TRUE )
    
    # if(!is.null(merge_mods.r1)){
    #   if(merge_mods.r1==TRUE){
    #     ggdats$level<-as.character(ggdats$level)
    #     ggdats$level[ggdats$level!="HORS SELECTION"]<-"SELECTION"
    #   }
    # }
    print(head(ggdats, 100))
    print(ggdats$VAR)
    
    ggplot(ggdats, aes(x=time, y=VAR, fill=ID))+
      geom_bar(stat = "identity" )+
      #geom_label_repel(aes(label=VAR), stat="identity", position=position_stack(vjust = 0.5), size=3)+
      scale_fill_manual(values = col.i, name="États : ")+
      theme_excel()+
      xlab(label = "")+ylab(label = "")+
      theme(axis.title = element_blank(), axis.text.x =  element_text(angle = 45) ,legend.text = element_text(size=2) )+
      facet_wrap(.~level)
    
  } else {
    if(TYPE.r1=="mt"){
      dats->ggdats
      #ggdats$VAR<-row.names(ggdats)
      ggdats[order(ggdats$Mean, decreasing = TRUE) , ]->ggdats
      #ggdats$ID<-factor(ggdats$ID, levels =labels_order, ordered = TRUE)
      # if(!is.null(merge_mods.r1)){
      #   if(merge_mods.r1==TRUE){
      #     df$level<-as.character(df$level)
      #     df$level[df$level!="HORS SELECTION"]<-"SELECTION"
      #   }
      # }
      
      ggdats%>%group_by(level)%>%arrange(desc(Mean))->ggdats
      
      ggplot(ggdats, aes(x=ID, y=Mean, fill=ID))+
        geom_bar(stat = "identity" )+
        geom_label(aes(label=Mean), stat="identity", size=3, colour=gray(0.2), nudge_y = 0.1*(max(ggdats$Mean)-min(ggdats$Mean)), show.legend = FALSE)+
        scale_fill_manual(values = col.i, name="États : ")+
        theme_excel()+
        xlab(label = "")+ylab(label = "")+
        theme(axis.title = element_blank() , axis.text.x =  element_text(angle = 45), legend.text = element_text(size=2))+
        facet_wrap(.~level)
    } else {
      if(TYPE.r1=="ms"){
        if(is.null(grup_var.r1)){
        seqstatd(seqdata = objseq.r1, )->stats.data
        colnames(stats.data$Frequencies)->store_colnames
        data.frame(stats.data$Frequencies)->df
        names(df)<-store_colnames
        df$level<-"Ensemble"
        df$ID<-row.names(df)
        
        } else {
          lapply(1:length(unique(grup_var.r1)), FUN = function(i){
            objseq.r1[grup_var.r1==unique(grup_var.r1)[i] , ]->objseq.r1.i
            seqstatd(seqdata = objseq.r1.i)->stats.data
            colnames(stats.data$Frequencies)->store_colnames
            data.frame(stats.data$Frequencies)->df
            names(df)<-store_colnames
            df$level<-unique(grup_var.r1)[i]
            df$ID<-row.names(df)
            return(df)
            
          })->lidat
          #data.frame(
          do.call("rbind", lidat)->df
        }
        names(df)[names(df)!="level"&names(df)!="ID"]->namvar
        
        reshape(df, direction="long", varying = list(namvar), times=namvar, ids = row.names(df))->df
        names(df)[!names(df)%in%c("time", "id", "level", "ID")]<-"value"
        
        # level
        # if(!is.null(merge_mods.r1)){
        #   if(merge_mods.r1==TRUE){
        #     df$level<-as.character(df$level)
        #     df$level[df$level!="HORS SELECTION"]<-"SELECTION"
        #   }
        # }
          
        df %>% group_by(level, time) %>% arrange(desc(value)) %>% slice(1) ->df.max
        df.max %>% ungroup() %>% group_by(level) %>% arrange(level, match(time, names(objseq.r1))) ->df.max
        df.max->ggdats2
        ggdats2$time<-factor(ggdats2$time, levels=name_time, ordered = TRUE)
        print(names(ggdats2))
        
        ggplot(ggdats2, aes(x=time, y=value, fill=ID))+
          geom_bar(stat = "identity")+
          ylim(c(0,  1)) + 
          #geom_label(aes(label=paste(round(value*100, 1), "%", sep = "")), size=1,
          #           nudge_y = 0.1, alpha=0.7, show.legend = FALSE, colour=gray(0.3))+
          scale_fill_manual(values = col.i, name="États : ")+
          theme_excel()+
          xlab(label = "")+ylab(label = "")+
          theme(axis.title = element_blank() , axis.text.x =  element_text(angle = 45, hjust = 1), legend.text = element_text(size=2))+
          facet_wrap(.~level)
        
        
      } else {
        if(TYPE.r1=="flux"){
          graph_flux_grp(seq_data = objseq.r1, col_periode = col_selected.r1, group_var = grup_var.r1)#, merge_mods = merge_mods.r1)
        } else {
          if(TYPE.r1=="sous.seq"){
            #dats$Support<-dats$Support/100
              gg<-ggplot(data=dats,aes(x=reorder(event,Support),y=Support)) +
                geom_bar(stat='identity',width=0.9,fill=gray(0.1))+
                geom_label(aes(label=paste(round(Support,2), "%", sep="")),
                           hjust=-1, colour="white", fill=gray(0.3), size=7/length(unique(dats$level)))+ 
                theme_hc()+
                xlab('')+
                theme(axis.text.x = element_text(size=9, angle=90, hjust=1,vjust = 0.3),
                      axis.title.x = element_blank(),
                      plot.title = element_text(hjust = 0.5,size=18,face = "bold"),
                      plot.subtitle = element_text(hjust = 0.5,size=14))+ylim(0,100)+
                coord_flip()+
                facet_wrap(.~level, )
              return(gg)
              
          } else {
    seqplot(seqdata =  objseq.r1, type=TYPE.r1, group = grup_var.r1)
          }
        }
    }}
  }
  #return(p)
}