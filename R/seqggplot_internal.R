#' @export

seqggplot.internal<-function(objseq.r1 = objseq, TYPE.r1=TYPE, grup_var.r1=NULL, 
                             col_selected.r1=col.selected, merge_mods.r1, 
                             pmin.sup1=0.05, STR.SUBS.1=NULL, max.k1=1,SORTV1=NULL, 
                             PAS.temps1=PAS.temps, TIME.varying1=TIME.varying, Pourc.eff1=Pourc.eff, Sens1=Sens){
  
  if(TYPE.r1=="txtr"){
  if(!is.null(Sens1)){
  if(Sens1==TRUE){
    objseq.r1<-objseq.r1[ , ncol(objseq.r1):1]
  }}}
  
  
  library(dplyr)
  attributes(objseq.r1)$cpal->col.i
  names(col.i)<-attributes(objseq.r1)$alphabet
  
  attributes(objseq.r1)$names->name_time
  attributes(objseq.r1)$alphabet->labels_order
  
  
  if(is.null(grup_var.r1)){
    if(TYPE.r1!="txtr"){
     # message("YOYO")
  DONNEES_POUR_PLOT(TYPE=TYPE.r1, objseq = objseq.r1, arrondi=2, pmin.sup=pmin.sup1, STR.SUBS=STR.SUBS.1, max.k = max.k1,
                    PAS.temps=PAS.temps1, TIME.varying=TIME.varying1, Pourc.eff=Pourc.eff1, Sens=Sens1)->dats
  
  if(TYPE.r1=="sous.seq"){
    message("debug1")
    seqecreate(objseq.r1)->seqe2
    if(is.null(STR.SUBS.1)){
      seqefsub(seqe2,pmin.support =pmin.sup1,  #str.subseq = unique(dats$event), 
               max.k = max.k1)->sub2
    } else {
      seqefsub(seqe2, str.subseq = STR.SUBS.1, max.k = max.k1, pmin.support =pmin.sup1)->sub2
    }
    print(sub2)
    print(seqe2)
    if(is.null(grup_var.r1)){
      dats$CodeValue<-0
    } else {
      if(length(unique(grup_var.r1))>1){
        seqecmpgroup(subseq = sub2, group = grup_var.r1, method = "bonferroni")->discr2
        cbind("subseq"=as.character(discr2$subseq),  discr2$data)->datdiscr2
        reshape(datdiscr2, direction = "long", varying = list(colnames(datdiscr2)[grepl("Resid.", colnames(datdiscr2), fixed = TRUE)]), 
                times = colnames(datdiscr2)[grepl("Resid.", colnames(datdiscr2), fixed = TRUE)])->datdiscr2
        datdiscr2$CodeValue<-sapply(1:nrow(datdiscr2), function(i){
          if(datdiscr2$p.value[i]<0.1){
            if(datdiscr2$p.value[i]>0.05){
              1} else {
                if(datdiscr2$p.value[i]>0.01){
                  2} else {
                    3
                  }
              }
          } else {0}
        })
        datdiscr2$CodeValue<-sapply(1:nrow(datdiscr2), function(i){
          if(datdiscr2[ , grepl("Resid.", colnames(datdiscr2), fixed = TRUE)][i]<0){
            -datdiscr2$CodeValue[i]
          } else {
            datdiscr2$CodeValue[i]
          }
        })
        datdiscr2$time<-gsub(pattern = "Resid.", replacement = "", fixed = TRUE, x = datdiscr2$time)
        
        if(length(intersect(dats$event, datdiscr2$subseq))==length(unique(dats$event))){
          message("debug2")
          
          left_join(dats, datdiscr2[, c("subseq", "time", "CodeValue")], by=c("event"="subseq", "level"="time"))->dats
        } else {
          dats$CodeValue<-0
        }
      } else {
        dats$CodeValue<-0
      }
    }
  }
  
  
    } else {
    seqtrate(seqdata = objseq.r1, time.varying = TIME.varying1, lag = PAS.temps1 , count = Pourc.eff1 )->tr.tx
    data.frame(as.table(tr.tx))->dfres
    names(dfres)[names(dfres)=="Var1"]<-"Départ"
    names(dfres)[names(dfres)=="Var2"]<-"Arrivée"
    names(dfres)[names(dfres)=="Var3"]<-"Date.départ"
    dfres->dats
    }
  as.data.frame(dats)->dats
  dats$level<-"Ensemble"
  dats$ID<-row.names(dats)
    } else {
    if(is.factor(grup_var.r1)){
      grup_var.r1<-as.character(grup_var.r1)
    }
    #if(TYPE.r1!="sous.seq"){
    lapply(1:length(unique(grup_var.r1)), FUN = function(i){
      objseq.r1[grup_var.r1==unique(grup_var.r1)[i] , ]->objseq.r1.i
      objseq.r1.i[!is.na(objseq.r1.i[ , 1]) , ]->objseq.r1.i
      
      if(TYPE.r1!="txtr"){
        
        DONNEES_POUR_PLOT(TYPE=TYPE.r1, objseq = objseq.r1.i, arrondi=2,pmin.sup=pmin.sup1, STR.SUBS=STR.SUBS.1, col.selected = col_selected.r1, 
                          PAS.temps=PAS.temps1, TIME.varying=TIME.varying1, Pourc.eff=Pourc.eff1, Sens=Sens1)->dats
      } else {
        seqtrate(seqdata = objseq.r1.i, time.varying = TIME.varying1, lag = PAS.temps1 , count = Pourc.eff1 )->tr.tx
        data.frame(as.table(tr.tx))->dfres
        names(dfres)[names(dfres)=="Var1"]<-"Départ"
        names(dfres)[names(dfres)=="Var2"]<-"Arrivée"
        names(dfres)[names(dfres)=="Var3"]<-"Date.départ"
        dfres->dats
        
      }
      as.data.frame(dats)->dats
      dats$level<-unique(grup_var.r1)[i]
      dats$ID<-row.names(dats)
      return(dats)
    })->lidat
    do.call("rbind", lidat)->dats
    
    if(TYPE.r1=="sous.seq"){
     # message("debug1")
      seqecreate(objseq.r1)->seqe2
      if(is.null(STR.SUBS.1)){
      seqefsub(seqe2,pmin.support =pmin.sup1,  #str.subseq = unique(dats$event), 
               max.k = max.k1)->sub2
      } else {
        seqefsub(seqe2, str.subseq = STR.SUBS.1, max.k = max.k1, pmin.support =pmin.sup1)->sub2
      }
      print(sub2)
      print(seqe2)
      
      if(is.null(grup_var.r1)){
        dats$CodeValue<-0
      } else {
      if(length(unique(grup_var.r1))>1){
        message("DON'T YOU ROCK MY BOAT")
      seqecmpgroup(subseq = sub2, group = grup_var.r1, method = "bonferroni")->discr2
      cbind("subseq"=as.character(discr2$subseq),  discr2$data)->datdiscr2
      reshape(datdiscr2, direction = "long", varying = list(colnames(datdiscr2)[grepl("Resid.", colnames(datdiscr2), fixed = TRUE)]), 
              times = colnames(datdiscr2)[grepl("Resid.", colnames(datdiscr2), fixed = TRUE)])->datdiscr2
      datdiscr2$CodeValue<-sapply(1:nrow(datdiscr2), function(i){
        if(datdiscr2$p.value[i]<0.1){
          if(datdiscr2$p.value[i]>0.05){
            1} else {
              if(datdiscr2$p.value[i]>0.01){
                2} else {
                  3
                }
            }
        } else {0}
      })
      datdiscr2$CodeValue<-sapply(1:nrow(datdiscr2), function(i){
        if(datdiscr2[ , grepl("Resid.", colnames(datdiscr2), fixed = TRUE)][i]<0){
          -datdiscr2$CodeValue[i]
        } else {
          datdiscr2$CodeValue[i]
        }
      })
      datdiscr2$time<-gsub(pattern = "Resid.", replacement = "", fixed = TRUE, x = datdiscr2$time)
      
      if(length(intersect(dats$event, datdiscr2$subseq))==length(unique(dats$event))){
        message("debug2")
        
        left_join(dats, datdiscr2[, c("subseq", "time", "CodeValue")], by=c("event"="subseq", "level"="time"))->dats
      } else {
        dats$CodeValue<-0
      }
      } else {
        dats$CodeValue<-0
      }
}
    }
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
      theme_excel(base_size = 18)+
      xlab(label = "")+ylab(label = "")+
      theme(axis.title = element_blank(), axis.text.x =  element_text(angle = 45) )+#,legend.text = element_text(size=2) )+
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
        theme_excel(base_size = 18)+
        xlab(label = "")+ylab(label = "")+
        theme(axis.title = element_blank() , axis.text.x =  element_text(angle = 45))+#, legend.text = element_text(size=2))+
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
          theme_excel(base_size = 18)+
          xlab(label = "")+ylab(label = "")+
          theme(axis.title = element_blank() , axis.text.x =  element_text(angle = 45, hjust = 1))+#, legend.text = element_text(size=2))+
          facet_wrap(.~level)
        
        
      } else {
        if(TYPE.r1=="flux"){
          graph_flux_grp(seq_data = objseq.r1, col_periode = col_selected.r1, group_var = grup_var.r1)#, merge_mods = merge_mods.r1)
        } else {
          if(TYPE.r1=="sous.seq"){
            #dats$Support<-dats$Support/100
            log(length(unique(dats$level)))->size.effect
            if(size.effect==0){size.effect<-1}
            #dats<-dats[!duplicated(dats$event) , ]
            
            dats$event<-factor(dats$event, levels = as.character(sub2$subseq[order(sub2$data$Support, decreasing = TRUE)]), ordered = TRUE)
            brewer.pal(name = "Spectral", n = 8)->colpal
            names(colpal)<-as.character(c(-3, -2, -1, 0, 1, 2, 3))
            factor(dats$CodeValue)->dats$CodeValue
            dats<-filter(dats, !is.na(event))
              gg<-ggplot(data=dats,aes(x=event,y=Support, fill= CodeValue)) +
                geom_bar(stat='identity',width=0.9)+
                geom_text(aes(label=paste(round(as.numeric(Support),2), "%", sep="")),
                           hjust=-1, colour=gray(0.3), size=5/size.effect)+
                scale_fill_manual(values = colpal)+
                theme_hc(base_size = 18)+
                xlab('')+
                theme(legend.position = "none", axis.text.x = element_text(size=9, angle=90, hjust=1,vjust = 0.3),
                      axis.title.x = element_blank(),
                      plot.title = element_text(hjust = 0.5,size=18,face = "bold"),
                      plot.subtitle = element_text(hjust = 0.5,size=14))+ylim(0,100)+
                coord_flip()+
                facet_wrap(.~level, ncol = 2)
              return(gg)
              
          } else {
            if(TYPE.r1=="txtr"){
              
              
              
              log(length(unique(dats$level)))->size.effect
              if(size.effect==0){size.effect<-1}
              if(Pourc.eff1==TRUE){
                dats$LABS<-dats$Freq
              } else {
                dats$LABS<-paste(round(dats$Freq*100, 1), "%", sep="")
              }

              gp<-ggplot(data = dats, aes(x=Arrivée, y=Départ, 
                                      #colour=Freq, 
                                      fill=Freq))+
                #geom_point(shape=21, size=18, alpha=0.6)+
                geom_raster(alpha=0.7, interpolate = FALSE)+
                
                geom_text(aes(label=LABS), 
                          family="Calibri Light", size=4.75/size.effect)+
                scale_colour_distiller(palette = "Greys", direction = -1)+
                scale_fill_viridis_c(direction = -1)+
                theme_light(base_family = "Calibri Light", base_size = 18)+
                theme(legend.position = "none", axis.title.y = element_text(angle = 0, vjust = 0.5))+
                scale_x_discrete(position = "top", labels = function(x) { 
                  gsub(pattern ="[->", replacement = "", x = x , fixed = TRUE)->res
                  gsub(pattern ="]", replacement = "", x = res, fixed = TRUE )->res
                  return(res)
                }) +
                scale_y_discrete(position = "left", 
                                 limits = rev(levels(dats$Départ)),
                                 labels = function(x) { 
                                   gsub(pattern ="->]", replacement = "", x = x , fixed = TRUE)->res
                                   gsub(pattern ="[", replacement = "", x = res, fixed = TRUE )->res
                                   return(res)
                                 }) +
                ylab(sprintf("Départ \u279C"))+xlab(sprintf("\u279C Arrivée"))
                #,legend.text = element_text(size=2) )+
                if(TIME.varying1==TRUE){
                  gp<-gp+facet_grid(rows = vars(Date.départ), cols = vars(level))
                } else {
                  gp<-gp+facet_wrap(.~level)
                }
              
            } else {
            
            
    seqplot(seqdata =  objseq.r1, type=TYPE.r1, group = grup_var.r1, sortv=SORTV1)
            p <- recordPlot()
            plot.new() 
            p 
          }
          }
        }
    }}
  }
  #return(p)
  }
  