# En cours
seqggplot<-function(TYPE="d", objseq = actcal.seq){
  
  attributes(objseq)$cpal->col
  names(col)<-attributes(objseq)$alphabet
  
  DONNEES_POUR_PLOT(type=TYPE, objseq = objseq)->dats
  as.data.frame(dats)->dats
  
  if(TYPE=="d"){
   reshape(dats, direction='long', varying = list("VAR"=names(dats)), times = names(dats), ids = row.names(dats))->ggdats
   names(ggdats)<-c("time", "VAR", "id")
      p<-ggplot(ggdats, aes(x=time, y=VAR, fill=id))+
       geom_bar(stat = "identity" )+
        geom_text(aes(label=VAR), stat="identity", position=position_stack(vjust = 0.5), size=2)+
        scale_fill_manual(values = col, name="États : ")+
        theme_hc()+
        xlab(label = "")+ylab(label = "")+
        theme(axis.title = element_blank() )
      
  } else {
    p<-seqplot(seqdata =  objseq, type=TYPE)
  }
  
  # if(type=="f"){
  #   strsplit(row.names(dats), split = "-")->lio
  #   lapply(lio, function(xlo){
  #     if(length(xlo)==1){
  #       strsplit(xlo, split = "/")->xlo.2
  #       rep(x = xlo.2[[1]][1], times=xlo.2[[1]][[2]])
  #     } else {
  #     unlist(
  #     sapply(xlo, function(vio){
  #       strsplit(vio, split = "/")->lio.2
  #       rep(x = lio.2[[1]][1], times=lio.2[[1]][[2]])
  #     }), use.names = FALSE
  #     )
  #     }
  #   })->lio2
  #   names(lio2)<-row.names(dats)
  #   data.frame(do.call("rbind", lio2), stringsAsFactors = FALSE)->lio2
  #   merge(lio2, dats, by="row.names")->ggdats
  #   ggdats[order(ggdats$Freq, decreasing = TRUE) , ]->ggdats
  #   reshape(ggdats, varying = list(names(lio2)), 
  #           times = attributes(objseq)$names, 
  #           direction="long")->ggdats
  #   ggdats$time<-factor(x =  ggdats$time, levels =attributes(objseq)$names,  ordered = TRUE)
  #   names(ggdats)[names(ggdats)=="Row.names"]<-"Seq_name"
  #   ggdats$Seq_name<-gsub(ggdats$Seq_name, pattern = "/", replacement = "")
  #   ggdats$Seq_name<-gsub(ggdats$Seq_name, pattern = "-", replacement = "", fixed = TRUE)
  #   ggdats$Seq_name<-as.character(ggdats$Seq_name)
  #   
  #   
  #   #ggplot(data=ggdats, aes(x=id , y = id, fill=X1))+
  #   #  geom_bar(stat="identity")+
  #   #  coord_flip()
  #   
  #   #ggdats$id<-factor(as.character(ggdats$id), levels = 
  #   
  #   p<-ggplot(ggdats, aes(x =  Seq_name, y=time, 
  #                         fill=X1, width=Freq )
  #             )+#abs(log(Percent)))) +
  #     geom_tile()+
  #     scale_fill_manual(values = col, name="États : ")+
  #     #scale_
  #     #scale_y_continuous()
  #     #scale_size(limits = c(2, 3))+
  #     theme_hc()+
  #     #xlab(label = "")+ylab(label = "")+
  #     theme(axis.title = element_blank() )
  # }
  return(p)
}
