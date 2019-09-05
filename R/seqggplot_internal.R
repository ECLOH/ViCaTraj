seqggplot.internal<-function(objseq.r1 = objseq, TYPE.r1=TYPE){
  attributes(objseq.r1)$cpal->col
  names(col)<-attributes(objseq.r1)$alphabet
  
  DONNEES_POUR_PLOT(TYPE=TYPE.r1, objseq = objseq.r1, arrondi=2)->dats
  as.data.frame(dats)->dats
  
  if(TYPE.r1=="d"){
    reshape(dats, direction='long', varying = list("VAR"=names(dats)), times = names(dats), ids = row.names(dats))->ggdats
    names(ggdats)<-c("time", "VAR", "id")
    ggplot(ggdats, aes(x=time, y=VAR, fill=id))+
      geom_bar(stat = "identity" )+
      geom_text(aes(label=VAR), stat="identity", position=position_stack(vjust = 0.5), size=3)+
      scale_fill_manual(values = col, name="États : ")+
      theme_hc()+
      xlab(label = "")+ylab(label = "")+
      theme(axis.title = element_blank() )
    
  } else {
    seqplot(seqdata =  objseq.r1, type=TYPE.r1)
  }
  #return(p)
}