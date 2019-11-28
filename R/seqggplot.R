# En cours
#' @export

seqggplot<-function(TYPE="d", objseq = actcal.seq, groupes=NULL, merge_mods, col.selected=c("1", "2")){
  
  if(sum(class(objseq)!=c("stslist", "data.frame"))==0){
    #p<-
      seqggplot.internal(objseq.r1 = objseq, TYPE.r1=TYPE, grup_var.r1=groupes, merge_mods.r1 = merge_mods, col_selected.r1=col.selected)
  } else {
    if(class(objseq)=="list"){
     # p<-
     lapply(1:length(objseq) , function(i){
       obji<-objseq[[i]]
       if(is.null(names(objseq))){
         nami<-i
       } else {nami <- names(objseq)[i] }
        seqggplot.internal(objseq.r1 = obji, TYPE.r1=TYPE, grup_var.r1=groupes)+ggtitle(label = nami)
      })
    } else {
      NULL
    }
  }
  #return(p)
}
