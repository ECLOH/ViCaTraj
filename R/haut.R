#' @export 

haut1<-function(nb.gr = NULL, 
                type.of.plot= "sous.seq", 
                nb.time=NULL){
  if(!is.null(nb.time)){
    nb<-nb.time
  } else {
    nb<-nb.gr
  }
  if(nb<=1){
    nb.log<-1
  } else {
    nb.log<-log(nb)
  }
  
  if (type.of.plot %in% c("sous.seq","sous.seq.ch")) {
    base.size<-800
  } else {
    if(!is.null(nb.time)){
      base.size<-700
    } else {
    base.size<-500
    }
  }
  return(nb.log*base.size)
}
