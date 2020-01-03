#' @export 

haut1<-function(nb = 2, type.of.plot= "sous.seq"){
  if(nb<=1){
    nb.log<-1
  } else {
    nb.log<-log(nb)
  }
  
  if (type.of.plot %in% c("sous.seq","sous.seq.ch")) {
    base.size<-1000
  } else {
    base.size<-500
  }
  
    return(nb.log*base.size)
}
