#' @title   palettes
#' @export

palettes<-function(levels_type="RSA_simple"){
  
  pal_RSA_simple<-c(
    "RSA"="#440154FF",
    "SUSPENDU"="#21908c",#  "#3B528BFF", 
    #"#21908CFF" 
    "AVANT RSA"="#ccccb3",
    "SORTIE TEMPORAIRE"="#5DC863FF" ,
    "SORTIE DURABLE"="#FDE725FF")
  
  if(levels_type=="RSA_simple"){
    res<-pal_RSA_simple
  }
  
  return(res)

}
