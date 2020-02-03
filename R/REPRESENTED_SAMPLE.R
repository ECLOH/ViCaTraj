#' @export

REPRESENTED_SAMPLE<-function(interact.var, SIZE, id.var){
  if(!is.null(interact.var)){
  data.frame(
    "INTERACTION"=interact.var,
    "IDROW"=1:length(interact.var)
  )->df.interact
  prop.table(table(df.interact$INTERACTION))->props
  df.interact$PROPS<-sapply(1:nrow(df.interact), FUN = function(i){
    props[df.interact$INTERACTION[i]]
  })
  df.interact$PROPS[is.na(df.interact$PROPS)]<-0
  sample(x = df.interact$IDROW, size = SIZE, replace = FALSE, prob = df.interact$PROPS)->SAM1
  } else {
    sample(x = 1:nrow(data), size = SIZE, replace = FALSE)->SAM1
  }
  
  if(is.null(id.var)){
    SAMres<-SAM1
  } else {
    SAMres<-id.var[SAM1]
  }
  return(SAMres)
}