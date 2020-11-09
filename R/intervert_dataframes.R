#' intervert_df() 
#' 
#' intervert_df() insert dans list.output les data.frames choisies dans list.input selon les equivalences de nom spécifiées
#' 
#' @export
#' @examples 
#' library(ViCaTraj)
#' data("EXAMPLE_LIST")
#' res->res.o
#' rm(res)
#' res.i<-lapply(res.o, function(x){mtcars})
#' names(res.i)<-paste0(names(res.i), "_i")
#' names(res.o)<-paste0(names(res.o), "_o")
#' res.3<-intervert_df(list.output=res.o, list.input=res.i, equivalences=c("a15_o"="a15_i", "a16_o"="a16_i", "a22_o"="a22_i"))
intervert_df<-function(list.output=res.o, list.input=res.i, 
                               equivalences=c("a15_o"="a15_i", "a16_o"="a16_i", "a22_o"="a22_i")){
  for(x in seq_along(equivalences)){
    list.output[[ names(equivalences)[x] ]]<-list.input[[ equivalences[[x]] ]]
  }
  return(list.output)
}
  