#' @export
table.joint<-function(data1, var1, data2, var2, BY=c("ID"="ID"), data3=NULL, var3=NULL, prop=FALSE, prop.margin=1){
  
  if(length(BY)==1){
    globy<-BY
  } else {
  globy<-c(unique(unlist(BY, use.names = FALSE)), names(BY))
  }
  
  data1[ , c(globy[globy%in%names(data1)], var1)]->data1
  data2[ , c(globy[globy%in%names(data2)], var2)]->data2
  
  paste(var1, "1", sep = ".")->nam1
  paste(var2, "2", sep = ".")->nam2
  names(data1)[names(data1)==var1]<-nam1
  names(data2)[names(data2)==var2]<-nam2
  
  if(!is.null(data3)){
    data3[ , c(globy[globy%in%names(data3)], var3)]->data3
    paste(var3, "3", sep = ".")->nam3
    names(data3)[names(data3)==var3]<-nam3
  }

  message("print(head(data1))")
  print(head(data1))
  message("print(head(data2))")
  print(head(data2))
  message("BY:")
  print(BY)
  message("BY in  data1")
  print(BY%in%names(data1))
  message("BY in  data2")
  print(BY%in%names(data2))
  #full_join(data1, data2, by=BY)->data12
  base::merge(x = data1, y = data2, all=TRUE)->data12
  if(!is.null(data3)){
    #full_join(data12, data3, by=BY)->data123
    base::merge(x = data12,y = data3, all=TRUE)->data123
    
  }
  
  if(is.null(data3)){
    table(var1=data12[ , nam1], var2=data12[ , nam2], exclude = NULL)->tab
    if(prop==TRUE){
      round(prop.table(tab, margin = prop.margin)*100, 2)->tab
      if(prop.margin==1){
        tab<-cbind(tab, "Ensemble"=rowSums(tab))
      }
      if(prop.margin==2){
        tab<-rbind(tab, "Ensemble"=colSums(tab))
      }
    } else {
      tab<-rbind(tab, "Ensemble"=colSums(tab))
      tab<-cbind(tab, "Ensemble"=rowSums(tab))
    }
  } else {
    data123[ , nam1]<-as.factor(data123[ , nam1])
    data123[ , nam2]<-as.factor(data123[ , nam2])
    
    lapply(unique(data123[ , nam3]), function(lev3){
      subset(data123, data123[ , nam3]==lev3)->df3
      table(var1=df3[ , nam1], var2=df3[ , nam2], exclude = NULL)->tab
      if(prop==TRUE){
        round(prop.table(tab, margin = prop.margin)*100, 2)->tab
        if(prop.margin==1){
          tab<-cbind(tab, "Ensemble"=rowSums(tab))
        }
        if(prop.margin==2){
          tab<-rbind(tab, "Ensemble"=colSums(tab))
        }
      } else {
        tab<-rbind(tab, "Ensemble"=colSums(tab))
        tab<-cbind(tab, "Ensemble"=rowSums(tab))
      }
      tab
    })->tab
    names(tab)<-unique(data123[ , nam3])
  }
  return(tab)
}
