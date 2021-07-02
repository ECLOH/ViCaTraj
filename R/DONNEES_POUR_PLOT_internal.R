#' @export

DONNEES_POUR_PLOT.internal<-function(TYPE.r1=TYPE, objseq.r1=objseq, arrondi=2, 
                                     col.selected.r1=NULL, pmin.sup1=0.05, STR.SUBS.1=NULL, max.k1=2,
                                     PAS.temps1=PAS.temps, TIME.varying1=TIME.varying, Pourc.eff1=Pourc.eff, Sens1=Sens){
  
  if(!is.null(col.selected.r1)){
    objseq.r1[ , which(names(objseq.r1)%in%col.selected.r1)]->objseq.r1
  }
  
  tye<-TYPE.r1
  as.data.frame(matrix(data = c(
    "d", "seqstatd(objseq.r1)->objdat;round(objdat$Frequencies*100, arrondi)",
    "f", "seqtab(objseq.r1)->objdat;round(attributes(objdat)$freq, arrondi)",
    "I", "seqtab(objseq.r1)->objdat;round(attributes(objdat)$freq, arrondi)",
    "Ht", "seqstatd(objseq.r1)->objdat;objdat$Entropy",
    "ms", "seqmodst(objseq.r1)",
    "mt", "round(seqmeant(objseq.r1), arrondi)",
    "r", "seqrep(objseq.r1)",
    "txtr", 'if(Sens1==TRUE){
    objseq.r1<-objseq.r1[ , ncol(objseq.r1):1]
    };
    seqtrate(seqdata = objseq.r1, time.varying = TIME.varying1, lag = PAS.temps1 , count = Pourc.eff1 )->tr.tx;
    if(Pourc.eff1==FALSE){
    tr.tx<-round(tr.tx*100, 2)
    }
    namescol<-colnames(tr.tx)

    if(class(tr.tx)=="array"){
        lapply(seq(1,dim(tr.tx)[3]), function(x){
        tr.tx[ , , x]->res
        res<-as.data.frame.matrix(res)
        res$DATE<-attributes(tr.tx)$dimnames[[3]][x]
        rownames_to_column(res, var="Départ")->res
        return(res)
        })->list.trate
      } else {
      res<-as.data.frame.matrix(tr.tx)
      res$DATE<-"Taux de transition moyens"
      rownames_to_column(res, var="Départ")->res
      res->list.trate
      }
      if(inherits(x = list.trate, what = "list")){
      do.call("rbind", list.trate)->list.trate
      }
      list.trate$Départ<-gsub("[", "", list.trate$Départ, fixed=TRUE)
      list.trate$Départ<-gsub("->]", "", list.trate$Départ, fixed=TRUE)
      list.trate$Départ<-trimws(list.trate$Départ,"both")

       names(list.trate)<-gsub("]", "", names(list.trate), fixed=TRUE)
       names(list.trate)<-gsub("[->", "", names(list.trate), fixed=TRUE)
       names(list.trate)<-trimws(names(list.trate),"both")
       names(list.trate)[!names(list.trate)%in%c("Départ", "DATE")]<-paste("Arrivée_", 
       names(list.trate)[!names(list.trate)%in%c("Départ", "DATE")],  
       sep="")
       
       list.trate
    ',
    "sous.seq", "seqecreate(objseq.r1)->seqe.obj;
    if(is.null(STR.SUBS.1)){
        seqefsub(seqe.obj, pmin.support=pmin.sup1, max.k = max.k1)->seqe.stat
        seqe.stat$data$Support<-round(seqe.stat$data$Support*100, 2)
    cbind('event'=as.character(seqe.stat$subseq), seqe.stat$data)->res
    } else {
    sapply(STR.SUBS.1,  function(str.x){sum(grepl(pattern = str.x, x = seqe.obj))})->str.control
    STR.SUBS.1[str.control>0]->STR.SUBS.1.control
    STR.SUBS.1[str.control<1]->STR.SUBS.1.NOcontrol
    if(length(STR.SUBS.1.control)>0){
      seqefsub(seqe.obj, pmin.support=pmin.sup1, str.subseq=STR.SUBS.1.control, max.k = max.k1)->seqe.stat
    seqe.stat$data$Support<-round(seqe.stat$data$Support*100, 2)
    data.frame(cbind('event'=as.character(seqe.stat$subseq), seqe.stat$data), stringsAsFactors = FALSE)->restemp
    }
    if(length(STR.SUBS.1.NOcontrol)>0){
    data.frame(do.call('rbind',  lapply(STR.SUBS.1.NOcontrol, function(nox){c('event'=nox, 'Support'=0, 'Count'=0)})),   stringsAsFactors = FALSE)->restemp.NO
    restemp.NO$Support<-as.numeric(as.character(restemp.NO$Support))
    restemp.NO$Count<-as.numeric(as.character(restemp.NO$Support))

    }
    if(length(STR.SUBS.1.control)>0&length(STR.SUBS.1.NOcontrol)>0){
    res<-rbind(restemp, restemp.NO)
    } else {
        if(length(STR.SUBS.1.control)<1&length(STR.SUBS.1.NOcontrol)>0){
        res<-restemp.NO
        } else {
        if(length(STR.SUBS.1.control)>0&length(STR.SUBS.1.NOcontrol)<1){
        res<-restemp
        } 
        }
    }
    }
    res$Support<-as.numeric(as.character(res$Support))
    res$Count<-as.numeric(as.character(res$Support))
    res",
    "flux", "seqstatd(objseq.r1)->objdat;round(objdat$Frequencies*100, arrondi)->res1;

as.data.frame.array(round(seqtrate(seqdata = objseq.r1, time.varying = TRUE)*100, arrondi))->res2;

colnames(res1)<-paste(colnames(res1), 'repartition', sep='');
colnames(res2)<-paste(colnames(res2), 'transition', sep='');

reshape(res2, varying = list(names(res2)), direction='long', ids = row.names(res2), times=names(res2))->res2df
res2df$id%>%
  gsub(pattern = ' ->]', replacement = '', fixed = TRUE)%>%
  gsub(pattern = '[', replacement = '', fixed = TRUE)->res2df$FROM

res2df$time%>%
  gsub(pattern = '[-> ', replacement = '', fixed = TRUE)%>%
  gsub(pattern = ']', replacement = '', fixed = TRUE)%>%
  gsub(pattern = 'transition', replacement = '', fixed = TRUE)->res2df$TO.G
res2df$SEQTIME<-sapply(1:nrow(res2df), function(i){strsplit(res2df$TO.G[i], split = '.', fixed=TRUE)[[1]][2]})
res2df$TO<-sapply(1:nrow(res2df), function(i){strsplit(res2df$TO.G[i], split = '.', fixed=TRUE)[[1]][1]})

names(res2df)[!names(res2df)%in%c('time' , 'id',  'FROM' , 'TO.G'  , 'SEQTIME', 'TO')]<-'Taux.transition'

t(res1)->res1bis;
colnames(res1bis)->save.colnames;
data.frame(res1bis)->res1bis;
names(res1bis)<-save.colnames;
reshape(res1bis, direction='long', varying = list(names(res1bis)), times = names(res1bis), ids = row.names(res1bis) )->res1bis.long;
res1bis.long$SEQTIME<-gsub(pattern = 'repartition', replacement = '', fixed = TRUE, x = res1bis.long$id);
names(res1bis.long)[!names(res1bis.long)%in%c('time', 'id', 'SEQTIME')]<-'Fréquences';
names(res1bis.long)[names(res1bis.long)==c('time')]<-'FROM';


left_join(res1bis.long, res2df, by=c('SEQTIME', 'FROM'))->resG;
resG[ , c('SEQTIME' , 'FROM', 'TO', 'Fréquences', 'Taux.transition')]->resG;
names(resG)[names(resG)=='time']<-'Etats';
names(resG)[names(resG)=='FROM.y']<-'FROM';
resG"
  ), ncol=2, byrow = TRUE), stringsAsFactors = FALSE)->df
  
  names(df)<-c("TYPE" , "EXPR")
  
  eval(parse(text =   df[df$TYPE==tye , ]$EXPR ) )->res
  
  return(res)
}