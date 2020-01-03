#' @export

data_discr<-function(dataseq, groups ){
  
}



# subs<-reactive({
#   if (req(input$plottypeG)=="Pearson"){
#     req(seq.select2(),dataCluster(),valuesG$df)
#     return(seqefsub(seqecreate(seq.select2(), tevent="state", use.labels=FALSE),pmin.support=input$pmin))
#   }
#   if (req(input$plottypeG) == "Pearson.ch"){
#     req(seq.select2(),dataCluster(),valuesG$df)
#     if(nrow(valuesG$df)>0){
#       unique(c(unique(valuesG$df[,1]),unique(valuesG$df[,2]),unique(valuesG$df[,3])))->valCh
#       valCh[valCh!="Aucun"]->valCh
#       seqecreate(seq.select2(), tevent="state", use.labels=FALSE)->seqGlobal22
#       
#       if(all(valCh %in% alphabet(seqGlobal22))){
#         vectSeqG<-vect.sous.seq(data = valuesG$df)
#         seqefsub(seqGlobal22,str.subseq=vectSeqG)->p22
#         return(p22[order(p22$data$Support,decreasing = TRUE),])
#       }else{
#         valCh[!(valCh %in% alphabet(seqGlobal22))]->valnonalphabet
#         valuesG$df<-valuesG$df[which(!(valuesG$df[,1] %in% valnonalphabet | valuesG$df[,2] %in% valnonalphabet | valuesG$df[,3] %in% valnonalphabet)),]
#         if(nrow(valuesG$df)>0){
#           vectSeqG<-vect.sous.seq(data = valuesG$df)
#           seqefsub(seqGlobal22,str.subseq=vectSeqG)->p22
#           return(p22[order(p22$data$Support,decreasing = TRUE),])
#         }
#       }
#     }
#     
#   }
# })
# 
# discr<-reactive({
#   if (req(input$plottypeG) %in% c("Pearson","Pearson.ch")){
#     req(subs(),data.select2())
#     if(nrow(valuesG$df)>0){
#       seqecmpgroup(subs() , group=data.select2()[,"Clustering"])
#     } 
#   }
# })
# 
# output$alpabeltTexte<-renderUI({
#   output$TexteAlpha<-renderText({
#     if (req(input$plottypeG) == "Pearson.ch"){
#       req(seq.select2(),dataCluster(),valuesG$df)
#       seqecreate(seq.select2(), tevent="state", use.labels=FALSE)->seqGlobalText
#       return(paste("Selectionnez des états se trouvant dans la liste suivante :",paste(alphabet(seqGlobalText),collapse = ", ")))
#     }
#   })
#   return(textOutput("TexteAlpha"))
# })
# 
# 
# observe({
#   req(seq.select2(),dataCluster(),ordre())
#   if (req(input$plottypeG) == "Pearson"){
#     #Pour la comparaison des sous-populations, on met les graphiques dans une liste#
#     if (input$souspop2!="Aucune" && (is.factor(dataCluster()[,input$souspop2]))){#|is.character(dataCluster()[,input$souspop2])) ) {
#       req(input$souspop_modalite2)
#       tailleGraph$height<-dim(ordre())[1]*400
#       lapply(1:length(input$souspop_modalite2), FUN=function(i){
#         paste0('SEQPLOTPEARSON', i)->id.output
#         output[[id.output]] <- renderPlot({
#           if (req(input$plottypeG) == "Pearson"){
#             if (input$souspop2!="Aucune" && (is.factor(dataCluster()[,input$souspop2]))){#|is.character(dataCluster()[,input$souspop2])) ) {
#               if(i<=length(input$souspop_modalite2)){
#                 req(input$nbAffiche)
#                 seq.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],]->seqSouspop2
#                 seqecreate(seqSouspop2, tevent="state", use.labels=FALSE)->seqGlobal2
#                 # titre<-paste("Graphique des sous-séquneces \n pour la variable",input$souspop2,"\n avec la modalité",input$souspop_modalite1)
#                 # sousTitre<-paste("Il y a",nrow(seqSouspop),"individus")
#                 seqefsub(seqGlobal2,pmin.support=input$pmin)->p2
#                 return(plot(seqecmpgroup(p2 , group=data.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],"Clustering"])[1:input$nbAffiche]))
#               }
#             }
#           } 
#         },height = haut(),width = 1300)
#       })
#     } else {
#       output$SEQPLOTPEARSON<-renderPlot({
#         if (req(input$plottypeG) == "Pearson"){
#           if (input$souspop2=="Aucune" || is.numeric(dataCluster()[,input$souspop2])) {
#             req(input$nbAffiche)
#             tailleGraph$height<-dim(ordre())[1]*400
#             seqecreate(seq.select2(), tevent="state", use.labels=FALSE)->seqGlobal2
#             seqefsub(seqGlobal2,pmin.support=input$pmin)->p2
#             return(plot(seqecmpgroup(p2 , group=data.select2()[,"Clustering"])[1:input$nbAffiche]))
#           }
#         }  
#       },height = haut(),width = 1300)
#       
#     }
#   }else{
#     ## Cas où l'utilisateur choisi les sous-séquences ##
#     if (req(input$plottypeG) == "Pearson.ch"){
#       req(valuesG$df)
#       #condition d'un data.frame values non vide pour exécuter la suite du code afin de ne pas avoir d'erreur quand la data.frame est vide
#       if(nrow(valuesG$df)>0){
#         unique(c(unique(valuesG$df[,1]),unique(valuesG$df[,2]),unique(valuesG$df[,3])))->valCh
#         valCh[valCh!="Aucun"]->valCh
#         if (input$souspop2!="Aucune" && (is.factor(dataCluster()[,input$souspop2]))){#|is.character(dataCluster()[,input$souspop2])) ) {
#           req(input$souspop_modalite2)
#           tailleGraph$height<-dim(ordre())[1]*400
#           lapply(1:length(input$souspop_modalite2), FUN=function(i){
#             paste0('SEQPLOTPEARSONCH', i)->id.output
#             output[[id.output]] <- renderPlot({
#               if (req(input$plottypeG) == "Pearson.ch"){
#                 if (input$souspop2!="Aucune" && (is.factor(dataCluster()[,input$souspop2]))){#|is.character(dataCluster()[,input$souspop2])) ) {
#                   if(i<=length(input$souspop_modalite2)){
#                     seq.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],]->seqSouspop2
#                     seqecreate(seqSouspop2, tevent="state", use.labels=FALSE)->seqGlobal2
#                     if(all(valCh %in% alphabet(seqGlobal2))){
#                       if(nrow(valuesG$df)>0){
#                         vectSeq2<-vect.sous.seq(data = valuesG$df)
#                         seqefsub(seqGlobal2,str.subseq=vectSeq2)->p2
#                         return(plot(seqecmpgroup(p2[order(p2$data$Support,decreasing = TRUE),] , group=data.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],"Clustering"])))
#                       }
#                       
#                     }else{
#                       valCh[!(valCh %in% alphabet(seqGlobal2))]->valnonalphabet
#                       valuesG$df<-valuesG$df[which(!(valuesG$df[,1] %in% valnonalphabet | valuesG$df[,2] %in% valnonalphabet | valuesG$df[,3] %in% valnonalphabet)),]
#                       if(nrow(valuesG$df)>0){
#                         vectSeq2<-vect.sous.seq(data = valuesG$df)
#                         seqefsub(seqGlobal2,str.subseq=vectSeq2)->p2
#                         return(plot(seqecmpgroup(p2[order(p2$data$Support,decreasing = TRUE),] , group=data.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],"Clustering"])))
#                       }
#                     }
#                   } 
#                 }
#               } 
#             },height = haut(),width = 1300)
#           })
#         } else {
#           output$SEQPLOTPEARSONCH<-renderPlot({
#             if (req(input$plottypeG) == "Pearson.ch"){
#               if (input$souspop2=="Aucune" || is.numeric(dataCluster()[,input$souspop2])) {
#                 if(nrow(valuesG$df)>0){
#                   tailleGraph$height<-dim(ordre())[1]*400
#                   seqecreate(seq.select2(), tevent="state", use.labels=FALSE)->seqGlobal2
#                   vectSeq2<-vect.sous.seq(data = valuesG$df)
#                   seqefsub(seqGlobal2,str.subseq=vectSeq2)->p2
#                   return(plot(seqecmpgroup(p2[order(p2$data$Support,decreasing = TRUE),] , group=data.select2()[,"Clustering"])))
#                 }
#               }   
#             }
#           },height = haut(),width = 1300)
#         }
#       }
#     }
#   }
# })
# 
# 
# 
# observe({
#   if(nrow(valuesG$df)>0){
#     req(discr())
#     updateNumericInput(session = session,inputId = "nbAffiche",max=nrow(discr()$data))
#   }else{
#     updateNumericInput(session = session,inputId = "nbAffiche",max=1)
#   }
# })
# 
# 
# #### Choix de sous-séqueneces ####
# 
# ### Mise a jour des inputs permettant de choisir des états ###
# observe({
#   input$plottypeG
#   isolate({
#     if (req(input$plottypeG)=="Pearson.ch"){
#       req(seq.select2())
#       updateSelectInput(session = session,inputId = "par.sous.seq1G",choices = alphabet(seq.select2()))
#       updateSelectInput(session = session,inputId = "par.sous.seq2G",choices = alphabet(seq.select2()))
#       updateSelectInput(session = session,inputId = "par.sous.seq3G",choices = cbind("Aucun",alphabet(seq.select2())))
#     }
#   })
# })
# 
# observe({
#   updateNumericInput(session = session,inputId = "ligne.supprG",max=nrow(valuesG$df))
# })
# 
# valuesG <- reactiveValues()
# valuesG$df <-  as.data.frame(setNames(replicate(3,character(0), simplify = F),c("Etat1","Etat2","Etat3") ))
# 
# observeEvent(input$add.buttonG,{
#   req(input$par.sous.seq1G,input$par.sous.seq2G)
#   newRow <- data.frame(input$par.sous.seq1G, input$par.sous.seq2G,input$par.sous.seq3G)
#   colnames(newRow)<-colnames(valuesG$df)
#   valuesG$df <- rbind(valuesG$df,newRow)
#   rownames(valuesG$df)<-(1:nrow(valuesG$df))
# })
# 
# observeEvent(input$delete.buttonG,{
#   if(nrow(valuesG$df)>1){
#     valuesG$df[!(vect.sous.seq(valuesG$df) %in% as.character(subs()$subseq)[input$ligne.supprG]),]->valuesG$df
#     rownames(valuesG$df)<-(1:nrow(valuesG$df))
#   }else {
#     valuesG$df <- valuesG$df[-nrow(valuesG$df), ]
#   }
# })  
# observe({
#   req(valuesG$df)
#   valuesG$df<-unique(valuesG$df)
# })