#### DATA ####
#data("trajs")
#trajs<-read.csv("G:/dgsd/Dopro/Projets/2018/E - Etude_RSA/E-RSA_Appli_Shiny_Trajectoires/GitHub/ViCaTraj-master/data/trajs.csv",row.names = 1)

# rev(wesanderson::wes_palette(name = "Darjeeling1", n = length(alphabet(trajs)), type = "discrete"))->cpal.seq
# cpal(seqdata = trajs)<-cpal.seq
# seqtab(trajs[ , ], idxs = 0, format = "STS")->unique.trajs

#### SERVER ####
server <- function(input, output, session) {

  
  library(shiny)
  library(TraMineR)
  library(ggplot2)
  library(TraMineRextras)
  library(RColorBrewer)
  library(DT)
  library(RColorBrewer)
  library(wesanderson)
  library(shinythemes)
  library(shinycssloaders)
  library(cluster)
  library(fastcluster)
  library(gridExtra)
  library(ggalluvial)
  library(tidyr)
  library(dplyr)
  library(shinyWidgets)
  library(formattable)
  library(WeightedCluster)
  library(stringr)
  library(shinyjs)
  library(classInt)
  library(shinyBS)
  library(forcats)
  library(ggthemes)
  
  
  
  callModule(module = module_data, id = "id1")->DATAs
  


observe({print(head(DATAs()$DATA_COMP))})


#### STATISTIQUES DESCRIPTIVES ####

  
  ###max PAStrate ####
observeEvent(eventExpr = DATAs()$SEQ_OBJ,{
  req(DATAs())
  updateNumericInput(session=session, inputId = "PAStrate",max=length(names(DATAs()$SEQ_OBJ))-1)
})

  PASTRAJ<-reactive({
    input$PAStrate
  })
  TYPETRATE<-reactive({
    input$TYPEtrate
  })
  
  data.seq.inv<-reactive({
    req(DATAs())
    #req(input$timecol)
    print("summary(DATAs()$SEQ_OBJ)")
    print(summary(DATAs()$SEQ_OBJ))
    return(
      #seqdef(DATAs()$DATA_COMP[,rev(names(DATAs()$SEQ_OBJ))],cpal = NULL)
      seqdef(DATAs()$SEQ_OBJ[rev(names(DATAs()$SEQ_OBJ))], 
             cpal=NULL,
             gaps = DATAs()$CODAGE_MANQUANT$GAP,
             right = DATAs()$CODAGE_MANQUANT$RIGHT,
             left = DATAs()$CODAGE_MANQUANT$LEFT, nr = "RMA")
      )
  })
  
  #On organise la sequence dans l'autre sens afin d'avoir les transitions arrivant et pas seulement celles partant d'une situation
  Seqordre<-reactive({
    req(DATAs(),data.seq.inv())
    if(input$DebArr=="deb"){
      return(DATAs()$SEQ_OBJ)
    }else{
      return(data.seq.inv())
    }
  })
  
  # On propose la possibilité d'avoir le pourcentage d'individus ou leur effectif
  TRAJTRATE<-reactive({
    req(Seqordre())
    if (input$TypeValeur=="Pourcentage"){
      return(seqtrate(seqdata = Seqordre(), lag = PASTRAJ(), time.varying = TYPETRATE(),count=FALSE))
    }
    if (input$TypeValeur=="Effectif"){
      return(seqtrate(seqdata = Seqordre(), lag = PASTRAJ(), time.varying = TYPETRATE(),count=TRUE))
    }
  })
  #Liste contenant le(s) tableau(x) de transitions
  LISTTRATE<-reactive({
    req(TRAJTRATE())
    if(class(TRAJTRATE())=="array"){
      if(input$DebArr=="deb"){
        lapply(seq(1,dim(TRAJTRATE())[3]), function(x){TRAJTRATE()[ , , x]})->list.trate
      }else{
        lapply(seq(dim(TRAJTRATE())[3],1), function(x){TRAJTRATE()[ , , x]})->list.trate
      }
      
    } else {
      list(TRAJTRATE())->list.trate
    }
    list.trate
  })
  
  observe({
    print(LISTTRATE() )
    req(DATAs(),LISTTRATE())
    
    lapply(1:length(LISTTRATE()), FUN=function(i){
      paste0('TRAJTRATE', i)->id.output
      x<-LISTTRATE()[[i]]
      if (input$TypeValeur=="Pourcentage"){
        #data.frame(round(x*100, 2))->xx
        round(x*100, 2)->xx
      }
      if (input$TypeValeur=="Effectif"){
        #data.frame(x)->xx
        x->xx
      }
       print("xx")
      print(xx)
      gsub(pattern=" ",replacement="_", x = alphabet(DATAs()$SEQ_OBJ))->names.alpha
      names(xx)<-names.alpha
      print("xx names")
      print(xx)
      print("alphabet")
      print(alphabet(DATAs()$SEQ_OBJ))
      
      colnames(xx)<-gsub(pattern = "[-> ", replacement = "", fixed = TRUE, x = colnames(xx))
      colnames(xx)<-gsub(pattern = "]", replacement = "", fixed = TRUE, x = colnames(xx))
      
      colnames(xx)->names.alpha
      if(input$DebArr=="deb"){
        cbind(xx, "DEPART"=alphabet(DATAs()$SEQ_OBJ))->xx
        #alphabet(DATAs()$SEQ_OBJ)->xx$DEPART
        xx[, c("DEPART", names.alpha)]->xx
      }else{
        cbind(xx, "ARRIVEE"=alphabet(DATAs()$SEQ_OBJ))->xx
        
        #alphabet(DATAs()$SEQ_OBJ)->xx$ARRIVEE
        xx[, c("ARRIVEE", names.alpha)]->xx
      }
      
      #print(xx)
      output[[id.output]] <- DT::renderDataTable({xx})
      #Affiche les pas de temps considérés dnas le(s) tableau(x)
      paste0('Text_TRAJTRATE', i)->id.output.text
      if (input$TYPEtrate==TRUE){
        output[[id.output.text]] <- renderText(paste("Entre",colnames(DATAs()$SEQ_OBJ)[i],"et",colnames(DATAs()$SEQ_OBJ)[i+PASTRAJ()]))
      }else{
        output[[id.output.text]] <- renderText(paste("Sur l'ensemble de la période étudiée (entre",colnames(DATAs()$SEQ_OBJ)[1],"et",colnames(DATAs()$SEQ_OBJ)[dim(DATAs()$SEQ_OBJ)[2]],")"))
      }
      paste0('DownloadTabTrans',i)->id.output.download
      output[[id.output.download]] <- downloadHandler(
        filename = function() {
          paste0('Transition_',colnames(DATAs()$SEQ_OBJ)[i],'_',colnames(DATAs()$SEQ_OBJ)[i+PASTRAJ()], input$TypeTrans)
        },
        content = function(file){
          write.table(xx,file,sep = input$sepcol,row.names=TRUE,col.names = NA,dec = input$dec , fileEncoding = input$endoding)
        }
      )
      
    })
  })
  output$infotrate<-renderText({
    req(LISTTRATE())
    paste("Nombre de transitions affichées :", length(LISTTRATE()))
  })
  output$dt <- renderUI({
    req(LISTTRATE())
    return(lapply(1:length(LISTTRATE()), function(i) {
      tagList(fluidRow(align="center",textOutput(paste0('Text_TRAJTRATE', i))),
              dataTableOutput(paste0('TRAJTRATE', i)),
              column(2,downloadButton(paste0('DownloadTabTrans',i),"Télécharger")),
              column(10,hidden(p(id=paste0("texteTransition",i),paste0("Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (",input$TypeTrans," ). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application."))))
      )
    }))
  })
  
  observe({
    lapply(1:length(LISTTRATE()), function(i) {
      onevent("mouseleave", paste0('DownloadTabTrans',i), hide(paste0("texteTransition",i)))
      onevent("mouseenter", paste0('DownloadTabTrans',i), show(paste0("texteTransition",i)))
    })
  })
  
  TRATELONG<-reactive({
    req(LISTTRATE())
    do.call("rbind", LISTTRATE())->trate.long
    cbind(trate.long, "TIME"=c(matrix(sapply(1:length(LISTTRATE()), function(i){rep(i, length(names.alpha))}), ncol=1)))->trate.long
    data.frame(trate.long)->trate.long
    trate.long$ORIGINE<-row.names(trate.long)
    trate.long %>% gather(-TIME, -ORIGINE, key = APRES, value = value) ->trate.long
    trate.long
  })
  ##### SELECT AND PLOT MODULE #####
  callModule(module = module_select_and_plot, data = DATAs(), id = "id2")
  ##### SELECT AND PLOT ANCIEN #####
  
  
  ####### Type de graph ####
  #mise a jour des input et création de nouveaux input selon le type de sous-population chois

  #### MODULE TABLE ####
  callModule(module = module_tabdes, data = DATAs(), id = "id3")
  
  #### MODULE CLASSIF ####
  
  
  callModule(module = module_classification, data=DATAs(), id = "id5")->DATAs.c#DATA.CLASSIF$DATAs.c
  
  #observe({
  observeEvent(eventExpr = input$reactKlass, {
    callModule(module = module_select_and_plot, data = DATAs.c(), #reactive(DATA.CLASSIF$DATAs.c), 
               id = "id25")
  })
  #})
  observeEvent(eventExpr = input$reactKlass, {
    
    callModule(module = module_tabdes, data = DATAs.c(), id = "id35")
    
  })
  
}