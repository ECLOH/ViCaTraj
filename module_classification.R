#' @title   classification
#' @description  A shiny Module qui permet de classer des trajectoires
#'
#' @param id shiny id
#' @param label fileInput label
#' @export
module_classification_UI <- function(id){#label = "CSV file") {
  ns <- NS(id)
 ####
  textOutput("id_module_output"),
  tabsetPanel(
    #tabsetPanel(
    tabPanel(title="Matrice de distance",
             fluidRow(
               column(3,
                      h4("Paramètres généraux de la classification :"),
                      shiny::selectInput(inputId = ns("selection_rows"), label = "Sur quelles données voulez-vous travailler?", 
                                         c("Un echantillon"="Sample", 
                                           "Des trajectoires uniques avec leurs poids"="unique.traj", 
                                           "Toutes les trajectoires"="all"), selected = "all", multiple = FALSE),
                      shiny::uiOutput("TEXT_NB_UNIQUE_TRAJS"),
                      hr(),
                      uiOutput(ns("SAMPLING_INPUTS")),
                      
                      shiny::uiOutput("TEXT_NB_SELECTED_TRAJS") %>% withSpinner(color="#0dc5c1"),
                      textOutput("CONTROL_ID_MATCHING")
               ),
               column(4,
                      h4("Type de distance :"),
                      shiny::selectInput(inputId = "type_distance", label = "", c("Edition de trajectoires"="edit", "Attributs communs"="common_attributes", "Distribution d'états"="distrib"), multiple = FALSE),
                      hr(),
                      conditionalPanel(condition = "input.type_distance=='edit'",
                                       h4("Paramètres des coûts :"),
                                       shiny::uiOutput("InfobulleCout"),
                                       #method [seqcost(method = )]
                                       selectInput(inputId = "method_edit_cost", label = NULL,
                                                   choices = c("CONSTANT" , "TRATE", "FUTURE" , "FEATURES" , "INDELS", "INDELSLOG"),
                                                   selected = "TRATE", multiple = FALSE),
                                       uiOutput("SEQCOST_INPUTS") %>% withSpinner(color="#0dc5c1"),
                                       shiny::actionButton(inputId = "calculCouts", label = "Calcul des couts"))
               ),
               
               column(4,
                      h4("Paramètres de la matrice de distance :"),
                      shiny::uiOutput("InfobulleMatDistance"),
                      #Quelle méthode voulez-vous choisir pour calculer la matrice de distance entre les trajectoires? 
                      shiny::selectInput(inputId = "classtype", label = NULL, choices = c("OM", "LCS", "HAM"), selected = "OM", multiple = FALSE),
                      uiOutput("SEQDIST_INPUTS") %>% withSpinner(color="#0dc5c1"),
                      hr(),
                      uiOutput("PRINTTIMEDIST") %>% withSpinner(color="#0dc5c1"),
                      hr(),
                      shiny::actionButton(inputId = "calculDist", label = "Calcul de la matrice de distance"),
                      #conditionalPanel(condition = "input.calculDist",
                      uiOutput("PRINTSEQDIST") %>% withSpinner(color="#0dc5c1")
                      #)
               )
             ),
             conditionalPanel(condition = "input.type_distance=='edit'",
                              
                              fluidRow(
                                h3("Affichage des coûts calculés:"),
                                hr(),
                                h4("Coûts 'indel' :" ),
                                uiOutput("PRINTINDEL") %>% withSpinner(color="#0dc5c1"),
                                h4("Coûts de substitution :" ),
                                uiOutput("PRINTSUBST") %>% withSpinner(color="#0dc5c1")
                              )
             ),
             uiOutput("ATTR_TRAJ_FORCLASS")
             # shiny::conditionalPanel(condition = "input.classtype=='OM'",
             #                         shiny::sliderInput(label = "Coûts de substitution: rapport aux coûts indel", inputId = "subst_ratio", min = 0.1, max = 5, step = 0.1, value = 1, width = "80%")
             # )
             
    )
    ,tabPanel(title="Classification",
              fluidRow(
                column(4,
                       br(),
                       shiny::uiOutput("InfobulleClassif"),
                       # Quelle méthode voulez-vous utiliser pour regrouper les séquences ? partir de la matrice de dissemblance?
                       shiny::selectInput(inputId = "cluster_type", label = NULL, 
                                          choices = c("Hierarchical Clustering"="CAH", "FAST Hierarchical Clustering"="fastCAH", "Partitionning Around Medoid"="PAM","Combinaison de la CAH et de PAM"="CAHPAM"), selected = "CAHPAM", multiple = FALSE)),
                column(4,br(),
                       conditionalPanel(condition = "input.cluster_type=='CAH' | input.cluster_type=='CAHPAM'",
                                        shiny::uiOutput("InfobulleClassifCAH"),
                                        #Choix de la méthode (CAH) :
                                        shiny::selectInput(inputId = "agnes_method", choices = c("average", "single", "complete", "ward", "weighted", "flexible", "gaverage"), label = NULL, selected = "ward", multiple = FALSE)),
                       conditionalPanel(condition = "input.cluster_type=='fastCAH'",
                                        shiny::uiOutput("InfobulleClassiffastCAH"),
                                        # Choix de la méthode (FAST CAH) :
                                        shiny::selectInput(inputId = "fastclust_method", choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" ,"centroid"), label = NULL, selected = "ward.D2", multiple = FALSE))
                ),
                # 
                
                column(2,
                       br(),br(),
                       shiny::actionButton(inputId = "calculCLUST", label = "Calcul de la classification")
                )),
              fluidRow(useShinyjs(),
                       uiOutput("classif_grp"),
                       column(2,
                              textOutput("textCluster")),
                       column(2,
                              conditionalPanel(condition = "input.Bouton_Clustering",
                                               shiny::radioButtons(inputId = "TypeFichierDownload", label = "Extension du fichier",choices=c("csv","txt"),selected = "csv"),
                                               downloadButton('ButtondownloadData', 'Télécharger les données')
                              )
                       ),
                       column(4,
                              shiny::uiOutput("TexteClassif"))
              ),
              uiOutput("classif"),
              uiOutput("tabind")
              
    )
  )
}
}

#' server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#' @param data
#'
#' @export
module_classification <- function(input, output, session, data) {
  #### PARAMETERS ####
  library(dplyr)
  library(TraMineR)
  library(RColorBrewer)
  library(ggplot2)
  library(ggthemes)
  ns <- session$ns
  ####
  observe({
    x <- input$type_distance
    names(
      seqdist.args[[x]])->listed_names_methods
    # Can also set the label and select items
    updateSelectInput(session=session, inputId = "classtype",
                      label ="Quel méthode voulez-vous choisir pour calculer la matrice de distance entre les trajectoires? ",
                      choices = listed_names_methods
    )
  })
  
  ### Infobulle pour donner des informations sur les méthodes de calcul de la matrice de distance
  
  titreInfobulleMatDistance<-reactive({
    return(paste("Vous avez choisi la méthode",input$classtype))
  })
  
  contenuInfobulleMatDistance<-reactive({
    ifelse(input$classtype =="OM",paste("<p> Vous avez selectionné la méthode",input$classtype,paste('<a href=','https://www.insee.fr/fr/accueil','> INSEE </a> ',sep='"'),"</p>"),
           ifelse(input$classtype=="OMloc",paste("<p> Vous avez selectionné la méthode",input$classtype,"</p>"),
                  ifelse(input$classtype=="HAM",paste("<p> Vous avez selectionné la méthode",input$classtype,"</p>"),
                         ifelse(input$classtype=="DHD",paste("<p> Vous avez selectionné la méthode",input$classtype,"</p>"),
                                ifelse(input$classtype=="LCS",paste("<p> Vous avez selectionné la méthode",input$classtype,"</p>"),
                                       ifelse(input$classtype=="LCP",paste("<p> Vous avez selectionné la méthode",input$classtype,"</p>"),
                                              ifelse(input$classtype=="RLCP",paste("<p> Vous avez selectionné la méthode",input$classtype,"</p>"),"")))))))
    
  })
  
  output$InfobulleMatDistance<-renderUI({
    span("Quelle méthode voulez-vous choisir pour calculer la matrice de distance entre les trajectoires? ",
         popify(el = icon(name = "info-circle", lib = "font-awesome"),trigger="click",placement = "right",title = titreInfobulleMatDistance() ,content = contenuInfobulleMatDistance())
    )
  })
  
  #### SEQCOST and SEQDIST input ####
  observeEvent(eventExpr = input$method_edit_cost, {
    shiny::renderUI({
      cost.args[[input$method_edit_cost]]->arg2
      edit.cost.inputs[names(edit.cost.inputs)%in%arg2]
    })->output$SEQCOST_INPUTS
  })
  #
  shiny::renderUI({
    seqdist.args[[input$type_distance]][[input$classtype]]->arg2
    seqdist.inputs[names(seqdist.inputs)%in%arg2]
  })->output$SEQDIST_INPUTS
  
  output$id_module_output<-renderPrint({
    paste(" print(data$ID_VAR) : ",  print(data$ID_VAR), "| dim(data$DATA_COMP) : ", dim(data$DATA_COMP), sep="")
    
    #print(data$ID_VAR)
  })
  
  MATCH_SEQ_DATA_C0NTROL<-reactive({
    if(sum(attributes(data$SEQ_OBJ)$row.names!=unique(unlist(lapply(data$DATA_COMP, FUN = function(x){ x[ , data$ID_VAR ] }))) )==0){
      1 
    } else {
      0
    }
  })
  output$CONTROL_ID_MATCHING<-renderPrint({ MATCH_SEQ_DATA_C0NTROL() })
  
  ###trajs.forclass  ####
  
  renderUI({
    if(input$selection_rows=="Sample"){
      list(
    shiny::numericInput(inputId = ns("sample_prop"), label = "Taille de l'échantillon", value = 0.1, min = 0.05, max = 0.95, step = 0.05),
    shiny::column(
      width = 4,
      list(
      shiny::selectInput(inputId = ns("SELECTDATE1"), label = "Date (1) : ",
                         choices = c("Pas de sélection",as.character( names(data$DATA_COMP) )), multiple = FALSE, width = "100%",
                         selected=NULL),
      shiny::selectInput(inputId = ns("VARDATE1"), label = "Variable (1) : ",
                         choices = NULL, multiple = FALSE, width = "100%",
                         selected=NULL)
      )
      ),
    shiny::column(
      width = 4,
      list(
        shiny::selectInput(inputId = ns("SELECTDATE2"), label = "Date (2) : ",
                           choices = c("Pas de sélection",as.character( names(data$DATA_COMP) )), multiple = FALSE, width = "100%",
                           selected=NULL),
        shiny::selectInput(inputId = ns("VARDATE2"), label = "Variable (2) : ",
                           choices = NULL, multiple = FALSE, width = "100%",
                           selected=NULL)
      )
    ),
    shiny::column(
      width = 4,
      list(
        shiny::selectInput(inputId = ns("SELECTDATE3"), label = "Date (3) : ",
                           choices = c("Pas de sélection",as.character( names(data$DATA_COMP) )), multiple = FALSE, width = "100%",
                           selected=NULL),
        shiny::selectInput(inputId = ns("VARDATE3"), label = "Variable (3) : ",
                           choices = NULL, multiple = FALSE, width = "100%",
                           selected=NULL)
      )
    )
      )
    }
  })->SELECT_SAMPLE

  
 reactive({
   if(input$selection_rows=="Sample"){
     lapply(1:length(c(input$SELECTDATE1, input$SELECTDATE2, input$SELECTDATE3)), FUN = function(i){
       dat.i<-c(input$SELECTDATE1, input$SELECTDATE2, input$SELECTDATE3)[i]
       if(!is.null(dat.i)){
         if(dat.i!="Pas de sélection"){
           data$DATA_COMP[[dat.i]][ , paste(VARDATE, i, sep="")]
         }
       }
     })->list.var.sample
     list.var.sample[lengths(list.var.sample) != 0]->list.var.sample
     data.frame(do.call("cbind", list.var.sample))->list.var.sample
     interaction(lapply(1:ncol(list.var.sample), function(j){list.var.sample[ , j]}))->interaction.var
     return(interaction.var)
   }
 })->interaction.var
  
  trajs.forclass<-reactive({
    req(MATCH_SEQ_DATA_C0NTROL())
    if(MATCH_SEQ_DATA_C0NTROL()==1){
      if(input$selection_rows=="Sample"){
        if(#input$sample_var==""|
          is.null(input$sample_var)#|length(input$sample_var)<1
        ){
          REPRESENTED_SAMPLE(data = data$DATA_COMP, interact.var = NULL, SIZE = input$sample_prop*nrow(data$SEQ_OBJ), id.var = data$ID_VAR)->vec.sample
        } else {
          REPRESENTED_SAMPLE(data = data$DATA_COMP, 
                             interact.var = interaction.var(), 
                             SIZE = input$sample_prop*nrow( data$DATA_COMP ), id.var=data$ID_VAR)->vec.sample
        }
        seqdef(data$SEQ_OBJ[row.names(data$SEQ_OBJ)%in%vec.sample , ], 
               left = data$CODAGE_MANQUANT$LEFT,#input$TEXT_LEFT, 
               right = data$CODAGE_MANQUANT$RIGHT,#input$TEXT_RIGHT, 
               gaps = data$CODAGE_MANQUANT$GAP,#input$TEXT_GAP, nr = "RMA",
               id = row.names( data$SEQ_OBJ[row.names(data$SEQ_OBJ)%in%vec.sample , ] ))
        
      } else {
        if(input$selection_rows=="unique.traj"){
          #### unique.traj ####
          #rev(wesanderson::wes_palette(name = "Darjeeling1", n = length(alphabet(data$SEQ_OBJ)), type = "continuous"))->cpal.seq
          #cpal(seqdata = data$SEQ_OBJ)<-cpal.seq
          seqtab(data$SEQ_OBJ, idxs = 0, format = "STS")->unique.trajs
          data.frame(unique.trajs)->unique.trajs.df
          seqdef(data = unique.trajs.df, weights = attributes(unique.trajs)$freq$Percent, cpal = cpal.seq, 
                 gaps = data$CODAGE_MANQUANT$GAP,
                 right = data$CODAGE_MANQUANT$RIGHT,
                 left = data$CODAGE_MANQUANT$LEFT, nr = "RMA")->unique.trajs.seq
          unique.trajs.seq
        } else {
          if(input$selection_rows=="all"){
            data$SEQ_OBJ
          }
        }
      }
    }
  })
  
  
  output$ATTR_TRAJ_FORCLASS<-renderUI({
    attributes(trajs.forclass() )->list.attr
    print(list.attr)
    list.attr[names(list.attr)!="row.names"]->list.attr
    lapply(1:length(list.attr), function(li){
      list(
        h5(paste(names(list.attr)[li], " : ", sep = "")),
        renderPrint({list.attr[[li]]})
      )
    })
    #summary(data$SEQ_OBJ)
  })
  
  #   ### SEQCOST  ###
  #
  SEQCOST<-eventReactive(eventExpr = input$calculCouts, {
    req(trajs.forclass())
    seqcost(seqdata=trajs.forclass(),
            method = input$method_edit_cost,
            cval = input$subst_ratio,
            time.varying=input$time_varying_substitution_costs,
            transition=input$transition_substitution_costs,
            lag=input$lag_subst_cost, weighted = TRUE, with.missing = FALSE)
  })
  #
  #    #### PRINT COSTS ####
  output$PRINTINDEL<-renderUI({
    req(SEQCOST())
    SEQCOST()$indel->the.indels
    if(length(the.indels)>1){
      the.indels<-data.frame("Etats"=alphabet(trajs.forclass()), "Cout(s)_INDEL"=round(the.indels, 2))
      DT::renderDataTable(the.indels)->output$bb
      dataTableOutput("bb", width = "80%")
    } else {
      renderText(as.character(the.indels))->output$bb
      textOutput("bb")
    }
  })
  #
  output$PRINTSUBST<-renderUI({
    req(SEQCOST())
    SEQCOST()$sm->the.sm
    if(class(the.sm)=="matrix"){
      DT::renderDataTable(the.sm)->output$aa
      dataTableOutput("aa")
    } else {
      renderPrint(the.sm)->output$aa
      shiny::verbatimTextOutput ("aa")
    }
  })
  
  ### Infobulle sur les méthodes proposées ###
  titreInfobulleCout<-reactive({
    return(paste("Vous avez choisi la méthode",input$method_edit_cost))
  })
  
  contenuInfobulleCout<-reactive({
    ifelse(input$method_edit_cost =="CONSTANT",paste("<p> Vous avez selectionné la valeur",input$method_edit_cost,paste('<a href=','https://www.insee.fr/fr/accueil','> INSEE </a>',sep='"'),"</p>"),
           ifelse(input$method_edit_cost=="TRATE",paste("<p> Vous avez selectionné la valeur",input$method_edit_cost,"</p>"),
                  ifelse(input$method_edit_cost=="FUTURE",paste("<p> Vous avez selectionné la valeur",input$method_edit_cost,"</p>"), 
                         ifelse(input$method_edit_cost=="FEATURES",paste("<p> Vous avez selectionné la valeur",input$method_edit_cost,"</p>"), 
                                ifelse(input$method_edit_cost=="INDELS",paste("<p> Vous avez selectionné la valeur",input$method_edit_cost,"</p>"), 
                                       ifelse(input$method_edit_cost=="INDELSLOG",paste("<p> Vous avez selectionné la valeur",input$method_edit_cost,"</p>"), "" ))))))
    
  })
  
  output$InfobulleCout<-renderUI({
    span("method [seqcost(method = )]",
         popify(el = icon(name = "info-circle", lib = "font-awesome"),trigger="click",placement = "right",title = titreInfobulleCout() ,content = contenuInfobulleCout())
    )
  })
  
  # #
  #### SEQDIST  ####
  output$PRINTTIMEDIST<-renderUI({
    req(NB_SELECT_TRAJS())
    predict.time.dist(nb.sequences = NB_SELECT_TRAJS())->pred.data
    paste("Pour", NB_SELECT_TRAJS(), "trajectoires, le temps de calcul estimé pour la fonction seqdist() est de", pred.data$nb.secondes, "secondes, soit", pred.data$nb.minutes, "minutes")->thetext
    renderText(thetext)->output$thetext
    textOutput("thetext")
  })
  SEQDIST<-eventReactive(eventExpr = input$calculDist, {
    req(input$refseq_seqdist,trajs.forclass(),SEQCOST(),input$norm_seqdist)
    if(input$refseq_seqdist==FALSE){REFSEQ<-NULL} else {REFSEQ<-input$refseq_seqdist==FALSE}
    seqdist(seqdata = trajs.forclass(), method = input$classtype, refseq = REFSEQ, 
            norm = input$norm_seqdist, indel = SEQCOST()$indel, sm = SEQCOST()$sm, 
            expcost = input$expcost_seqdist, context=input$context_seqdist, weighted = TRUE, with.missing = FALSE)
  })
  #
  observeEvent(input$calculDist, {
    output$PRINTSEQDIST<-renderUI({
      req(SEQDIST())
      renderText(paste("Création d'un objet 'dist' comportant", length(SEQDIST()), "élements. La distance minimale est de", min(SEQDIST()), "la distance maximale de", max(SEQDIST()), "et la distance moyenne de", round(sum(SEQDIST())/length(SEQDIST()), 2), sep = " "))->output$cc
      textOutput("cc") #%>% withSpinner(color="#0dc5c1")
    })
  })
  
  ### CLASSIFICATION ####
  SEQCLASS<-eventReactive(eventExpr = input$calculCLUST, {
    req(SEQDIST())
    if(input$cluster_type=="CAH" | input$cluster_type=="CAHPAM"){
      #agnes(x = SEQDIST(), method = input$agnes_method)
      agnes(as.dist(SEQDIST()), method = input$agnes_method, keep.diss=FALSE)
    } else {
      if(input$cluster_type=="fastCAH"){
        fastcluster::hclust(d = as.dist(SEQDIST()) , method = input$fastclust_method, members = NULL)
        
      }
    }
  })
  ### Infobulle sur les différents types de classification (CAH,PAM,...) ###
  titreInfobulleClassif<-reactive({
    return(paste("Vous avez choisi la méthode",input$cluster_type))
  })
  
  contenuInfobulleClassif<-reactive({
    ifelse(input$cluster_type =="CAH",paste("<p> Vous avez selectionné la méthode",input$cluster_type,paste('<a href=','https://www.insee.fr/fr/accueil','> INSEE </a> ',sep='"'),"</p>"),
           ifelse(input$cluster_type=="fastCAH",paste("<p> Vous avez selectionné la méthode",input$cluster_type,"</p>"),
                  ifelse(input$cluster_type=="PAM",paste("<p> Vous avez selectionné la méthode",input$cluster_type,"</p>"),
                         ifelse(input$cluster_type=="CAHPAM",paste("<p> Vous avez selectionné la méthode",input$cluster_type,"</p>"),""))))
    
  })
  
  output$InfobulleClassif<-renderUI({
    span("Quelle méthode voulez-vous utiliser pour regrouper les séquences ? partir de la matrice de dissemblance?",
         popify(el = icon(name = "info-circle", lib = "font-awesome"),trigger="click",placement = "right",title = titreInfobulleClassif() ,content = contenuInfobulleClassif())
    )
  })
  
  ### Infobulle sur les méthodes de classification (ward,...) ###
  titreInfobulleClassif2<-reactive({
    return(paste("Vous avez choisi la méthode",input$cluster_type))
  })
  
  contenuInfobulleClassif2<-reactive({
    ifelse(input$cluster_type =="CAH",paste("<p> Vous avez selectionné la méthode",input$cluster_type,paste('<a href=','https://www.insee.fr/fr/accueil','> INSEE </a> ',sep='"'),"</p>"),
           ifelse(input$cluster_type=="fastCAH",paste("<p> Vous avez selectionné la méthode",input$cluster_type,"</p>"),
                  ifelse(input$cluster_type=="CAHPAM",paste("<p> Vous avez selectionné la méthode",input$cluster_type,"</p>"),"")))
    
  })
  ### CAH et CAHPAM ###
  output$InfobulleClassifCAH<-renderUI({
    span("Choix de la méthode (CAH) :",
         popify(el = icon(name = "info-circle", lib = "font-awesome"),trigger="click",placement = "right",title = titreInfobulleClassif2() ,content = contenuInfobulleClassif2())
    )
  })
  ### fastCAH ###
  output$InfobulleClassiffastCAH<-renderUI({
    span("Choix de la méthode (FAST CAH) :",
         popify(el = icon(name = "info-circle", lib = "font-awesome"),trigger="click",placement = "right",title = titreInfobulleClassif2() ,content = contenuInfobulleClassif2())
    )
  })
  
  ##### Graphiques (dendogramme et inertie) #####
  output$classif<- renderUI({
    input$calculCLUST
    isolate({
      req(SEQCLASS())
      if (input$cluster_type=="CAH" | input$cluster_type=="fastCAH"){
        renderPlot(plot(SEQCLASS(), which.plots = 2))->output$dd
        return(plotOutput("dd") %>% withSpinner(color="#0dc5c1"))
      }
      if(input$cluster_type=="CAHPAM"){
        output$dendo<-renderPlot({
          plot(as.dendrogram(SEQCLASS()))
        })
        output$inertie<-renderPlot({
          plot(sort(SEQCLASS()$height, decreasing=TRUE)[1:20], type="s", xlab="nb de classes", ylab="inertie")
        })
        
        
        return(tagList(
          noUiSliderInput(inputId = "SliderGrp",label="Nombre de groupes",min=2,max=10,value = c(4,6),limit=2,step=1,margin=2,behaviour = "drag"),
          fluidRow(column(12,
                          splitLayout(
                            plotOutput("dendo") %>% withSpinner(color="#0dc5c1"),
                            plotOutput("inertie") %>% withSpinner(color="#0dc5c1"))))
          
        ))
        
      }
    })
    
  })
  
  # output$DENDOGRAM<-renderUI({
  #   req(SEQCLASS())
  #   renderPlot(plot(SEQCLASS(), which.plots = 2))->output$dd
  #   plotOutput("dd") %>% withSpinner(color="#0dc5c1")
  # })
  
  
  #### Tableau indicateurs pour évaluer la qualité des classifiactions testées ####
  
  output$tabind<-renderUI({
    req(SEQCLASS(),input$SliderGrp)
    input$calculCLUST
    input$SliderGrp
    isolate({
      if(input$cluster_type=="CAHPAM"){
        indicateur<-Creation_indicateur(nb_cluster_min=min(input$SliderGrp,na.rm=TRUE),nb_cluster_max=max(input$SliderGrp,na.rm=TRUE),mat_dist=SEQDIST(),intialclust=SEQCLASS())
        output$tabIndicateur<-renderFormattable(tableau_cluster(indicateurs=indicateur$dataFrame))
        return(fluidRow(column(12,
                               formattableOutput("tabIndicateur"))))
      }
      
    })
  })
  
  
  ### Création d'une variable "Clustering" donnant la classification choisie ###
  output$classif_grp<-renderUI({
    req(SEQCLASS())
    input$calculCLUST
    input$SliderGrp
    isolate({
      if (input$cluster_type=="CAHPAM"){
        req(input$SliderGrp)
        return(tagList(
          column(2,
                 shiny::numericInput(inputId = "nb_cluster",label="Nombre de groupes choisi",step=1,value = min(input$SliderGrp,na.rm=TRUE)+1,min=min(input$SliderGrp,na.rm=TRUE),max=max(input$SliderGrp,na.rm=TRUE))
          ),
          column(2,
                 shiny::actionButton(inputId = "Bouton_Clustering",label = "Faire les groupes")
          )
        ))
        
      }
      if(input$cluster_type=="CAH"){
        return(tagList(
          column(2,
                 shiny::numericInput(inputId = "nb_cluster",label="Nombre de groupes choisi",step=1,value = 2,min=2 ,max=10)
          ),
          column(2,
                 shiny::actionButton(inputId = "Bouton_Clustering",label = "Faire les groupes")
          )
        ))
      }
    })
  })
  
  
  dataCluster<-eventReactive(eventExpr = input$Bouton_Clustering,{
    
    req(SEQCLASS())
    
    
    if(input$selection_rows=="Sample"){
      
      data$DATA_COMP[data$DATA_COMP[ , data$ID_VAR]%in%row.names(trajs.forclass()) , ]->df_pour_class
      
    } else {data$DATA_COMP->df_pour_class}
    
    if (input$cluster_type=="CAHPAM"){
      
      
      
      
      indicateur<-Creation_indicateur(nb_cluster_min=min(input$SliderGrp,na.rm=TRUE),
                                      nb_cluster_max=max(input$SliderGrp,na.rm=TRUE)
                                      ,mat_dist=SEQDIST(),
                                      intialclust=SEQCLASS())
      #return(
      data_cluster(indicateur,
                   df_pour_class,#data$DATA_COMP,
                   input$nb_cluster)->res_def
    } else {
      if(input$cluster_type=="CAH"){
        
        clusterCAH<-as.factor(cutree(SEQCLASS(),k = input$nb_cluster))
        clusterCAH.class<-factor(clusterCAH,labels = paste0("G",1:input$nb_cluster))
        
        if(input$selection_rows=="Sample"){
          
          data.frame("ID"=row.names(trajs.forclass()), "Clustering"=clusterCAH.class)->df
          
          merge(df_pour_class, df, by.x=data$ID_VAR, by.y="ID", all.x=TRUE)->dataCopieCAH
          
        } else {
          if(input$selection_rows=="all"){
            df_pour_class->dataCopieCAH
            dataCopieCAH[,"Clustering"]<-clusterCAH.class
          } else {
            if(input$selection_rows=="unique.traj"){
              
              data.frame("ID"= sapply(1:nrow(unique.trajs()), function(i){paste(unique.trajs()[i , ], collapse = "-")}), 
                         "Clustering"=clusterCAH.class)->df
              
              data.frame("IDVAR"=row.names(data$SEQ_OBJ), 
                         "ID"=sapply(1:nrow(data$SEQ_OBJ), function(i){paste(data$SEQ_OBJ[i , ], collapse = "-")})
              )->df2
              
              merge(df2, df, by.x="ID", by.y="ID")->df3
              
              merge(df_pour_class, df3, by.x=data$ID_VAR, by.y="IDVAR")->dataCopieCAH #INDVAR_UNI()
            }
          }
        }
        dataCopieCAH->res_def
      }
      
      
    }
    data.frame(lapply(res_def, function(x){if(is.character(x)){as.factor(x)} else {x}}))->res_def
    #as.data.frame(dataCopieCAH, stringsAsFactors = TRUE)->dataCopieCAH
    #dataCopieCAH<-data.frame(lapply(dataCopieCAH, FUN = function(x){
    #   if(is.character(x)|is.factor(x)){
    #     as.factor(as.character(x))
    #   } else {x}
    # }))
    return(res_def)
    #return(dataCopieCAH)
  })
  
  
  output$textCluster<-renderText({
    input$Bouton_Clustering
    isolate({
      req(dataCluster())
      return(paste("Vous avez créé",length(levels(dataCluster()[,"Clustering"])),"groupes"))
    })
    
  })
  
  ### Télécharger le fichier de données avec la classification ###
  
  output$ButtondownloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("DataCluster",input$TypeFichierDownload,sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      shiny::withProgress(
        message = "Veuillez patienter, le téléchargement de votre fichier est en cours",
        value = 0,
        {
          # Write to a file specified by the 'file' argument
          if (input$rowname == TRUE){
            write.table(dataCluster(), file, sep = input$sepcol,row.names=TRUE,col.names = NA,dec = input$dec,fileEncoding = input$endoding, na = argna())
          }else{
            write.table(dataCluster(), file, sep = input$sepcol,row.names = FALSE,dec = input$dec,fileEncoding = input$endoding, na = argna())
          }
          shiny::incProgress(1)
        })
    }
  )
  
  output$TexteClassif<-renderUI({
    hidden(p(id="texteClassification",paste0("Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (",input$TypeFichierDownload," ). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application.")))
  })
  
  onevent("mouseleave", 'ButtondownloadData', hide("texteClassification"))
  onevent("mouseenter", 'ButtondownloadData', show("texteClassification"))
}