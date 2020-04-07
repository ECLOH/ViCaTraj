#' @title   classification
#' @description  A shiny Module qui permet de classer des trajectoires
#'
#' @param id shiny id
#' @param label fileInput label
#' @export
module_classification_UI <- function(id){#label = "CSV file") {
  ns <- NS(id)
 ####
  #textOutput("id_module_output"),
  tabsetPanel(
    #tabsetPanel(
    tabPanel(title="Matrice de distance",
             
             fluidRow(
               column(3,
                      h4("Paramètres généraux de la classification :"),
                      shiny::selectInput(inputId = ns("selection_rows"), 
                                         label = "Sur quelles données voulez-vous travailler?", 
                                         choices = c("Un echantillon"="Sample", 
                                           "Des trajectoires uniques avec leurs poids"="unique.traj", 
                                           "Toutes les trajectoires"="all"), 
                                         selected = "all", multiple = FALSE),
                      uiOutput(ns("SELECT_SAMPLE")),
                      #uiOutput(ns("SELECTED_SELECTION")),
                      #uiOutput(ns("SELECTED_SELECTION2")),
                      #uiOutput(ns("TRAJS_FOR_CLASS_CONTROL")),
                      #DTOutput(ns("TABLE_ECH")),
                      uiOutput(ns("TEXT_NB_UNIQUE_TRAJS")),
                      uiOutput(ns("TEXT_NB_SELECT_TRAJS"))%>% withSpinner(color="#0dc5c1"),
                      hr(),
                      textOutput("CONTROL_ID_MATCHING")
               ),
               column(4,
                      h4("Type de distance :"),
                      shiny::selectInput(inputId = ns("type_distance"), 
                                         label = "", c("Edition de trajectoires"="edit", 
                                                       "Attributs communs"="common_attributes", 
                                                       "Distribution d'états"="distrib"), multiple = FALSE),
                      hr(),
                      conditionalPanel(condition = "input.type_distance=='edit'",ns = ns,
                                       h4("Paramètres des coûts :"),
                                       shiny::uiOutput(ns("InfobulleCout")),
                                       #method [seqcost(method = )]
                                       selectInput(inputId = ns("method_edit_cost"), label = NULL,
                                                   choices = c("CONSTANT" , "TRATE", "FUTURE" , "FEATURES" , "INDELS", "INDELSLOG"),
                                                   selected = "TRATE", multiple = FALSE),
                                       uiOutput(ns("SEQCOST_INPUTS")),# %>% withSpinner(color="#0dc5c1"),
                                       shiny::actionButton(inputId = ns("calculCouts"), label = "Calcul des couts"))
               ),
               
               column(4,
                      h4("Paramètres de la matrice de distance :"),
                      shiny::uiOutput(ns("InfobulleMatDistance")),
                      #Quelle méthode voulez-vous choisir pour calculer la matrice de distance entre les trajectoires? 
                      shiny::selectInput(inputId = ns("classtype"), label = NULL, 
                                         choices = c("OM", "LCS", "HAM"), selected = "OM", multiple = FALSE),
                      uiOutput(ns("SEQDIST_INPUTS")) %>% withSpinner(color="#0dc5c1"),
                      hr(),
                      uiOutput(ns("PRINTTIMEDIST")) %>% withSpinner(color="#0dc5c1"),
                      hr(),
                      shiny::actionButton(inputId = ns("calculDist"), label = "Calcul de la matrice de distance"),
                      #conditionalPanel(condition = "input.calculDist",
                      uiOutput(ns("PRINTSEQDIST")) %>% withSpinner(color="#0dc5c1")
                      #)
               )
             ),
             conditionalPanel(condition = "input.type_distance=='edit'",ns = ns,
                              
                              fluidRow(
                                h3("Affichage des coûts calculés:"),
                                hr(),
                                h4("Coûts 'indel' :" ),
                                uiOutput(ns("PRINTINDEL")) %>% withSpinner(color="#0dc5c1"),
                                h4("Coûts de substitution :" ),
                                uiOutput(ns("PRINTSUBST")) %>% withSpinner(color="#0dc5c1")
                              )
             ),
             uiOutput("ATTR_TRAJ_FORCLASS")
             # shiny::conditionalPanel(condition = "input.classtype=='OM'",
             #                         shiny::sliderInput(label = "Coûts de substitution: rapport aux coûts indel", inputId = "subst_ratio", min = 0.1, max = 5, step = 0.1, value = 1, width = "80%")
             # )
             
    )
    ,tabPanel(title="Classification",
              fluidRow(
                #shinyjs::useShinyjs(), 
                #id=ns("form"),
                #shiny::actionButton(inputId = ns("refresh"), label = "Réinitialiser les paramètres de clustering"),
                column(6,
                       br(),
                       shiny::uiOutput("InfobulleClassif"),
                       # Quelle méthode voulez-vous utiliser pour regrouper les séquences ? partir de la matrice de dissemblance?
                       shiny::selectInput(inputId = ns("cluster_type"), label = NULL, 
                                          choices = c("Hierarchical Clustering"="CAH", 
                                                      "FAST Hierarchical Clustering"="fastCAH", 
                                                      "Partitionning Around Medoid"="PAM",
                                                      "Combinaison de la CAH et de PAM"="CAHPAM"), 
                                          selected = "CAHPAM", multiple = FALSE),
                       conditionalPanel(condition = "input.cluster_type=='CAH' | input.cluster_type=='CAHPAM'",ns=ns,
                                        shiny::uiOutput("InfobulleClassifCAH"),
                                        #Choix de la méthode (CAH) :
                                        shiny::selectInput(inputId = ns("agnes_method"), choices = c("average", "single", "complete", "ward", "weighted", "flexible", "gaverage"), 
                                                           label = NULL, selected = "ward", multiple = FALSE)),
                       conditionalPanel(condition = "input.cluster_type=='fastCAH'", ns=ns,
                                        shiny::uiOutput("InfobulleClassiffastCAH"),
                                        # Choix de la méthode (FAST CAH) :
                                        shiny::selectInput(inputId = ns("fastclust_method"), choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" ,"centroid"), 
                                                           label = NULL, selected = "ward.D2", multiple = FALSE)),
                       shiny::actionButton(inputId = ns("calculCLUST"), label = "Calcul de la classification")
                ),
                column(width = 6),
                column(width = 12, 
                hr(),
                hr()
                ),
              column(width=12,
                #useShinyjs(),
              #column( width = 6,
                      useShinyjs(),
                      uiOutput(ns("classif")),
                       uiOutput(ns("classif_grp")),
                              textOutput(ns("textCluster"))
              ),
              column(width=12,#),
                              shiny::downloadButton(outputId = ns("DOWNSEQ"), 
                                    label = "Enregistrer les trajectoires, la classification, et les données complémentaires sur le disque : " )
              ),
              column(width=12,
                      # column(2,
                              #conditionalPanel(condition = "input.Bouton_Clustering", ns=ns, 
                              #                 shiny::radioButtons(inputId = ns("TypeFichierDownload"), label = "Extension du fichier",choices=c("csv","txt"),selected = "csv"),
                              #                 downloadButton('ButtondownloadData', 'Télécharger les données')
                              #),
                       
                       #column(4,
                              shiny::uiOutput(ns("TexteClassif"))
              ),
              
              uiOutput(ns("tabind"))
              
    )
  )
  )
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
  ###trajs.forclass  ####
  
  #observeEvent(input$refresh, {
  #  shinyjs::reset("form")
  #})
  
  
  renderUI({
    if(input$selection_rows=="Sample"){
      list(
        shiny::numericInput(inputId = ns("sample_prop"), label = "Taille de l'échantillon", value = 0.1, min = 0.05, max = 0.95, step = 0.05),
        h4("Variables utilisées pour la représentativité : "), 
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
  })->output$SELECT_SAMPLE
  
  observe({
    if(!is.null(input$SELECTDATE1)){
      if(input$SELECTDATE1!="Pas de sélection"){
        names(data$DATA_COMP[[input$SELECTDATE1]])->NAMES1
        updateSelectInput(session = session, inputId = "VARDATE1",choices = NAMES1)
        
      }
    }
  })
    
  observe({
      if(!is.null(input$SELECTDATE2)){
      if(input$SELECTDATE2!="Pas de sélection"){
        names(data$DATA_COMP[[input$SELECTDATE2]])->NAMES2
        updateSelectInput(session = session, inputId = "VARDATE2",choices = NAMES2)
        
      }
      }
  })
    
  observe({
    if(!is.null(input$SELECTDATE3)){
      if(input$SELECTDATE3!="Pas de sélection"){
        names(data$DATA_COMP[[input$SELECTDATE3]])->NAMES3
        updateSelectInput(session = session, inputId = "VARDATE3",choices = NAMES3)
        
      }
    }
  })
  
  
  output$SELECTED_SELECTION<-renderUI({
    message("COUCOU 212")
    h3(input$selection_rows)
  })
  
  reactive({
    if(input$selection_rows=="Sample"){
      dati<-c(input$SELECTDATE1, input$SELECTDATE2, input$SELECTDATE3)
      vali<-c(input$VARDATE1, input$VARDATE2, input$VARDATE3)
#      if(length(unique(dati))==1&unique(dati)=="Pas de sélection"){
#        NULL
#      } else { 
      print(vali)
      print(names(data))
      print(names(data$DATA_COMP))
      print(names(data$DATA_COMP[[1]]))
      print(head(data$DATA_COMP[[1]]))
      
      lapply(X = 1:3, FUN = function(i){
        dati[i]->dat.i
        print(dat.i)
        print(vali[i])
        if(!is.null(dat.i)){
          if(dat.i!="Pas de sélection"){
            message("coucou 221")
            print(data$DATA_COMP[[dat.i]][ , vali[i] ])
            as.character(data$DATA_COMP[[dat.i]][ , vali[i] ])->res
            return(res)
          } else {
            print("Walou!")
            #print(class(data$DATA_COMP[[1]]))
            #print(data$DATA_COMP[[dat.i]])
            
            rep("1", times=nrow(data$DATA_COMP[[1]]))
          }
        }
      })->list.var.sample
      print(list.var.sample)
      
      list.var.sample[lengths(list.var.sample) != 0]->list.var.sample
      if(length(list.var.sample)>0){
      data.frame(do.call("cbind", list.var.sample), stringsAsFactors = FALSE)->list.var.sample
      interaction(lapply(1:ncol(list.var.sample), function(j){list.var.sample[ , j]}))->interaction.var
      
      return(interaction.var)
      }
 #     }
    } else {
      message("215")
      return(NULL)
    }
    })->selected_react
  
  
  output$SELECTED_SELECTION2<-renderUI({
    message("COUCOU 223")
    h3(selected_react())
  })
  
  
  trajs.forclass<-reactive({
      if(input$selection_rows=="Sample"){
          REPRESENTED_SAMPLE(interact.var = selected_react(), 
                             SIZE = input$sample_prop*nrow( data$SEQ_OBJ ), id.var=row.names(data$SEQ_OBJ))->vec.sample
        print(vec.sample)
        seqdef(data$SEQ_OBJ[row.names(data$SEQ_OBJ)%in%vec.sample , ], 
               left = data$CODAGE_MANQUANT$LEFT,#input$TEXT_LEFT, 
               right = data$CODAGE_MANQUANT$RIGHT,#input$TEXT_RIGHT, 
               gaps = data$CODAGE_MANQUANT$GAP,#input$TEXT_GAP, nr = "RMA",
               id = row.names( data$SEQ_OBJ[row.names(data$SEQ_OBJ)%in%vec.sample , ] ))->trajsforclass
        
      } else {
        if(input$selection_rows=="unique.traj"){
          #### unique.traj ####
          #rev(wesanderson::wes_palette(name = "Darjeeling1", n = length(alphabet(data$SEQ_OBJ)), type = "continuous"))->cpal.seq
          #cpal(seqdata = data$SEQ_OBJ)<-cpal.seq
          seqtab(data$SEQ_OBJ, idxs = 0, format = "STS")->unique.trajs
          data.frame(unique.trajs)->unique.trajs.df
          
          seqdef(data = unique.trajs.df, weights = attributes(unique.trajs)$freq$Percent, 
                 #gaps = data$CODAGE_MANQUANT$GAP,
                 #right = data$CODAGE_MANQUANT$RIGHT,
                 #left = data$CODAGE_MANQUANT$LEFT, 
                 nr = attributes(data$SEQ_OBJ)$nr)->trajsforclass
          print(length(attributes(data$SEQ_OBJ)$cpal))
          cpal(seqdata = trajsforclass)<-attributes(data$SEQ_OBJ)$cpal
        } else {
          if(input$selection_rows=="all"){
            data$SEQ_OBJ->trajsforclass
          }
        }
      }
    message("coucou 259")
    message(dim(trajsforclass))
    return(trajsforclass)
    })
  
  output$TABLE_ECH<-renderDataTable({
    if(input$selection_rows=="Sample"){
    as.data.frame.array(
      t(rbind(
    "Effectifs"=table(selected_react())->tab1,
    "Poids"=round(prop.table(tab1), 4)
    )
    ))->df
    DT::datatable(df)
    } else {
      if(input$selection_rows=="unique.traj"){
        seqtab(data$SEQ_OBJ, idxs = 0, format = "STS")->unique.trajs
        data.frame(unique.trajs)->unique.trajs.df
        DT::datatable(unique.trajs.df)
        
      }
    }
  })
  
  ####
  #### NOMBRE DE TRAJECTOIRES: TOTAL ET  SELECTIONNEES  ####
  NB_TRAJS<-shiny::reactive({
    nrow(data$SEQ_OBJ)
  })
  unique.trajs<-shiny::reactive({
    seqtab(data$SEQ_OBJ[ , ], idxs = 0, format = "STS")
  })
  
  NB_UNIQUE_TRAJS<-shiny::reactive({
    length(attributes(unique.trajs())$row.name)
  })
  
  renderUI(expr = tags$sub(
    paste("Pour information, il y a",NB_TRAJS(), "trajectoires, et",   NB_UNIQUE_TRAJS(), "trajectoires uniques dans le jeu de données",sep=" "))
  )->output$TEXT_NB_UNIQUE_TRAJS
  
  NB_SELECT_TRAJS<-shiny::reactive({
    nrow(trajs.forclass())
  })
  
  renderUI(expr = tags$sub(
    paste("Vous avez sélectionné",NB_SELECT_TRAJS(), "trajectoires", sep=" "))
  )->output$TEXT_NB_SELECT_TRAJS
  
  
  output$TRAJS_FOR_CLASS_CONTROL<-renderUI({
    message("COUCOU 223")
    h3(dim(trajs.forclass()))
  })
  
  
  
  # observe({
  #   req(trajs.forclass())
  #   print("message 262")
  #   
  #   print(dim(trajs.forclass()))
  #   print(head(trajs.forclass()))
  #   
  # })
  
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
  
  #### COST AND DIST ####
  #### -> INPUTS ####
  ##### seqcost inputs #####
  observeEvent(eventExpr = input$method_edit_cost, {
    shiny::renderUI({
  edit.cost.inputs.local<-list(
    "cval"=shiny::numericInput(
                        label = "Coûts de substitution: rapport aux coûts indel (1)", 
                       inputId = ns("subst_ratio"), 
                       min = 0.1, max = 5, step = 0.1, value = 2, width = "20%"),
    "time.varying"=shiny::checkboxInput(inputId=ns("time_varying_substitution_costs"), 
                                 label="Les taux de transitions sont-ils dépendants du temps?", 
                                 value = FALSE, width = NULL),
    "transition"=shiny::selectInput(inputId = ns("transition_substitution_costs"), label = "Type de transition", choices = c("previous" , "next", "both"), selected = "both", multiple = FALSE),
    "lag"=shiny::numericInput(inputId = ns("lag_subst_cost"), label ="Pas de temps pour le calcul des taux de transition", value = 1, min = 1, max = 36, step = 1)
  )
  cost.args[[input$method_edit_cost]]->arg2
  return(edit.cost.inputs.local[names(edit.cost.inputs.local)%in%arg2])
    })->output$SEQCOST_INPUTS
  })
  ##### seqdist inputs ######
  shiny::renderUI({
  seqdist.inputs<-list(
    "norm"=selectInput(inputId = ns("norm_seqdist"), label = "seqdist(norm = )", 
                       choices = c("none" , "auto","maxlength", "gmean", "maxdist", "YujianBo"), 
                       selected = "none", multiple = FALSE),
    "refseq"=list(
      shiny::checkboxInput(inputId = ns("refseq_LOG_seqdist"), 
                           label = "Calculer les distance par rapport à une trajectoire de référence?",value = FALSE),
      conditionalPanel(condition = "input.refseq_LOG_seqdist == 1", ns = ns, 
                       shiny::numericInput(inputId = ns("refseq_seqdist"), 
                                           label ="Trajectoire de référence", 
                                           value=0, min = 0, step = 1))),
    "expcost"=shiny::numericInput(inputId = ns("expcost_seqdist"), label ="Coût de modification de la taille des séquences", value = 0.5, min = 0, step = 0.1),
    "context"=shiny::numericInput(inputId = ns("context_seqdist"), label ="Coût d'insertion (default: 1-2*expcost)", value = 1-2*0.5, min = 0, step = 0.1)
  )
  seqdist.args[[input$type_distance]][[input$classtype]]->arg2
  seqdist.inputs[names(seqdist.inputs)%in%arg2]
  })->output$SEQDIST_INPUTS
  
  #### CALCUL COUTS ####
  
  SEQCOST<-eventReactive(eventExpr = input$calculCouts, {
    req(trajs.forclass())
    print("COUCOU 428")
    seqcost(seqdata=trajs.forclass(),
            method = input$method_edit_cost,
            cval = input$subst_ratio,
            time.varying=input$time_varying_substitution_costs,
            transition=input$transition_substitution_costs,
            lag=input$lag_subst_cost, weighted = TRUE, with.missing = FALSE)->cost
    return(cost)
  })
  observe({print(SEQCOST())})
  #
  #    #### PRINT COSTS ####
  output$PRINTINDEL<-renderUI({
    req(SEQCOST())
    SEQCOST()$indel->the.indels
    if(length(the.indels)>1){
      the.indels<-data.frame("Etats"=alphabet(trajs.forclass()), "Cout(s)_INDEL"=round(the.indels, 2))
      DT::renderDataTable(the.indels)->output$bb
      dataTableOutput(outputId = ns("bb"), width = "80%")
    } else {
      renderText(as.character(the.indels))->output$bb
      textOutput(ns("bb"))
    }
  })
  #
  output$PRINTSUBST<-renderUI({
    req(SEQCOST())
    SEQCOST()$sm->the.sm
    if(class(the.sm)=="matrix"){
      DT::renderDataTable(the.sm)->output$aa
      dataTableOutput(outputId = ns("aa"))
    } else {
      renderPrint(the.sm)->output$aa
      shiny::verbatimTextOutput (outputId = ns("aa"))
    }
  })
  
  
  ##### SEQDIST CALC #####
  #### SEQDIST  ####
  output$PRINTTIMEDIST<-renderUI({
    
    
    predict.time.dist<-function(df=calculated.times.for.dist, nb.sequences=10000){
        library(stats)
        fit_nls <- nls(time ~ a*(amplit ^ b), data = df[ , ], start =  c(a=0.5, b = 1), trace = F, control=nls.control(maxiter=1000))
        pp<-ggplot(data = df)+geom_point(aes(x=amplit, y=time))+
          geom_line(aes(y=predict(fit_nls, newdata = data.frame(amplit = amplit)), x=amplit))
        #
        fit_nls$m$predict(newdata = data.frame(amplit=1:nb.sequences))->c.predict
        nb.secondes<-round(c.predict[nb.sequences]/1000, 2)
        nb.minutes<-round(nb.secondes/60, 2)
        message(paste("Nombre de secondes pour", nb.sequences, "trajectoires :", nb.secondes, sep=" "))
        message(paste("\n nombre de minutes :",  nb.minutes, sep=" "))
        message(paste(sep=" ", "Estimation réalisée sur", nrow(calculated.times.for.dist), "lancements de seqdist()", "pour un nombre de trajectoires allant de", min(df$amplit), "à", max(df$amplit)))
        res<-list("nb.secondes"=nb.secondes, "nb.minutes"=nb.minutes, "pp"=pp)
        return(res)
      }
    
    req(NB_SELECT_TRAJS())
    predict.time.dist(nb.sequences = NB_SELECT_TRAJS())->pred.data
    paste("Pour", NB_SELECT_TRAJS(), "trajectoires, le temps de calcul estimé pour la fonction seqdist() est de", pred.data$nb.secondes, "secondes, soit", pred.data$nb.minutes, "minutes")->thetext
    renderText(thetext)->output$thetext
    textOutput(ns("thetext"))
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
      textOutput(ns("cc")) #%>% withSpinner(color="#0dc5c1")
    })
  })
  #### CLASSIFICATION   ####
  #### 
  #### CREATION DUE LA CLASSIFICATION
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
  
  #### PLOT DENDOGRAM
  #observe({
  #  input$calculCLUST
    
  #isolate({
    output$classif<- renderUI({
      input$calculCLUST
    #isolate({
      req(SEQCLASS())
      if (input$cluster_type=="CAH" | input$cluster_type=="fastCAH"){
        renderPlot(plot(SEQCLASS(), which.plots = 2))->output$dd
        return(plotOutput(ns("dd")) %>% withSpinner(color="#0dc5c1"))
      }
      if(input$cluster_type=="CAHPAM"){
        output$dendo<-renderPlot({
          plot(as.dendrogram(SEQCLASS()))
        })
        output$inertie<-renderPlot({
          plot(sort(SEQCLASS()$height, decreasing=TRUE)[1:20], type="s", xlab="nb de classes", ylab="inertie")
        })
        res<-tagList(
          shiny::sliderInput(inputId = ns("SliderGrp"),label="Nombre de groupes",min=2,max=10,value = c(4,6),step=1),
          fluidRow(column(12,
                          splitLayout(
                            plotOutput(ns("dendo")) %>% withSpinner(color="#0dc5c1"),
                            plotOutput(ns("inertie")) %>% withSpinner(color="#0dc5c1"))))
          
        )
        
        return(res)
        
      }
    #})
    
  })
  #})
  
  
  #### Tableau indicateurs pour évaluer la qualité des classifiactions testées ####
  
  output$tabind<-renderUI({
    req(SEQCLASS(),input$SliderGrp)
    input$calculCLUST
    input$SliderGrp
    isolate({
      if(input$cluster_type=="CAHPAM"){
        indicateur<-Creation_indicateur(nb_cluster_min=min(input$SliderGrp,na.rm=TRUE),
                                        nb_cluster_max=max(input$SliderGrp,na.rm=TRUE),
                                        mat_dist=SEQDIST(),intialclust=SEQCLASS())
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
                 shiny::numericInput(inputId = ns("nb_cluster"),label="Nombre de groupes choisi",step=1,value = min(input$SliderGrp,na.rm=TRUE)+1,min=min(input$SliderGrp,na.rm=TRUE),max=max(input$SliderGrp,na.rm=TRUE))
          ),
          column(2,
                 shiny::actionButton(inputId = ns("Bouton_Clustering"),label = "Faire les groupes")
          )
        ))

      }
      if(input$cluster_type=="CAH"){
        return(tagList(
          column(2,
                 shiny::numericInput(inputId = ns("nb_cluster"),label="Nombre de groupes choisi",step=1,value = 2,min=2 ,max=10)
          ),
          column(2,
                 shiny::actionButton(inputId = ns("Bouton_Clustering"),label = "Faire les groupes")
          )
        ))
      }
      
      if(input$cluster_type=="fastCAH"){
        return(tagList(
          column(2,
                 shiny::numericInput(inputId = ns("nb_cluster"),label="Nombre de groupes choisi",step=1,value = 2,min=2 ,max=10)
          ),
          column(2,
                 shiny::actionButton(inputId = ns("Bouton_Clustering"),label = "Faire les groupes")
          )
        ))
      }
      
      
      
      
      
    })
  })
  
  
    # output$classif_grp<-renderUI({
  #   req(SEQCLASS())
  #   input$calculCLUST
  #   input$SliderGrp
  #   isolate({
  #     if (input$cluster_type=="CAHPAM"){
  #       req(input$SliderGrp)
  #       return(tagList(
  #         column(2,
  #                shiny::numericInput(inputId = ns("nb_cluster"),label="Nombre de groupes choisi",step=1,value = min(input$SliderGrp,na.rm=TRUE)+1,min=min(input$SliderGrp,na.rm=TRUE),max=max(input$SliderGrp,na.rm=TRUE))
  #         ),
  #         column(2,
  #                shiny::actionButton(inputId = ns("Bouton_Clustering"),label = "Faire les groupes")
  #         )
  #       ))
  #       
  #     }
  #     if(input$cluster_type=="CAH"){
  #       return(tagList(
  #         column(2,
  #                shiny::numericInput(inputId = ns("nb_cluster"),label="Nombre de groupes choisi",step=1,value = 2,min=2 ,max=10)
  #         ),
  #         column(2,
  #                shiny::actionButton(inputId = ns("Bouton_Clustering"),label = "Faire les groupes")
  #         )
  #       ))
  #     }
  #   })
  # })
  
  
  DATA_COMPc<-eventReactive(eventExpr = input$calculDist,{
    
    if(input$selection_rows=="Sample"){
      lapply(data$DATA_COMP, function(dat.i){
        subset(dat.i, dat.i[ , data$ID_VAR]%in%row.names(trajs.forclass()))
      })->DATA_COMPc
      return(DATA_COMPc)
    } else  {
    return(data$DATA_COMP)
    }
    
  })
  
  
  
  dataCluster<-eventReactive(eventExpr = input$Bouton_Clustering,{
    
    req(SEQCLASS())
    
    
    if (input$cluster_type=="CAHPAM"){
      
      indicateur<-Creation_indicateur(nb_cluster_min=min(input$SliderGrp,na.rm=TRUE),
                                      nb_cluster_max=max(input$SliderGrp,na.rm=TRUE)
                                      ,mat_dist=SEQDIST(),
                                      intialclust=SEQCLASS())
      
      data_cluster(tabl_ind = indicateur, data =  trajs.forclass(), nb_groupe = input$nb_cluster)->dataclusts
      data.frame(dataclusts)->dataclusts
      dataclusts$ID<-row.names(dataclusts)
      
      lapply(X = DATA_COMPc(), function(x){
        base::merge(x, dataclusts[ , c("ID", "Clustering")], by.x=data$ID_VAR, by.y="ID", all.x=TRUE)
      })->DATA_COMPc2
      #return(
      #lapply(X = DATA_COMPc(), function(x){data_cluster(tabl_ind = indicateur, data = x, nb_groupe = input$nb_cluster)   })->DATA_COMPc2
      #data_cluster(indicateur,
      #             ,#DATAs()$DATA_COMP,
      #             input$nb_cluster)->res_def
    } else {
      if(input$cluster_type=="CAH"|input$cluster_type=="fastCAH"){
        
        clusterCAH<-as.factor(cutree(SEQCLASS(),k = input$nb_cluster))
        clusterCAH.class<-factor(clusterCAH,labels = paste0("G",1:input$nb_cluster))
        clusterCAH.class<-as.character(clusterCAH.class)
        
        if(input$selection_rows=="Sample"){
          
          data.frame("ID"=row.names(trajs.forclass()), "Clustering"=clusterCAH.class)->df
          
          lapply(X = DATA_COMPc(), function(x){
            merge(x, df, by.x=data$ID_VAR, by.y="ID", all.x=TRUE)
          })->DATA_COMPc2
          
        } else {
          if(input$selection_rows=="all"){
            data.frame("ID"=row.names(data$SEQ_OBJ), "Clustering"=clusterCAH.class)->df
            lapply(X = DATA_COMPc(), function(x){
              merge(x, df, by.x=data$ID_VAR, by.y="ID")
            })->DATA_COMPc2
            
          } else {
            if(input$selection_rows=="unique.traj"){
              
              data.frame("ID"= sapply(1:nrow(unique.trajs()), function(i){paste(unique.trajs()[i , ], collapse = "-")}), 
                         "Clustering"=clusterCAH.class)->df
              
              data.frame("IDVAR"=row.names(data$SEQ_OBJ), 
                         "ID"=sapply(1:nrow(data$SEQ_OBJ), function(i){paste(data$SEQ_OBJ[i , ], collapse = "-")})
              )->df2
              
              merge(df2, df, by.x="ID", by.y="ID")->df3
              lapply(X = DATA_COMPc(), function(x){
                merge(x, df3, by.x=data$ID_VAR, by.y="IDVAR")
              })->DATA_COMPc2
              
            }
          }
        }
      }
      
      
    }
    ####
    lapply(DATA_COMPc2, function(xdf){
      xdf[]<-lapply(X = xdf, as.character)#data.frame(lapply(x, function(x){if(is.character(x)){as.factor(x)} else {x}}))
      xdf
      })->res_def
    #DATA_COMPc2->res_def
    return(res_def)
  })
  
  observe({print(dataCluster())})
  observe({print(dim(dataCluster()))})
  observe({print(class(dataCluster()))})
  observe({for(ji in dataCluster() ){ print(dim(ji) ) } })
  
  
  trajs.forclass.output<-reactive({
    if(input$selection_rows=="Sample"){
      trajs.forclass()
    } else {
      data$SEQ_OBJ
    }
  })
  

  res<-reactive({
    req(trajs.forclass.output())
    req(dataCluster())
    list("SEQ"=trajs.forclass.output(), "DATA"=dataCluster())->res
    res
  })
  
  output$DOWNSEQ <- downloadHandler(
    filename =  function(){
      paste("mes_trajectoires_clustering.RData")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      dat<-res()
      shiny::withProgress(
        message = "Veuillez patienter, le téléchargement des données est en cours",
        value = 0,
        {
          save(dat, file=file)
          shiny::incProgress(1)
        })
    } 
  )
  
  
  
  
  return(
    list("SEQ_OBJ"=reactive({trajs.forclass.output()}), 
         "DATA_COMP"=reactive({dataCluster()}), 
         "TYPE_SOURCE"=reactive({data$TYPE_SOURCE}), 
         "CODAGE_MANQUANT"=reactive({data$CODAGE_MANQUANT}),
         "ID_VAR"=reactive({data$ID_VAR})
    )
  )
  
}