#' @title   select_and_plot
#' @description  A shiny Module qui permet de sélectionner des données et de représenter les trajectoires
#' @param id shiny id
#' @param label fileInput label
#' @importFrom stringr str_detect
#' @export
module_select_and_plot2_UI <- function(id){#label = "CSV file") {
  ns <- NS(id)

    
#  shiny::fluidRow(
    fluidPage(
    tabsetPanel(id = ns("tabs1"), type = "pills",
                
                #shinyjs::useShinyjs(), 
                #id=ns("form"),
                #shiny::actionButton(inputId = ns("refresh"), label = "Réinitialiser la sélection"),
                #hr(),
                
                tabPanel(title = "Sélection des données et des indicateurs : ", 
  #div(
  #  id="form",
  shiny::column(width=6,
  shiny::selectInput(inputId = ns("plottype"), label = "Quel graphique voulez-vous représenter? ", 
					choices = c("Taux de transition [seqtrate()]"="txtr",				  
                     "Chronogramme [seqplot(type = 'd')] "="d", "Séquences les plus fréquentes [seqplot(type = 'f')] "="f", 
                                 "Tapis [seqplot(type = 'I')] "="I", "Etat modal [seqplot(type = 'ms')] "="ms", 
                                 "Durée moyenne dans chaque état [seqplot(type = 'mt')] "="mt",
                                 "Graphique d'entropie [seqplot(type = 'Ht')] "="Ht", 
                                 "Graphique de flux"="flux",
                                 "Sous-séquences  [seqefsub()] "="sous.seq",
                                 "Séquences représentatives  [seqplot(type = 'r')] "="r"
                                 ), selected = "txtr", multiple = FALSE),
  shiny::conditionalPanel(condition = "input.plottype=='I'", ns = ns, 
                          selectInput(inputId = ns("tapis_order"), label = "Rangement des séquences : ", 
                                      choices = c("Depuis le début"="from.start", "Depuis la fin"="from.end"), 
                                      selected = "from.start", multiple = FALSE)
                          ),
  uiOutput(ns("DATE_server_created")),
  shiny::conditionalPanel(condition = "input.DATE_SELECT_M1!='Pas de groupes'", ns = ns, 
                          uiOutput(ns("VAR_server_created")),
                          uiOutput(ns("CLASS_SELECT_server_created")),
                          uiOutput(ns("MOD_SELECT_server_created"))
  )
  ),
  shiny::column(width=6,
  uiOutput(ns("SELECT_COL_TIME_FLUX")),
  uiOutput(ns("SELECT_PARAM_TXTR")),					
  uiOutput(ns("SELECT_EVENTS")),
  uiOutput(ns("MERGED_SELECTED_mod")),
  uiOutput(ns("WARNING_LENGTH"))
  )
  #)
  ),
  tabPanel(title = "Paramètres d'affichage du graphique",
          uiOutput(ns("SELECT_THEMES_UI")),
          shiny::numericInput(inputId = ns("size_text_plot"), label = "Correction de la taille des éléments de texte", min = -10, max = 10, step = 0.5, value = 0)
           
  )
  ),
  hr(),
  shiny::column(width=12,
  actionButton(inputId = ns("UPDATE_PLOT"), label = "Mettre à jour le graphique et les données")
  ),
  shiny::column(width=12,
  #plotOutput(ns("plot1"))%>% withSpinner(color="#0dc5c1")
  uiOutput(ns("plot1_UI")) %>% withSpinner(color="#0dc5c1"),
  downloadButton(outputId=ns("DownGraphGlobal"),label="Télécharger le graphique")
  
  ),
  shiny::column(width=12,
                
  uiOutput(ns("DATA_r"))%>% withSpinner(color="#0dc5c1")),
  shiny::column(width=6,
                
  shiny::radioButtons(inputId = ns("TypeFichierDownload"), label = "Extension du fichier",choices=c("csv2","csv"),selected = "csv"),
  helpText("'csv' : format .csv standard | 'csv2' :  pour ouverture avec excel")
  ),
  shiny::column(width=6,
  downloadButton(outputId = ns('ButtondownloadData'), 'Télécharger les données')
  )
  
  )
  
  }
#' server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#' @param obj.seq
#' @param data.comp
#' @importFrom utils read.csv
#' @importFrom glue glue
#' @export
#' @rdname mod_csv_fileInput
module_select_and_plot2 <- function(input, output, session, data) {
  #data<-list()
  #data$DATA_COMP<-data$DATA_COMP()
  #data$SEQ_OBJ<-data$SEQ_OBJ()
  #data$TYPE_SOURCE<-data$TYPE_SOURCE()
  #data$CODAGE_MANQUANT<-data$DATA_COMP()
  #data$ID_VAR<-data$ID_VAR()
  
  #### PARAMETERS ####
  library(dplyr)
  library(TraMineR)
  library(RColorBrewer)
  library(ggplot2)
  library(ggthemes)
  ns <- session$ns
  ####
  getNamespaceExports("ggthemes")->funkggthemes
  funkggthemes[grepl(pattern = "theme_", x = funkggthemes, fixed = TRUE)]->funkggthemes
  funkggthemes<-funkggthemes[grepl(pattern = "theme_", x = funkggthemes, fixed = TRUE)&!funkggthemes%in%c("theme_solid", 
                                                                                                          "theme_update", 
                                                                                                      "theme_get",  
                                                                                                      "theme_set" , 
                                                                                                      "theme_replace" ,  
                                                                                                      "theme_test")]
  paste("ggthemes::", funkggthemes, "()", sep = "")->funkggthemes
  ###
  getNamespaceExports("ggplot2")->funkggplot2
  funkggplot2[grepl(pattern = "theme_", x = funkggplot2, fixed = TRUE)]->funkggplot2
  
  funkggplot2<-funkggplot2[grepl(pattern = "theme_", x = funkggplot2, fixed = TRUE)&!funkggplot2%in%c("theme_update", 
                                                                                         "theme_get",  
                                                                                         "theme_set" , 
                                                                                         "theme_replace" ,  
                                                                                         "theme_test")]
  paste("ggplot2::", funkggplot2, "()", sep = "")->funkggplot2
  
  vec.of.themes<-c(
    funkggthemes,
    funkggplot2
  )
  
  output$SELECT_THEMES_UI<-renderUI({
    shiny::selectInput(inputId = ns("theme_select"), label = "Thème (ggplot2 ou ggthemes)",
                       choices = c("choix par défaut", vec.of.themes), selected = NULL, multiple = FALSE)
  })
  
 #return(reactive(class(data() )))
  #renderPlot({
  
  #observeEvent(input$refresh, {
  #  shinyjs::reset("tabs1")
  #})
  
  
  output$DATE_server_created<-renderUI({
    ns <- session$ns
    shiny::selectInput(inputId=ns("DATE_SELECT_M1"), label = "Date pour groupes : ", 
                       choices = c("Pas de groupes", names(data$DATA_COMP() )), 
                       selected = "Pas de groupes", multiple = FALSE)
  })
  reactive({
    req(input$DATE_SELECT_M1)
    message("COUCOU 50")
    print(input$DATE_SELECT_M1)
    if(input$DATE_SELECT_M1!="Pas de groupes"){
      data$DATA_COMP()[[as.character(input$DATE_SELECT_M1)]]
    } else {
      data$DATA_COMP()[[1]]
    } 
    })->the.df
  
  
  output$VAR_server_created<-renderUI({
    ns <- session$ns
    req(the.df())
    shiny::selectInput(inputId=ns("VAR_SELECT_M1"), label = "Variable pour groupes : ", 
                       choices = c("Pas de groupes", names(the.df())), 
                       selected = "Pas de groupes", multiple = FALSE)
  })
  
  reactive({
    req(input$VAR_SELECT_M1)
    req(the.df())
    #
    message("Coucou 75.1")
    if(input$VAR_SELECT_M1!="Pas de groupes"){
    the.df()[ , input$VAR_SELECT_M1]
    }
  })->THE_VAR
  
  reactive({
    req(THE_VAR())
    #
    message("Coucou 75.2")
    
    class(THE_VAR())
  })->CLASS_THE_VAR
  
  
  
  output$CLASS_SELECT_server_created<-renderUI({
    ns <- session$ns
    req(THE_VAR())
    req(CLASS_THE_VAR())
    message("Coucou 75")
   
    shiny::selectInput(inputId = ns("classSelect"), label = "Contrôle de la classe", 
                       choices =  c("factor", "character", "numeric", "Date", "integer"), 
                       selected = CLASS_THE_VAR())
    
  })
  
  
  
  observeEvent(input$classSelect, {
  output$MOD_SELECT_server_created<-renderUI({
    ns <- session$ns
    req(input$VAR_SELECT_M1)
    req(the.df())
    req(input$classSelect)
    if(input$VAR_SELECT_M1!="Pas de groupes"){
      
    the.df()[ , input$VAR_SELECT_M1]->THE_VAR
    message("COUCOU 94")
    
    if(input$classSelect%in%c("numeric", "integer")){
      message("COUCOU 94: numeric")
      
      as.numeric(THE_VAR)->temp.num.var
      
      minis <- min(temp.num.var, 
                   na.rm = TRUE)
      maxis <- max(temp.num.var, 
                   na.rm = TRUE)
      
      
      shiny::sliderInput(inputId = ns("NumSelect"), label = "Valeurs sélectionnées", 
                         min = minis, max = maxis, 
                         value = c(minis, maxis))
    } else {
      if(input$classSelect=="factor"){#, "character") ){
        message("COUCOU 94: factor")
        
        if(length(unique(THE_VAR))>100){
          table(THE_VAR)->tab
          tab[order(tab, decreasing = TRUE)]->tab
          tab[1:25]->tab
        } else {tab<-unique(THE_VAR )}
        shiny::selectInput(inputId = ns("FactSelect"), label="Valeurs sélectionnées", 
                           choices = tab  , multiple = TRUE)
      } else {
        if(input$classSelect=="character"){#, "character") ){
          message("COUCOU 94: char")
          
          shiny::textInput(inputId = ns("CharPatSelect"), label="'Paterns' à rechercher (sep by '/'", value = ""
          )
        } else  {
          if(input$classSelect=="Date" ){
            message("COUCOU 94: date")
            
            renderText(print( unique(THE_VAR )[!is.na(unique(THE_VAR) )][1]  ) )->output$AFFICHDATE
            list(
              column(width=4, 
                     h4("Format de la date dans les données:"),
                     textOutput(ns("AFFICHDATE"))),
              column(width=4,
                     h4("Spécification du format de date: "),
                     h5("Voir ?strptime pour plus de détails"),
                     h5("Exemple :  01/01/1900 => %d/%m/%Y")),
              column(width=4, 
                     shiny::textInput(inputId = ns("DATEformat"), label = "Format d'origine", value = "" )),
              shiny::dateRangeInput(inputId = ns("DATE_RANGE"), 
                                    label = "Bornes des dates : ")#, format = "%d/%m/%Y" )#, format = input$DATEformat)
            )
          }
        }
      }
    }
    }
  })
  })
  
  
  
  observe({
    req(input$VAR_SELECT_M1)
    req(the.df())
    req(input$classSelect)
    
    if(input$classSelect=="Date"){
      
    message(" COUCOU : print(input$VAR_SELECT_M1)")
    print(input$VAR_SELECT_M1)
    req(input$DATEformat)
    
    if(input$VAR_SELECT_M1!="Pas de groupes"){
    the.df()[ , input$VAR_SELECT_M1]->THE_VAR
      message("COUCOU : the.df()[ , input$VAR_SELECT_M1]->THE_VAR")
      #print(THE_VAR)
    if(class(THE_VAR)=="Date"){
      THE_VAR->THE_VAR_DATE
    } else {
      if(class(THE_VAR)=="character"){
    
        as.Date(THE_VAR, format=as.character(input$DATEformat) )->THE_VAR_DATE
      } else {
        if(class(THE_VAR)=="numeric"|class(THE_VAR)=="integer"){
          as.POSIXct(THE_VAR,origin="1970-01-01")->THE_VAR_DATE
        }
      }
    }
    min(THE_VAR_DATE, na.rm = TRUE)->mindate
    max(THE_VAR_DATE, na.rm = TRUE)->maxdate
    shiny::updateDateRangeInput(session = session, inputId = "DATE_RANGE", start = mindate, end=maxdate)
    }
    }
  })
  
  reactive({
    
    #req(input$VAR_SELECT_M1)
    #req(the.df())
    
    
    message("Hello")
    "a"
    
    })->testi
  
  observe({
    print(testi())
  })
  
  reactive({
    req(input$VAR_SELECT_M1)
    req(the.df())
    
    print("ON EST LA!!!")
    message("COUCOU 1")
    #print(head(the.df()))
    if(input$DATE_SELECT_M1=="Pas de groupes"){
      the.df()->the.df.for.groupe
      the.df.for.groupe$Ensemble<-"Ensemble"
      ordered.df<-the.df.for.groupe[match(row.names(data$SEQ_OBJ()), the.df.for.groupe[ , data$ID_VAR()]) , ]#order(the.df.for.groupe[ , 1][order(row.names(data$SEQ_OBJ))]) , ]
      grups<-ordered.df[ , "Ensemble"]
    } else {
      if(input$VAR_SELECT_M1=="Pas de groupes"){
        the.df()->the.df.for.groupe
        the.df.for.groupe$Ensemble<-"Ensemble"
        ordered.df<-the.df.for.groupe[match(row.names(data$SEQ_OBJ()), the.df.for.groupe[ , data$ID_VAR()]) , ]#order(the.df.for.groupe[ , 1][order(row.names(data$SEQ_OBJ))]) , ]
        grups<-ordered.df[ , "Ensemble"]
      } else {
      #if(input$UserSelect==TRUE){
   #   if(input$UserSelect==FALSE){#&length(the.df()[ , data()$ID_VAR])<20){
        the.df()[ , c(data$ID_VAR(), input$VAR_SELECT_M1)]->the.df.for.groupe
        subset(the.df.for.groupe, 
               as.character(the.df.for.groupe[ , data$ID_VAR()])%in%row.names(data$SEQ_OBJ()))->the.df.for.groupe
        #####
        if(input$classSelect=="character"){
          if(length(unique(the.df.for.groupe[ , input$VAR_SELECT_M1]))<20|length(input$CharPatSelect)<20){
            
          subset(the.df.for.groupe, 
                 grepl(pattern =  input$CharPatSelect, x = as.character(the.df.for.groupe[ , input$VAR_SELECT_M1]), fixed = TRUE))->the.df.for.groupe
          the.df.for.groupe$VARgrup<-the.df.for.groupe[ , input$VAR_SELECT_M1]
          }
        } else {
          if(input$classSelect=="factor"){
            if(length(unique(the.df.for.groupe[ , input$VAR_SELECT_M1]))<20){
            if(length(input$FactSelect)>=1){
              subset(the.df.for.groupe, 
                   as.character(the.df.for.groupe[ , input$VAR_SELECT_M1])%in%as.character(input$FactSelect))->the.df.for.groupe
              the.df.for.groupe$VARgrup<-as.character(the.df.for.groupe[ , input$VAR_SELECT_M1])
              
            } else {
              the.df.for.groupe$VARgrup<-as.character(the.df.for.groupe[ , input$VAR_SELECT_M1])
            }		
            } else {
			the.df.for.groupe$VARgrup<-"HORS SELECTION"}
          } else {
            if(input$classSelect=="numeric"|input$classSelect=="integer"){
              subset(the.df.for.groupe, 
                     as.numeric(the.df.for.groupe[ , input$VAR_SELECT_M1])>=input$NumSelect[1]&as.numeric(the.df.for.groupe[ , input$VAR_SELECT_M1])<=input$NumSelect[2])->the.df.for.groupe
              the.df.for.groupe$VARgrup<-"SELECTION"
              
            } else {
              if(input$classSelect=="Date"){
                ##
                if(class(the.df.for.groupe[ , input$VAR_SELECT_M1])=="Date"){
                  the.df.for.groupe[ , input$VAR_SELECT_M1]->THE_VAR_DATE
                } else {
                  if(class(the.df.for.groupe[ , input$VAR_SELECT_M1])=="character"){
                    
                    as.Date(the.df.for.groupe[ , input$VAR_SELECT_M1], format=as.character(input$DATEformat) )->THE_VAR_DATE
                  } else {
                    if(class(the.df.for.groupe[ , input$VAR_SELECT_M1])=="numeric"|class(the.df.for.groupe[ , input$VAR_SELECT_M1])=="integer"){
                      as.POSIXct(the.df.for.groupe[ , input$VAR_SELECT_M1],origin="1970-01-01")->THE_VAR_DATE
                    }
                  }
                }
                ##
                ##
                subset(the.df.for.groupe, 
                       THE_VAR_DATE>input$DATE_RANGE[1]&THE_VAR_DATE<input$DATE_RANGE[2])->the.df.for.groupe
                the.df.for.groupe$VARgrup<-"SELECTION"
                
              }
            }
          }
        }
        
        message("COUCOU 62")
        print(head(the.df.for.groupe))
        row.names(data$SEQ_OBJ())[!row.names(data$SEQ_OBJ())%in%as.character(the.df.for.groupe[ , data$ID_VAR()])]->id.not.in.groupes
        if(length(id.not.in.groupes)>=1){
          data.frame(
            "id"=id.not.in.groupes, 
            "VAR"=NA,
            "VARgrup"="HORS SELECTION")->comdf
          names(comdf)<-names(the.df.for.groupe)
          message("COUCOU 68")
          print(head(comdf))
          data.frame(
            rbind(
              the.df.for.groupe, 
              comdf
            ), stringsAsFactors = FALSE)->the.df.for.groupe
        }
        message("COUCOU 250")
        
        #print(head(the.df.for.groupe))
        ordered.df<-the.df.for.groupe[match(row.names(data$SEQ_OBJ()), the.df.for.groupe[ , 1]) , ]#order(the.df.for.groupe[ , 1][order(row.names(data$SEQ_OBJ))]) , ]
        grups<-ordered.df[ , "VARgrup"]
   ###########################################
  
      #} else {
       # the.df()[ , c(data$ID_VAR, input$VAR_SELECT_M1)]->the.df.for.groupe
      #  message("COUCOU 59")
      #  print(head(the.df.for.groupe))
      #  subset(the.df.for.groupe, 
      #         as.character(the.df.for.groupe[ , data$ID_VAR])%in%row.names(data$SEQ_OBJ))->the.df.for.groupe
      #  ordered.df<-the.df.for.groupe[match(row.names(data$SEQ_OBJ), the.df.for.groupe[ , data$ID_VAR]) , ]#order(the.df.for.groupe[ , 1][order(row.names(data$SEQ_OBJ))]) , ]
      #  grups<-ordered.df[ , input$VAR_SELECT_M1]
        ###############################
       
      #}
      }
    }
    #}
  ###############
        #if(input$UserSelect==TRUE){
          
    #    }
        #}
        #
        #
    if(!is.null(input$merge_moda)){
      if(input$merge_moda==TRUE){
        grups<-as.character(grups)
        grups[grups!="HORS SELECTION"]<-"SELECTION"
      }
    }
    
    grups<-as.character(grups)
    
    return(grups)
    })->the.grups
  
  
  output$WARNING_LENGTH<-renderUI({
    if(length(unique( the.grups()  ))>20){
      h4("ATTENTION: il y a plus de 20 valeurs uniques dans la sélection. Cela risque de ralentir la production des graphiques et données et de bloquer l'appli. ")
    }
  })
  
  output$MERGED_SELECTED_mod<-renderUI({
    #req(input$VAR_SELECT_M1)
    #req(the.df())
    #req(input$classSelect)
	req(data)		 
    if(input$VAR_SELECT_M1!="Pas de groupes"){
      if(input$classSelect=="factor"|input$classSelect=="character"){
      shiny::checkboxInput(inputId = ns("merge_moda"), label = "Le modalités sélectionnées doivent-elles être fusionnées?", value = FALSE)
      }
    }
  })
  
  output$SELECT_PARAM_TXTR<-renderUI({
    if(input$plottype=="txtr"){
      list(
      shiny::numericInput(inputId = ns("PAStrate"), label = "Pas de temps pour le calcul des taux de transition",value = 1, min = 1, max = 36, step = 1),
      shiny::checkboxInput(inputId = ns("TYPEtrate"), label = "Les taux de transitions varient-ils avec le temps?",value = FALSE),
      shiny::checkboxInput(inputId = ns("TypeValeur"), label = "Afficher les effectifs plutôt que les pourcentages?",value=FALSE),
      shiny::checkboxInput(inputId = ns("DebArr"),label="Calculer les taux de transition en partant de la fin?", value=FALSE)
      )
    }
  })
  
  observe({
    req(input$plottype)
    if(input$plottype=="txtr"){
      
    updateNumericInput(session=session, inputId = "PAStrate",max=length(names(data$SEQ_OBJ()))-1)
      
    }
  })
  
    observe({
    req(input$pminsup )
    req(input$nb_event)
    seqecreate(data = data$SEQ_OBJ())->sese
    seqefsub(eseq = sese, pmin.support = input$pminsup )->sese.sub
    sese.sub$subseq[order(sese.sub$data$Count, decreasing = TRUE), ]->choices.sese
    shiny::updateSelectInput(session = session, inputId = "select_event", choices = choices.sese)
  })	
  
  output$SELECT_COL_TIME_FLUX<-renderUI({
    #req(input$VAR_SELECT_M1)
    #req(the.df())
    #req(input$classSelect)
    if(input$plottype=="flux"){
        shiny::selectInput(inputId = ns("select_col_time_flux"), 
                           label = "Temporalitées choisies pour le graphique de flux : ", 
                           choices = names(data$SEQ_OBJ()), selected = names(data$SEQ_OBJ())[1], multiple = TRUE)
      
    }
  })
  
  output$SELECT_EVENTS<-renderUI({
    if(input$plottype=="sous.seq"){
      list(
      shiny::numericInput(inputId = ns("nb_event"), 
                         label = "Nombre maximum d'états pour caractériser les transitions:", min = 1, max = 5, value = 2),
      shiny::numericInput(inputId = ns("pminsup"), 
                          label = "Support minimum :", min = 0, max = 0.95, value = 0.05, step = 0.05),
      shiny::selectInput(inputId = ns("select_event"), 
                          label = "Transitions choisies :", selected = NULL, multiple = TRUE, choices=NULL)
      )
      
    }
  }) 
  
  observe({
    req(input$pminsup )
    req(input$nb_event)
    seqecreate(data = data$SEQ_OBJ())->sese
    seqefsub(eseq = sese, pmin.support = 0.01 )->sese.sub
    
    shiny::updateNumericInput(session = session, inputId = "pminsup",max = max(sese.sub$data$Support))
  })
  
  observe({
    req(input$pminsup )
    req(input$nb_event)
    seqecreate(data = data$SEQ_OBJ())->sese
    seqefsub(eseq = sese, pmin.support = input$pminsup )->sese.sub
    sese.sub$subseq[order(sese.sub$data$Count, decreasing = TRUE), ]->choices.sese
    shiny::updateSelectInput(session = session, inputId = "select_event", choices = choices.sese)
  })
  
  ##### OUTPUTS #####
  
  
  tailleGraph<-reactiveValues(height=400)
  
  
  THE_PLOT<-eventReactive(input$UPDATE_PLOT, {
    message("COUCOU 372")
    print(isolate(the.grups()))
    if(length(unique(isolate(the.grups())))<20){
      p<-seqggplot(TYPE = isolate(input$plottype), 
		PAS.temps = isolate(input$PAStrate), 
		TIME.varying =isolate(input$TYPEtrate) , 
		Pourc.eff = isolate(input$TypeValeur), 
		Sens = isolate(input$DebArr),		
                   objseq = data$SEQ_OBJ(), 
                   groupes = isolate(the.grups()), 
                   merge_mods = isolate(input$merge_moda), 
                   col.selected = isolate(input$select_col_time_flux), pmin.sup = isolate(input$pminsup), str.subs = isolate(input$select_event), 
                   SORTV=isolate(input$tapis_order))
      if(inherits(x = p, what = "ggplot")){
        if(length(input$theme_select)>0){
          if(input$theme_select!="choix par défaut"){
            p<-p+eval(parse(text = input$theme_select))
         












		 }
        }
		
        
        if(!is.null(input$size_text_plot)&length(p$theme$text$size)>0){
          p<-p+theme(text = element_text(size = p$theme$text$size+input$size_text_plot))
        }
      }				  
    } else {NULL}
    return(p)
  })
  
  observeEvent(input$UPDATE_PLOT, {
    output$plot1_UI<-renderUI({
      
  
      if(isolate(input$plottype)=="txtr"){
       if(isolate(input$TYPEtrate)==TRUE){
         tailleGraph$height<-haut1(nb.time = ncol(isolate(data$SEQ_OBJ())), 
                                   type.of.plot = isolate(input$plottype ))
       } else {
         tailleGraph$height<-haut1(nb.gr = length(unique(isolate(the.grups() )) ), type.of.plot = isolate(input$plottype ))
         
       } 
      } else {
        tailleGraph$height<-haut1(nb.gr = length(unique(isolate(the.grups() )) ), type.of.plot = isolate(input$plottype ))
      }	  	   
      
      
      output$PLOTi<-renderPlot({ isolate(THE_PLOT()) })
      
      plotOutput(ns("PLOTi"), height = tailleGraph$height)#->PLOT
      
    })
  })
  
  output$DownGraphGlobal <- downloadHandler(
    filename =  function() {
      paste0(input$plottype, ".png")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      shiny::withProgress(
        message = "Veuillez patienter, le téléchargement de votre graphique est en cours",
        value = 0,
        {
          ggsave(file,
                 plot=THE_PLOT(),#,
                 #height =7.5*dim(ordre1())[1],
                 #width = widthSousSeq(),
                 device = png()) #méthode pour télécharger les graphiques ggplot
          
          dev.off()  # turn the device off
          shiny::incProgress(1)
        })
    } 
  )
  
  THE_DATA_base1<-eventReactive(input$UPDATE_PLOT, {
    
    req(the.grups())
    if(length(unique(isolate(the.grups()) ))<20){
      
      lapply(unique(isolate(the.grups() )),FUN = function(levi){
        
        data$SEQ_OBJ()[isolate(the.grups())==levi , ]->seqi
        seqi[!is.na(seqi[ , 1]) , ]->seqi
        DONNEES_POUR_PLOT(TYPE = isolate(input$plottype), objseq = seqi,
                          pmin.sup=isolate(input$pminsup), STR.SUBS=isolate(input$select_event),isolate(input$select_col_time_flux),
                          PAS.temps = isolate(input$PAStrate), TIME.varying =isolate(input$TYPEtrate) , 
                          Pourc.eff = isolate(input$TypeValeur), Sens = isolate(input$DebArr))
        
      })->listdat
      #
      names(listdat)<-unique(isolate(the.grups()))
      return(listdat)
      
    }
    
  })
  
  THE_DATA_DToutput<-reactive({
  if(input$plottype!="txtr"){
      ROWFT<-TRUE
      } else {
        ROWFT<-FALSE
      }
    req(the.grups())
    req(THE_DATA_base1())
    THE_DATA_base1()->listdat

      lapply(1:length(listdat), function(li){
        message("coucou 412")
        print(listdat[[li]])
        if(class(listdat[[li]])=="list"){
          message("coucou 409")
          lapply(1:length(listdat[[li]]), function(li2){ 
            output[[ paste("l", li, li2, sep="")]]<-DT::renderDataTable(datatable(listdat[[li]][[li2]], filter="top", rownames = ROWFT))

              list(
            h3( names(listdat[[li]])[li2]),
              DT::dataTableOutput(ns(paste("l2", li, li2, sep="")))
            )->res
            
              return(res)
            
          })
        } else {
          message("coucou 416")
          message(class(listdat[[li]]))
          #print(head(listdat[[li]]))
          message("coucou 424")
          
          output[[ paste("l1", li, sep="") ]]<-DT::renderDataTable(datatable(data.frame(listdat[[li]]), filter="top", rownames = ROWFT))
          
          list(
            h3( names(listdat)[li]),
            DT::dataTableOutput(ns(paste("l1", li, sep="")))
          )->res
          
return(res)
        }
      })->liou
      
      print(liou)
      
      return(liou)
  })
    
  
  
  output$DATA_r<-renderUI({
    req(THE_DATA_DToutput())
    THE_DATA_DToutput()
  })
  
  THE_DATA_forDownload<-reactive({
    req(the.grups())
    req(THE_DATA_base1())
    THE_DATA_base1()->listdat
    
    
    lapply(1:length(listdat), function(li){
      if(class(listdat[[li]])=="list"){
        lapply(1:length(listdat[[li]]), function(li2){ 
          
          if(isolate(input$plottype)!="txtr"){
          
          data.frame(listdat[[li]][[li2]])->tempidf2
          print(tempidf2)
          tempidf2$SELECTION<-names(listdat[[li]])[li2]
          print(tempidf2)
          tempidf2<-reshape(direction = "long", data = tempidf2, 
                            varying = list(names(tempidf2)[names(tempidf2)!="SELECTION"]), 
                            times = names(tempidf2)[names(tempidf2)!="SELECTION"], ids = row.names(tempidf2))
          names(tempidf2)[!names(tempidf2)%in%c("SELECTION", "time", "id")]<-"value"
           print(tempidf2)
          } else {
            listdat[[li]][[li2]]->tempidf2
            tempidf2$`Pas de temps`<-isolate(input$PAStrate)
          }
          return(tempidf2)

        })->souslist
        do.call("rbind", souslist)->df
     } else {
        if(isolate(input$plottype)!="txtr"){
      data.frame(listdat[[li]])->df
        print(df)
        df$SELECTION<-names(listdat)[li]
        print(df)
        df<-reshape(direction = "long", data = df, varying = list(names(df)[names(df)!="SELECTION"]), 
                          times = names(df)[names(df)!="SELECTION"], ids = row.names(df))
        names(df)[!names(df)%in%c("SELECTION", "time", "id")]<-"value"
        
      print(df)
        } else {
          df<-listdat[[li]]
          df$`Pas de temps`<-isolate(input$PAStrate)
          df$SELECTION<-names(listdat)[li]
        }
      }
        return(df)
    })->datadf
    do.call("rbind", datadf)->df
    df
  })
    
    
  output$ButtondownloadData <- downloadHandler(
    filename = function() {
      paste("DATA",gsub(pattern = "2", replacement = "", x = input$TypeFichierDownload, fixed = TRUE),sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      
      shiny::withProgress(
        message = "Veuillez patienter, le téléchargement de votre fichier est en cours",
        value = 0,
        {
          if(input$TypeFichierDownload%in%c("csv", "txt")){
            write.csv(x = THE_DATA_forDownload(), file, row.names = FALSE)
          } else {
            
            if(input$TypeFichierDownload=="csv2"){
              write.csv2(x = THE_DATA_forDownload(), file, row.names = FALSE)
            }
          }
          shiny::incProgress(1)
        })
    }
  )
  
#### CLOTURE :  
}
  
#     if(length(unique(the.grups()))<=1){
#       seqggplot(TYPE = input$plottype, objseq = data$SEQ_OBJ, groupes = NULL) #seqplot(seqdata = seq.select1(), type = input$plottype), 
#       #width = large(),height = haut1()
#      } else {
#        lapply(X = unique(the.grups()), FUN = function(grup.i){
#          data$SEQ_OBJ[the.grups()==grup.i , ]->dat.seq.i
#        })->seqli
#        names(seqli)<-unique(the.grups())
#        seqggplot(TYPE = input$plottype, objseq = seqli)
#        
#      }
#     
#     #if(input$plottype %in% c("d", "f", "I", "ms", "mt", "r","Ht","pc", "r")){
#     #  seqplot(seqdata = data$SEQ_OBJ, type = input$plottype, group=the.grups())
#     #} else {
#     #  NULL
#     #}
#     #return(res)
#   })->res
#     return(res)
#    # })->d1
#   #return(reactive(d1))
#   #### RETURN ####
#   #return(reactive({
#   #  list("SEQ_OBJ"=data.seq(), 
#   #       "DATA_COMP"=data2(), 
#   #       "TYPE_SOURCE"=input$DataType, 
#   #       "CODAGE_MANQUANT"=list("GAP"=input$TEXT_GAP, "RIGHT"=input$TEXT_RIGHT, "LEFT"=input$TEXT_LEFT) )
#   #}))
# }