#' @title   tabdes
#' @description  A shiny Module qui permet de tabdes
#'
#' @param id shiny id
#' @param label fileInput label
#' @importFrom stringr str_detect
#' @export
module_tabdes_UI <- function(id){#label = "CSV file") {
  ns <- NS(id)
  
  tabsetPanel(type = "pills",
              tabPanel("Tri à plat : ",
                       uiOutput(ns("SELECTVAR_OUPTUT")),
                       #textOutput(ns("LISTOFTABLES")),
                       uiOutput(ns("LISTOF_DT")) %>% withSpinner(color="#0dc5c1")
              ),
              tabPanel("Tables de contingences",
                       fluidRow(
                         shiny::column(
                           width = 4,
                           uiOutput(ns("SELECTVAR_CON1"))  %>% withSpinner(color="#0dc5c1")),
                         shiny::column(
                           width = 4,
                           uiOutput(ns("SELECTVAR_CON2")) %>% withSpinner(color="#0dc5c1")),
                         shiny::column(
                           width = 4,
                           uiOutput(ns("SELECTVAR_CON3"))  %>% withSpinner(color="#0dc5c1")),
                       #conditionalPanel(condition = "input.mask==1")
                       
                       shiny::selectInput(inputId = ns("PROP_INPUT"), label = "Afficher des pourcentages?", multiple = FALSE, selected = "Pas de pourcentages",
                                          choices = c("Pas de pourcentages", "Pourcentages en ligne", "Pourcentages en colonne")),
                       uiOutput(ns("TABCONOUT"))  %>% withSpinner(color="#0dc5c1")#,
                       # CONTROL textOutput("a1")#,
                       #uiOutput("LISTTABLESCONTINGENCE")
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
#' @importFrom utils read.csv
#' @importFrom glue glue
#' @export
module_tabdes <- function(input, output, session, data) {
  #### PARAMETERS ####
  library(dplyr)
  library(TraMineR)
  library(RColorBrewer)
  library(ggplot2)
  library(ggthemes)
  ns <- session$ns
  
  options(DT.options = list(pageLength = 25,
                            dom = 'tp',
                            #language = list(search = 'Filter:'),
                            initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header()).css({'background-color':'#fff' , 'color': '#cc0000'  });",
                              "}")
  )
  )
  
  listovars<-reactive({unique(unlist(lapply(data$DATA_COMP, function(dfx){
    lapply(1:ncol(dfx), function(j){
      if(class(dfx[ , j])%in%c("integer", "numeric")){
        names(dfx)[j]
      } else {
        if(class(dfx[ , j])%in%c("factor", "character")){
          if(length(unique(dfx[ , j]))<=20){
            names(dfx)[j]
          }
        }
      }
    })
  })
  ))
  })
                                            
  output$SELECTVAR_OUPTUT<-renderUI({
    shiny::selectInput(inputId = ns("VARSSELECT"),choices = listovars(),selected = NULL, multiple = TRUE, label="Sélection des variables  : ")
  })
  
  listoftabs<-reactive({
    req(input$VARSSELECT)
    lapply(input$VARSSELECT, function(ni){
      
      unilevel.ni<-unique(unlist(lapply(data$DATA_COMP, function(dfj){
        if(ni %in% names(dfj)){
        unique(dfj[ ,  ni])
        }
      })))
      
      listtable.ni<-lapply(1:length(data$DATA_COMP), function(j){
        if(ni %in% names(data$DATA_COMP[[j]])){
          data$DATA_COMP[[j]]->dfj
          dfj[ , ni]<-factor(dfj[ , ni], levels = unilevel.ni)
          table(dfj[ , ni], exclude = NULL)->tab
          data.frame(tab)->dftab
          names(dftab)<-c("États", "Effectifs")
          dftab$Répartition<-round(dftab$Effectif/sum(dftab$Effectif)*100, 2)
          dftab$DATE<-if(is.null(names(data$DATA_COMP))){j} else {names(data$DATA_COMP)[j]}
          dftab[order(dftab$Effectifs, decreasing = TRUE) , ]->dftab
          dftab$États<-as.character(dftab$États)
          dftab[(nrow(dftab)+1) , ]<-c("TOTAL", sum(dftab$Effectifs), sum(dftab$Répartition), unique(dftab$DATE))
          
          return(dftab)
        }
      })
      
      do.call("rbind", listtable.ni)->glodf.ni
      print(glodf.ni)
      return(glodf.ni)
    })->list.of.tables
    #print(list.of.tables)
    return(list.of.tables)
  })
  output$LISTOFTABLES<-renderPrint({print( listoftabs() )})
  
  ####
  output$LISTOF_DT<-renderUI({
    
    if(length(input$VARSSELECT)<1){
      tags$code("Choisissez au moins une variable")
    } else {
        lapply(1:length(listoftabs()), FUN = function(ki){
          df.ki<-listoftabs()[[ki]]
          #levels(df.ki$Modalités)<-c(levels(df.ki$Effectifs), "TOTAL")
          df.ki$États<-as.factor(df.ki$États)
          df.ki$DATE<-as.factor(df.ki$DATE)
          df.ki$Effectifs<-as.numeric(as.character(df.ki$Effectifs))
          df.ki$Répartition<-as.numeric(as.character(df.ki$Répartition))
          
          print(df.ki)
          h3(names(listoftabs())[ki])->DISPO

          list(h3(DISPO),
               renderDT(datatable(df.ki,rownames = FALSE, filter = "top"), #caption=unique(SELECTED_GLOVAR()),
                        options=list(2, "desc"))
          )
          #list(DISPO,
          #     renderDT(df.ki, caption=SELECTED_GLOVAR()[[ki]])
          #)
        })->list.of.dt1
      
        if(!length(list.of.dt1)>=2){
          list.of.dt1
        } else {
          bA1<-1
          bA2<-if(length(list.of.dt1) %% 2 ==0){
            length(list.of.dt1)/2
          } else {floor(length(list.of.dt1)/2)}
          bB1<-bA2+1
          bB2<-length(list.of.dt1)
          fluidRow(column(6, list.of.dt1[seq(from = bA1, to = bA2, by = 1)],
                          style='border-right: 1px solid red'),
                   column(6, list.of.dt1[seq(from = bB1,
                                             to=bB2, by=1)])
          )
        }
        
      }
    })
  ####
  
  #### TABLE DE CONTINENCE ####
  ###### VAR1 ######
  output$SELECTVAR_CON1<-renderUI({
    list(
    shiny::selectInput(inputId = ns("SELECTDATE1"), label = "Date (1) : ",
                       choices = c("Pas de sélection",as.character( names(data$DATA_COMP) )), multiple = FALSE, width = "150%",
                       selected=NULL),
    shiny::selectInput(inputId = ns("VARDATE1"), label = "Variable (1) : ",
                       choices = NULL, multiple = FALSE, width = "150%",
                       selected=NULL)
    )
  })
  observe({
    req(input$SELECTDATE1)
    updateSelectInput(session = session, inputId = "VARDATE1", choices = names(data$DATA_COMP[[input$SELECTDATE1]]))
  })
  ###### VAR2 ######
  output$SELECTVAR_CON2<-renderUI({
    list(
      shiny::selectInput(inputId = ns("SELECTDATE2"), label = "Date (2) : ",
                         choices = c("Pas de sélection",as.character( names(data$DATA_COMP) )), multiple = FALSE, width = "150%",
                         selected=NULL),
      shiny::selectInput(inputId = ns("VARDATE2"), label = "Variable (2) : ",
                         choices = NULL, multiple = FALSE, width = "150%",
                         selected=NULL)
    )
  })
  observe({
    req(input$SELECTDATE2)
    updateSelectInput(session = session, inputId = "VARDATE2", choices = names(data$DATA_COMP[[input$SELECTDATE2]]))
  })
  ###### VAR3 ######
  output$SELECTVAR_CON3<-renderUI({
    list(
      shiny::selectInput(inputId = ns("SELECTDATE3"), label = "Date (3) : ",
                         choices = c("Pas de sélection",as.character( names(data$DATA_COMP) )), multiple = FALSE, width = "150%",
                         selected="Pas de sélection"),
      shiny::selectInput(inputId = ns("VARDATE3"), label = "Variable (3) : ",
                         choices = NULL, multiple = FALSE, width = "150%",
                         selected=NULL)
    )
  })
  observe({
    req(input$SELECTDATE3)
    updateSelectInput(session = session, inputId = "VARDATE3", choices = names(data$DATA_COMP[[input$SELECTDATE3]]))
  })
  
  #### TABLE JOINT ####
  perclog<-reactive({input$PROP_INPUT!="Pas de pourcentages"})
  perctype<-reactive({if(input$PROP_INPUT=="Pourcentages en ligne"){1} else {if(input$PROP_INPUT=="Pourcentages en colonne"){2}} })
  
  reactive({
    if(input$SELECTDATE1!="Pas de sélection"&input$SELECTDATE2!="Pas de sélection"){
      if(input$SELECTDATE3=="Pas de sélection"){
        print("coucou215")
        print(as.character(data$ID_VAR))
        print("control.x")
        print(as.character(data$ID_VAR)%in%names(data$DATA_COMP[[input$SELECTDATE1]]))
        print("control.y")
        print(as.character(data$ID_VAR)%in%names(data$DATA_COMP[[input$SELECTDATE2]]))
        
    table.joint(data1=data$DATA_COMP[[input$SELECTDATE1]], var1=input$VARDATE1, 
                data2=data$DATA_COMP[[input$SELECTDATE2]], var2=input$VARDATE2, BY=as.character(data$ID_VAR), data3 = NULL, var3 = NULL,
                prop=perclog(), prop.margin=perctype())->tabj
      } else {
        table.joint(data1=data$DATA_COMP[[input$SELECTDATE1]], var1=input$VARDATE1, 
                    data2=data$DATA_COMP[[input$SELECTDATE2]], var2=input$VARDATE2, BY=as.character(data$ID_VAR), 
                    data3 = data$DATA_COMP[[input$SELECTDATE3]], var3 = input$VARDATE3, prop=perclog(), prop.margin=perctype())->tabj
      }
    } else {tabj<-NULL}
return(tabj)    
  })->tabs
  
  output$TABCONOUT<-renderUI({
    req(tabs())
    if(class(tabs())=="list"){
      print("length(tabs())")
      print(length(tabs()))
      if(length(tabs())<20){
      lapply(1:length(tabs()), function(ti){
        output[[paste("aa",ti, sep="")]]<-DT::renderDataTable(DT::datatable(data = as.data.frame.array(tabs()[[ti]]), 
                                                                            rownames = TRUE))
       res0<-list(
          h4(names(tabs())[[ti]]),
        DTOutput(ns(paste("aa",ti, sep="")))
        )
       return(res0)
      })
      } else {h3("Erreur : les valeurs uniques de la variable 3 sont trop nombreuses")}

    } else {
      
      output$aa<-DT::renderDataTable(DT::datatable(data = as.data.frame.array(tabs()), 
                                                   rownames = TRUE))
      DTOutput(ns("aa"))
    }

  })
  
}