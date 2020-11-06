#' @title   type de file, select of ind and data
#' @description  A shiny Module that imports data, select them and export as output: data.for.seq, data.comp
#'
#' @param id shiny id
#' @param label fileInput label
#' @importFrom stringr str_detect
#' @export
module_data_launcher_UI <- function(id){#label = "CSV file") {
  ns <- NS(id)

  
  tabPanel("Les données",
           shiny::actionButton(inputId = ns("ValidParametres"), label = "Je valide ces trajectoires"),
           shiny::downloadButton(outputId = ns("DOWNSEQ"), 
                                 label = "Enregistrer les trajectoires et leurs données complémentaires sur le disque : " ),
           uiOutput(ns("DES_TRAJ_OBJ")),
           hr(),
           #### SUB-PANEL: PARAMETRAGE ####
           tabsetPanel(id = "tabpan",
                       #tabPanel("TEST",
                      #          numericInput(inputId = ns("numinput"), label = "numeric input test", value = 5, min = 1 ,max = 10),
                      #          uiOutput(ns("ui_server_created")),
                      #          uiOutput(ns("ui_sever_created2"))
                      #                   
                      # ),
                       
                       tabPanel(title = "Import et formattage des données : ",
                                verticalLayout(fluid=TRUE, 
                                               
                                #### CHARGEMENT FICHIER ####
                                wellPanel(style = "background: #DCDCDC",
                                          fluidRow(h3("Chargement du fichier"), 
                                  width = 12,
                                  shiny::column(width = 6,
                                                shiny::selectInput(inputId = ns("DataType"), label = "Choix du type de données", 
                                                                   choices = c("Un objet RData contenant de multiples data.frame"="objet", 
                                                                               "Un objet SQL contenant de multiples data.frame"="SQL",
                                                                               "Un objet RData contenant un objet seqdata"="objseq",
                                                                               "Un fichier .csv unique contenant les données"="fichier" 
                                                                   ), 
                                                                   multiple = FALSE, selected = "fichier"),
                                                helpText("Voir les trois fichiers d'example qui illustrent les différents types de données : data(package = 'ViCaTraj') "),
                                                helpText("ATTENTION : vous pouvez charger un objet .RData contenant déjà des trajectoires tout en choisissant l'option 'un objet .RData contenant de multiples data.frame' : dans ce cas, les trajectoires contenues dans l'objet sont supprimées, et les données complémentaires sont utilisées pour sélectionner des individus et reconstruire des trajectoires. ")
                                                
                                  ),
                                  shiny::column(width = 6,
                                                
                                                conditionalPanel(
                                                  condition = paste0("input['", ns("DataType"), "'] == 'fichier'"),
                                                  #condition = "input.DataType == 'fichier'"
                                                  shiny::selectInput(inputId=ns("sepcol"), label= "Separateur de colonnes", 
                                                                     choices=c("Virgule" = ",","Point-Virgule" = ";","Tabulation" = "\t"), selected=","),
                                                  shiny::selectInput(inputId=ns("dec"), label= "Séparateur décimal", 
                                                                     choices=c("Virgule" = ",","Point" = "."), selected="."),
                                                  shiny::selectInput(inputId=ns("endoding"), label= "Comment est codé le fichier ?", 
                                                                     choices=c(UTF8 = "UTF-8", Latin1 = "latin1"), selected = "UTF-8", multiple = FALSE, width = "50%"),
                                                  shiny::checkboxInput(inputId = ns("header"), label="La première ligne correspond-elle aux noms des variables ?",
                                                                       value=TRUE),  
                                                  shiny::selectInput(inputId = ns("na"), label = "Codage des valeurs manquantes", 
                                                                     choices = c("Vide" , "Espace" = " ", "NA" = "NA"), 
                                                                     selected = "NA", multiple = TRUE, selectize = TRUE),
                                                  
                                                  fileInput(inputId=ns("file1"), label="Sélectionnez votre fichier source:", 
                                                            multiple = FALSE, accept = c("text/csv",
                                                                                         "text/comma-separated-values,text/plain",
                                                                                         ".csv"), width = NULL)
                                                ), 
                                                conditionalPanel(
                                                  #condition = "input.DataType == 'objet'",
                                                  condition = paste0("input['", ns("DataType"), "'] == 'objet'"),
                                                  helpText("INFO: pour des raisons de sécurité il n'est pas possible de charger directement un dossier dans un navigateur web. Vous pouvez utiliser la fonction LIST_MULTIPLE_CSV du package ViCaTraj pour créer l'objet RData à partir de mulitples fichiers .csv"),
                                                  fileInput(inputId=ns("LIST_SOURCE_BIG_DF"), 
                                                            label="Sélectionner l'objet .RData contenant les multiples data.frame", 
                                                            multiple = FALSE, accept = NULL, width = NULL)
                                                  
                                                  #hr()
                                                  #shiny::textOutput("CONTROLDATA"))
                                                ),
                                                conditionalPanel(
                                                  #condition = "input.DataType == 'objet'",
                                                  condition = paste0("input['", ns("DataType"), "'] == 'SQL'"),
                                                  helpText("INFO: pour des raisons de sécurité il n'est pas possible de charger directement un dossier dans un navigateur web. Vous pouvez utiliser la fonction 'A VENIR' du package ViCaTraj pour créer l'objet .sqlite à partir de mulitples fichiers .csv"),
                                                  fileInput(inputId=ns("SOURCE_SQLITE"), 
                                                            label="Sélectionner l'objet .sqlite contenant les multiples data.frame", 
                                                            multiple = FALSE, accept = NULL, width = NULL)
                                                  
                                                  #hr()
                                                  #shiny::textOutput("CONTROLDATA"))
                                                ),
                                                conditionalPanel(
                                                  #condition = "input.DataType == 'objseq'",
                                                  condition = paste0("input['", ns("DataType"), "'] == 'objseq'"),
                                                  
                                                  helpText("INFO: vous pouvez charger un objet .RData contenant une liste d'objet, dont au moins un objet de type seqdata, et éventuellement un ou des data.frames complémentaires (si plusieurs : un par date)"),
                                                  fileInput(inputId=ns("LIST_SEQ"), 
                                                            label="Sélectionner l'objet .RData contenant l'objet seqdata", 
                                                            multiple = FALSE, accept = NULL, width = NULL)
                                                  
                                                  #hr()
                                                  #shiny::textOutput("CONTROLDATA"))
                                                )
                                  ))),
                                hr(),
                                #conditionalPanel(condition = paste0("input['", ns("DataType"), "'] == 'SQL'"), 
                                                 module_data_internal_SQL_UI(id="SQL1")#,
                                #                 h2("TESTSQL")),
                                #conditionalPanel(condition = paste0("input['", ns("DataType"), "'] !== 'SQL'"), 
                                  #               module_data_internal_Rbase_UI(id="Rbase1")#,
                                 #                h2("TESTbase"))

                                ))))}
                                

#' server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @importFrom utils read.csv
#' @importFrom glue glue
#' @export
#' @rdname mod_csv_fileInput
module_data_launcher <- function(input, output, session) {
  library(TraMineR)
  library(RColorBrewer)
  ns <- session$ns
  r<-reactiveValues()

  reactive({
    c(input$file1, input$LIST_SOURCE_BIG_DF, input$LIST_SEQ)
  })->react.choice
  
  #res<-eventReactive(input$DataType, {
    observeEvent({
    if(input$DataType=="SQL"){
      (module = module_data_internal_SQL, id = sub("-", "", session$ns("")))->DATA.res
    } else {
      callModule(module = module_data_internal_Rbase, id = sub("-", "", session$ns("")))->DATAs.res
    }
    })
    #return(DATAs.res)
  #})
  return(DATA.res)
}