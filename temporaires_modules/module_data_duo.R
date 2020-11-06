#' @title   type de file, select of ind and data
#' @description  A shiny Module that imports data, select them and export as output: data.for.seq, data.comp
#'
#' @param id shiny id
#' @param label fileInput label
#' @importFrom stringr str_detect
#' @export
module_data_duo_UI <- function(id){#label = "CSV file") {
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
                                                                                            multiple = FALSE, accept = NULL, width = NULL),
                                                                                  textOutput(ns("CONTROLNAMES.SQL"))
                                                                                  
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
                                               wellPanel(style = "background: #DCDCDC",
                                                         fluidRow(h3("Format des données"), 
                                                                  #sidebarPanel( h3("Format des données"), 
                                                                  width = 12,
                                                                  #### condition = "input.DataType == 'fichier'"  ####
                                                                  
                                                                  #### condition = "input.DataType == 'objet'"  ####
                                                                  
                                                                  conditionalPanel(
                                                                    #condition = "input.DataType == 'objet'",
                                                                    condition = paste0("input['", ns("DataType"), "'] == 'objet'"),
                                                                    
                                                                    shiny::column(width = 4,
                                                                                  shiny::selectInput( inputId = ns("MINTIMEBIG"), label = "Borne temporelle inférieure:", 
                                                                                                      multiple = FALSE, choices = "" , width = '100%')),
                                                                    shiny::column(width = 4,
                                                                                  shiny::selectInput( inputId = ns("MAXTIMEBIG"), label = "Borne temporelle supérieure:", 
                                                                                                      multiple = FALSE , choices = "", width = '100%')),
                                                                    shiny::column(width = 4,
                                                                                  shiny::numericInput(inputId = ns("PAS_TEMPS_BIGDATA"), label = "Pas de temps pour les données:", 
                                                                                                      value = 1, min = 1, step = 1, width = '100%')),
                                                                    shiny::column(width = 6, "Noms des data.frame sélectionnés : "),  
                                                                    shiny::column(width = 6, textOutput(ns("CONTROLNAMES"))),
                                                                    #shiny::column(width = 6, "Noms des data.frame sélectionnés : "),  
                                                                    #shiny::column(width = 6, textOutput(ns("SLIDERTEXT")))
                                                                  )
                                                         )),
                                               wellPanel(style = "background: #DCDCDC",
                                                         fluidRow(h3("Identifiant des individus"), 
                                                                  column(width = 12, 
                                                                         conditionalPanel(
                                                                           #condition = "input.DataType == 'objet'||input.DataType == 'objseq'",
                                                                           condition = paste0(
                                                                             "input['", ns("DataType"), "'] == 'objet'||input['", ns("DataType"), "'] == 'objseq'||input['", ns("DataType"), "'] == 'fichier'||input['", ns("DataType"), "'] == 'SQL'"),
                                                                           #column(12, 
                                                                           #sidebarPanel( h3("Identifiant des individus"), width = 12,
                                                                           
                                                                           
                                                                           uiOutput(ns("UI_INDVAR_CHOOSE")),
                                                                           uiOutput(ns("MSSG_DUPLI")),
                                                                           #DT::dataTableOutput(ns("DATA_DUPLI")),
                                                                           #uiOutput(ns("TEXTE_NROW_BIGLIST_AVANT_APRES")),
                                                                           uiOutput(ns("NROW_BIGLIST_AVANT_APRES")) %>% withSpinner(color="#0dc5c1"),
                                                                           uiOutput(ns("DELETE_DUPLI_NA"))
                                                                         )
                                                                         # )
                                                                  )
                                                         ),
                                                         
                                                         #### AJOUT TIERS ####
                                                         wellPanel(style = "background: #DCDCDC",
                                                                   fluidRow(h3("Ajout de variables issues d'un jeu de données tiers :"), 
                                                                            width=12,
                                                                            shiny::checkboxInput(inputId = ns("AJOUT_TIERS_LOG"), 
                                                                                                 label = "Ajout de variables issues d'un jeu de données tiers ?", 
                                                                                                 value = FALSE),
                                                                            conditionalPanel(condition = "input.AJOUT_TIERS_LOG == 1", ns = ns, 
                                                                                             
                                                                                             shiny::selectInput(inputId=ns("sepcolCOMP"), label= "Separateur de colonnes", 
                                                                                                                choices=c("Virgule" = ",","Point-Virgule" = ";","Tabulation" = "\t"), selected=","),
                                                                                             shiny::selectInput(inputId=ns("decCOMP"), label= "Séparateur décimal", 
                                                                                                                choices=c("Virgule" = ",","Point" = "."), selected="."),
                                                                                             shiny::selectInput(inputId=ns("endodingCOMP"), label= "Comment est codé le fichier ?", 
                                                                                                                choices=c(UTF8 = "UTF-8", Latin1 = "latin1"), selected = "UTF-8", multiple = FALSE, width = "50%"),
                                                                                             shiny::checkboxInput(inputId = ns("headerCOMP"), label="La première ligne correspond-elle aux noms des variables ?",
                                                                                                                  value=TRUE),
                                                                                             shiny::fileInput(label = "Fichier complémentaire", 
                                                                                                              inputId = ns("FichComp"), multiple = FALSE, 
                                                                                                              accept = c("text/csv",
                                                                                                                         "text/comma-separated-values,text/plain",
                                                                                                                         ".csv")),
                                                                                             shiny::selectInput(label = "Variable servant de clé dans les données complémentaires", 
                                                                                                                inputId = ns("CleComp"), choices = NULL),
                                                                                             shiny::selectInput(label = "Variable servant de clé dans les données de base", 
                                                                                                                inputId = ns("CleBase"), choices = NULL)
                                                                            )
                                                                            
                                                                   )),
                                                         
                                                         
                                                         #hr(),
                                                         #### FORMAT DONNNEES ####
                                                         
                                                         hr(),
                                                         
                                                         #### condition = "input.DataType == 'objet'"  ####
                                                         wellPanel(style = "background: #DCDCDC",
                                                                   fluidRow(h3("Sélection des individus :"), 
                                                                            #wellPanel(style = "background: blue"
                                                                            width = 12,
                                                                            
                                                                            shiny::checkboxInput(inputId=ns("addCONDS"), label = "Ajouter des conditions ? ", value = FALSE),
                                                                            
                                                                            conditionalPanel(
                                                                              #condition = "input.DataType == 'objet' && input.addCONDS == 1",
                                                                              condition = paste0(
                                                                                #"input['", ns("DataType"), "'] == 'objet' && 
                                                                                "input['", ns("addCONDS"), "']==1"),
                                                                              
                                                                              sidebarPanel( #h3("Sélection des individus:"), 
                                                                                width = 12,
                                                                                hr(),
                                                                                #shiny::column(width=6, 
                                                                                uiOutput(ns("UI_PAQUET_SELECT")),
                                                                                helpText("INFO: "),
                                                                                helpText("toutes les conditions ajoutées dans le même paquet ne sont pas additives (utilisation de l'opérateur logique 'ou' ('|')."),
                                                                                helpText("toutes les conditions ajoutées dans des paquets différents sont additives (utilisation de l'opérateur logique 'et' entre les paquets de conditions ('&'))."),
                                                                                
                                                                                shiny::uiOutput(ns("UI_DATE_SELECT")),
                                                                                helpText("ATTENTION : si vous sélectionnez plusieurs dates simultanément, ne seront retenus que les individus qui remplissent la condition pour TOUTES les dates sélectionnées."),
                                                                                
                                                                                #shiny::checkboxInput(inputId = ns("addvar"), label = "Ajouter une variable d'un jeu de donnée tiers?", value = FALSE),
                                                                                
                                                                                shiny::uiOutput(ns("ADDDATA_CSV")),
                                                                                
                                                                                uiOutput(ns("UI_VAR_SELECT")),#), 
                                                                                #shiny::column(width=12, 
                                                                                uiOutput(ns("UI_CLASS_SELECT")),#),
                                                                                uiOutput(ns("UI_MOD_SELECT")),
                                                                                hr(),
                                                                                
                                                                                actionButton(inputId=ns("addROW"), label = "Ajouter la condition"),
                                                                                #),
                                                                                actionButton(inputId = ns('delROW'), label = "Supprimer les conditions sélectionnées"),
                                                                                DT::DTOutput(ns("TABLE_POUR_SELECTION"))
                                                                              )
                                                                            )#,
                                                                            #### CONTROLS A AJOUTER ####
                                                                            #textOutput(ns("LENGTH_IND_SUBS")),
                                                                            #textOutput(ns("LENGTH_SUBSETTED")),
                                                                            #textOutput(ns("LENGTH_BIGLIST")),
                                                                            #textOutput(ns("LENGTH_BIGLIST1"))#,
                                                                            #,#,
                                                                            #DT::dataTableOutput(ns("NROW_BIGLIST2"))#,
                                                                            
                                                                            #shiny::downloadButton(outputId = "downlist", label = "Enregistrer le jeu de données sur le disque (pour réutilisation ultérieure)")
                                                                            
                                                                            #)
                                                                   ))
                                                         
                                               ))),
                       #### Construction des trajectoires ####
                       tabPanel(title = " Paramétrage des trajectoires ",
                                #),
                                
                                
                                conditionalPanel(
                                  #condition = "input.DataType != 'objseq'", 
                                  condition = paste0("input['", ns("DataType"), "']!= 'objseq'"),
                                  
                                  
                                  sidebarPanel(
                                    
                                    h3("Paramétrage des trajectoires"),
                                    width = 12,
                                    shiny::selectInput(inputId = ns("timecol"), label = "Variables temporelles (mettre dans l'ordre chronologique)", 
                                                       choices = "", selected = NULL, multiple = TRUE, selectize = TRUE),
                                    shiny::uiOutput(ns("DATA_UI")),
                                    shiny::textInput(inputId = ns("TEXT_GAP"), label = "Label pour les 'gaps' : ", value = "GAP"),
                                    shiny::textInput(inputId = ns("TEXT_RIGHT"), label = "Label pour les censures à droite : ", value = "CENSURE"),
                                    shiny::textInput(inputId = ns("TEXT_LEFT"), label = "Label pour les départs tardifs : ", value = "LEFT"),
                                    shiny::selectInput(inputId = ns("MISSING_forseq"), 
                                                       label = "Etats présents dans les données et considérés comme des états manquants dans les trajectoires", 
                                                       choices = NULL, selected = NULL, multiple = TRUE),
                                    helpText("Les états considérés comme manquants seront supprimés des trajectoires et remplacés par le codage prévu pour les valeurs manquantes"),
                                    shiny::numericInput(inputId = ns("criterNb"), label = "Critère de sortie : nombre de mois consécutifs",value = 1, min = 1, max = 36, step = 1),
                                    uiOutput(ns("CONTROL_DUPLICATED_ID"))
                                  ))
                       ),
                       tabPanel(title="Résumé des trajectoires:",
                                
                                sidebarPanel(
                                  width = 12,
                                  uiOutput(ns("ATTR_TRAJ_OBJ"))
                                ),
                                mainPanel(
                                  shiny::dataTableOutput("contenu")
                                )
                       )
                       
                       #######
           )
           
  )
}
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
module_data_duo <- function(input, output, session) {
  library(TraMineR)
  library(RColorBrewer)
  ns <- session$ns
  r<-reactiveValues()
  #### CHARGEMENT DES DONNEES : liste de df ####
  reactive({
    c(input$file1, input$LIST_SOURCE_BIG_DF, input$LIST_SEQ, input$SOURCE_SQLITE)
  })->react.choice
  
  BIGLIST.FIRST<-reactive({
    if ( is.null(input$LIST_SOURCE_BIG_DF)) return(NULL)
    inFile <- input$LIST_SOURCE_BIG_DF
    file <- inFile$datapath
    # load the file into new environment and get it from there
    e = new.env()
    name <- load(file, envir = e)
    data <- e[[name]]
    
    sapply(data, function(x){inherits(x, "stslist", which = FALSE)})->cherche.seq
    if(TRUE%in%cherche.seq){
      data[cherche.seq==FALSE]->data
      data[[1]]->data
    }
    #
    temp.lenght<-length(data)
    # b1<-names(temp.lenght)[1]
    # b.max<-names(temp.lenght)[temp.lenght]
    #print(data)
    if(temp.lenght>1){
      names(data)<-sapply(names(data), function(ni){if(substr(ni, 1, 1)%in%c("0", as.character(seq(1, 9, 1)))  ) paste("D", ni, sep=".") else ni })
      names(data)<-gsub(pattern = "-", replacement = "_", fixed = TRUE, x=names(data))
    } else {
      if(temp.lenght==1){
        names(data)<-"Table.unique"
      }
    }
    return(data)
  })
  
  
  
  #### CHARGEMENT DES DONNEES : SQL####
  BIGLIST.CON<-reactive({
    if ( is.null(input$SOURCE_SQLITE)) {
      message("SQL NULL")
      return(NULL)
    } else {
    inFile <- input$SOURCE_SQLITE
    file <- inFile$datapath
    library(RSQLite)
    library(DBI)
    library(tidyverse)
    message("coucou SQL1")
    message("file")
    mydb <- dbConnect(RSQLite::SQLite(), file)
    message("coucou SQL1")
    dbListTables(mydb)->li.tab.names
    print(li.tab.names)
    message(paste(li.tab.names, collapse = "/"))
    temp.lenght<-length(li.tab.names)
    #
    return(mydb)
    }
  })
  
  output$CONTROLNAMES.SQL<-renderText({
    req(BIGLIST.CON())
    print("coucou SQL CONTROL NAMES")
    print(dbListTables(BIGLIST.CON()))
    return( print(dbListTables(BIGLIST.CON())))
  })
  
  #### CHARGEMENT DES DONNEES : list avec objet de class seqdata ####
  
  LIST_OBJSEQ<-reactive({
    if ( is.null(input$LIST_SEQ)) return(NULL)
    inFile <- input$LIST_SEQ
    file <- inFile$datapath
    # load the file into new environment and get it from there
    e = new.env()
    name <- load(file, envir = e)
    data <- e[[name]]
    #
    temp.lenght<-length(data)
    # b1<-names(temp.lenght)[1]
    # b.max<-names(temp.lenght)[temp.lenght]
    return(data)
  })
  
  CLASS_LIST_OBJSEQ<-reactive({
    req( LIST_OBJSEQ())
    lapply(LIST_OBJSEQ(), class)->class.listSEQ
    print(class.listSEQ)
    lapply(class.listSEQ, function(xi){sum(grepl(pattern = "stslist", x = xi))})->li2
    li2
  })
  
  OBJSEQ<-reactive({
    req( LIST_OBJSEQ())
    req(CLASS_LIST_OBJSEQ())
    LIST_OBJSEQ()[[which(CLASS_LIST_OBJSEQ()>0)]]
  })
  
  OBJSEQ_COMPLEMENT<-reactive({
    req( LIST_OBJSEQ())
    req(CLASS_LIST_OBJSEQ())
    LIST_OBJSEQ()[[which(CLASS_LIST_OBJSEQ()<1)]]->df.pour.seq
    if(class(df.pour.seq)=="data.frame"){
      message("pb seq 360")    
      list("Table.unique"=df.pour.seq)->res
    } else {
      message("pb seq 363")    
      res<-df.pour.seq
    }
    return(res)
  })
  
  #### CHARGEMENT DES DONNEES : 1 seul fichier ####
  
  trajs <- reactiveValues(df = NULL, dataSource = NULL)
  
  observe({
    req(input$file1)
    trajs$dataSource <- input$file1$datapath
    updateCheckboxInput(session=session,inputId = "rowname",value = FALSE )
    updateSelectInput(session = session, inputId = "rownames_par",choices = "")
  })
  
  observe({
    req(input$sepcol)
    updateCheckboxInput(session=session,inputId = "rowname",value = FALSE )
    updateSelectInput(session = session, inputId = "rownames_par",choices = "")
  })
  
  
  argna<-reactive({
    req(trajs$dataSource)
    if ("Vide" %in% input$na){
      c("",input$na)
      
    }else{
      input$na
      
    }
  })
  
  data<-reactive({
    message("coucou 343")
    if ( is.null(input$file1)) return(NULL)
    if(input$DataType=="fichier"){ #### Chargement d'un seul fichier CSV ####
      #if (input$rowname==TRUE && input$rownames_par!=""){
      message("coucou 347")
      userData <- read.csv(file = input$file1$datapath, 
                           sep = input$sepcol, 
                           encoding = input$endoding,
                           #row.names = input$rownames_par,
                           header=input$header,na.strings = argna(),
                           dec=input$dec)
      #row.names(userData)<-userData[ , input$rownames_par]
      #mycolumns<-c(colnames(userData))
      #updateSelectInput(session = session, inputId = "timecol", choices = mycolumns)
      # }
      
      #if(input$rowname==FALSE){
      # userData <- read.csv(file = input$file1$datapath, sep = input$sepcol, 
      #                    encoding = input$endoding,header=input$header,na.strings = argna(),dec=input$dec)
      #mycolumns <- c(colnames(userData))
      #updateSelectInput(session = session, inputId = "timecol", choices = mycolumns)
      # trajs$df<- userData}
      
      names(userData)<-sapply(names(userData), function(ni){if(substr(ni, 1, 1)%in%c("0", as.character(seq(1, 9, 1)))  ) paste("D", ni, sep=".") else ni })
      names(userData)<-gsub(pattern = "-", replacement = "_", fixed = TRUE, x=names(userData))
      
      
      list("Table.unique"=userData)->res
      return(res)
    } else {
      
    }
  })
  
  output$contenu<-shiny::renderDataTable({
    req(data())
    data()
  })
  
  #### CHARGEMENT DES DONNEES : CREATION D'UN OBJET UNIQUE : BIGLIST1() ####
  
  BIGLIST1<-reactive({
    req(input$DataType)
    if(input$DataType=="fichier"){
      req(data())
      print(head(data()[[1]]))
      return(data())
    } else {
      if(input$DataType=="objseq"){
        req(OBJSEQ_COMPLEMENT())
        return(OBJSEQ_COMPLEMENT())
      } else {
        req(BIGLIST.FIRST())
        if(input$DataType=="objet"){
          req(BIGLIST.FIRST())
          req(input$MINTIMEBIG)
          req(input$MAXTIMEBIG)
          req(input$PAS_TEMPS_BIGDATA)
          vecsel<-1:length(BIGLIST.FIRST())
          if(length(input$MAXTIMEBIG)>0&length(input$MINTIMEBIG)>0&length(input$PAS_TEMPS_BIGDATA)>0){
            #if(input$MAXTIMEBIG!=""&input$MINTIMEBIG!=""&input$PAS_TEMPS_BIGDATA!=""){
            req(input$PAS_TEMPS_BIGDATA)
            req(input$MINTIMEBIG)
            req(input$MAXTIMEBIG)
            
            if(input$MINTIMEBIG==""){FROM<-1} else {FROM<-which(names(BIGLIST.FIRST())==input$MINTIMEBIG)}
            if(!is.numeric(FROM)|FROM<=0){FROM<-1}
            
            if(input$MAXTIMEBIG==""){TO<-1} else {TO<-which(names(BIGLIST.FIRST())==input$MAXTIMEBIG)}
            if(!is.numeric(TO)|TO<=0){TO<-length(BIGLIST.FIRST())}
            
            if(length(input$PAS_TEMPS_BIGDATA)>0){
              if(input$PAS_TEMPS_BIGDATA==""){bypas<-1} else {bypas<-input$PAS_TEMPS_BIGDATA}
              if(!is.numeric(bypas)|bypas<=0){bypas<-1}
            } else {bypas<-1}
            
            vecsel<-seq(FROM, TO, bypas) 
          }
          #}
          BIGLIST.FIRST.SEL<-BIGLIST.FIRST()[vecsel]
          return(BIGLIST.FIRST.SEL)
        } else {
          if(input$DataType=="SQL"){
            req(BIGLIST.CON())
            # req(input$MINTIMEBIG)
            # req(input$MAXTIMEBIG)
            # req(input$PAS_TEMPS_BIGDATA)
            # #### SQL FIRST SEL
            # DBI::dbListTables(BIGLIST.CON())->lista
            # vecsel<-1:length(lista)
            # if(length(input$MAXTIMEBIG)>0&length(input$MINTIMEBIG)>0&length(input$PAS_TEMPS_BIGDATA)>0){
            #   #if(input$MAXTIMEBIG!=""&input$MINTIMEBIG!=""&input$PAS_TEMPS_BIGDATA!=""){
            #   req(input$PAS_TEMPS_BIGDATA)
            #   req(input$MINTIMEBIG)
            #   req(input$MAXTIMEBIG)
            #   
            #   if(input$MINTIMEBIG==""){FROM<-1} else {FROM<-which(lista==input$MINTIMEBIG)}
            #   if(!is.numeric(FROM)|FROM<=0){FROM<-1}
            #   
            #   if(input$MAXTIMEBIG==""){TO<-1} else {TO<-which(lista==input$MAXTIMEBIG)}
            #   if(!is.numeric(TO)|TO<=0){TO<-length(lista)}
            #   
            #   if(length(input$PAS_TEMPS_BIGDATA)>0){
            #     if(input$PAS_TEMPS_BIGDATA==""){bypas<-1} else {bypas<-input$PAS_TEMPS_BIGDATA}
            #     if(!is.numeric(bypas)|bypas<=0){bypas<-1}
            #   } else {bypas<-1}
            #   
            #   vecsel.sel<-seq(FROM, TO, bypas) 
            # }
            # vecsel.no<-vecsel[!vecsel%in%vecsel.sel]
            # exclude.names<-lista[vecsel.no]
            # for(ni in exclude.names){
            #   dbRemoveTable(conn = BIGLIST.CON(), ni)
            # }
            # BIGLIST.FIRST.SEL<-BIGLIST.CON()
            return(BIGLIST.CON())
          } else {NULL}
      }
    }
  }
    })
  ##### MINTIMEBIG / MAXTIMEBIB #####
  observeEvent(react.choice() , {
    #req(input$DataType)
    message("CONTROL.DATA.TYPE")
    message(input$DataType)
    
    if(input$DataType=="objet"){
      req(BIGLIST.FIRST())
      r$origin.df<-BIGLIST.FIRST()
      namod<-names(isolate(BIGLIST.FIRST()))
      message("namod")
      print(namod)
      updateSelectInput(
        session = session, 
        inputId = "MINTIMEBIG", 
        choices = namod,
        selected= namod[1]
      )
      updateSelectInput(
        session = session, 
        inputId = "MAXTIMEBIG", 
        choices = namod,
        selected= namod[length(namod)])
      updateNumericInput(session = session, inputId = "PAS_TEMPS_BIGDATA", value=1)
    } else {
      #if(input$DataType=="SQL"){
      #  req(BIGLIST.CON())
      #  r$origin.df<-BIGLIST.CON()
      #  dbListTables(BIGLIST.CON())->namod
      #  message("control.namod")
      #  print(namod)

      #} else {
      NULL}
  })
  
  output$CONTROLNAMES<-renderText({
    print("coucou 315")
    print(names(BIGLIST1() ))
    return(print(names(BIGLIST1() )))
  })
  
  output$SLIDERTEXT<-renderText({
    req( BIGLIST1() )
    which(names(BIGLIST1() )==input$MINTIMEBIG)->base
    which(names(BIGLIST1() )==input$MAXTIMEBIG)->top
    if(length(base)!=1){
      base<-1
    }
    if(length(top)!=1){
      top<-1
    }
    seq(from=base, to = top, by=input$PAS_TEMPS_BIGDATA)->SEQ
    print( names(BIGLIST1())[SEQ]  )
  })
  
  #### CONDITIONS ON INDIDIVUS
  ##### DEFINE DF #####
  
  values <- reactiveValues()
  values$DF_subset_initial <- data.frame("PAQUET"="", "DATE"="", "VARIABLE"="", "TYPE"="", 
                                         "TEXT_PATTERN"="","FACTOR_LEVELS"="", "min"="",  "max"="",  "DATE_min"="", "DATE_max"="", stringsAsFactors = FALSE)
  
  observe({ print(values$DF_subset_initial) })
  
  ##### DEFINE INPUTS #####
  output$UI_PAQUET_SELECT<-renderUI({
    ns <- session$ns
    shiny::numericInput(inputId=ns("PAQUET_FOR_SELECT"), value=1, label = "Paquet : ", min = 1, step = 1)
  })
  
  output$UI_DATE_SELECT<-renderUI({
    ns <- session$ns
    #reactive({
    names(BIGLIST1())->names.pick
    shiny::selectInput(inputId = ns("DATE_FOR_SELECT"), label = "Date pour sélection:",
                       choices = names.pick, multiple = TRUE)
  })
  #### SELECT VAR et MODLITE ####
  output$UI_INDVAR_CHOOSE<-renderUI({
    req(BIGLIST1())
    req(input$DataType)
    ns <- session$ns
    if(req(input$DataType)=="objet"){
      lapply(BIGLIST1(), function(bi){
        names(bi)
      })->lina
      unique(unlist(lina))->glona
      message<-h5("Renseignez la variable dans les données qui identifie des individus uniques et qui sera utilisée comme paramètre 'id' de seqdata()")
      
    } else {
      if(input$DataType=="objseq"){
        #if( OBJSEQ_COMPLEMENT())
        #names( BIGLIST1()[[1]] )->glona
        message("ERROR 490")
        message(length(BIGLIST1()))
        message(class(BIGLIST1()))
        message(names(BIGLIST1()))
        lapply(BIGLIST1(), function(bi){
          names(bi)
        })->lina
        unique(unlist(lina))->glona
        
        
        lapply(BIGLIST1(), function(bi){
          names(bi)
        })->lina
        unique(unlist(lina))->glona
        print(glona)
        message<-h5("Renseignez la variable dans les données complémentaires qui correspond à l'attribut row.names de l'objet seqdata")
      } else {
        if(input$DataType=="fichier"){
          lapply(BIGLIST1(), function(bi){
            names(bi)
          })->lina
          unique(unlist(lina))->glona
          message<-h5("Renseignez la variable dans les données qui identifie des individus uniques et qui sera utilisée comme paramètre 'id' de seqdata()")
        } else {
          if(input$DataType=="SQL"){
            names(dbGetQuery(BIGLIST1(), paste0('SELECT * FROM ', dbListTables(BIGLIST1())[1])))->glona
            message("SQL688")
            print(glona)
            message<-h5("Renseignez la variable dans les données qui identifie des individus uniques et qui sera utilisée comme paramètre 'id' de seqdata()")
            } else {NULL}
      }
      }
    }
    
    list(
      message,
      selectInput(inputId = ns("INDVAR"), label = "Variable pour identifiant individuel: ", 
                  choices = glona, multiple = FALSE)
    )
  })
  
  INDVAR<-reactive({input$INDVAR})
  
  #### DUPLI / NA
  
  control_dupli_na<-reactive({
    req(input$INDVAR)
    req(BIGLIST1())
    #if(req(input$DataType)!="fichier"){
    if(input$DataType=="objet"|input$DataType=="fichier"){
      lapply(1:length(BIGLIST1()), function(i){
        req(input$INDVAR)
        #lapply(1:length(csv.list), FUN = function(i){
        bi<-BIGLIST1()[[i]]
        bi[ , input$INDVAR]->varbi
        data.frame(
          "NA_values"=sum(is.na(varbi)),
          "Duplicated"=sum(duplicated(varbi[!is.na(varbi)]))
        )->df
        df$DATE<-names(BIGLIST1())[[i]]
        df
      })->dfresum
      do.call("rbind", dfresum)->dfresum
      if(sum(dfresum$NA_values)==0&sum(dfresum$Duplicated)==0){
        "ok"
      } else {
        dfresum
      }
    } else {"ok"}
    #} else {"ok"}
  })
  
  output$MSSG_DUPLI<-renderUI({
    ns <- session$ns
    if(is.null(control_dupli_na()))
      req(control_dupli_na())
    if(class(control_dupli_na())=="data.frame"){
      h3("Attention: la variable d'identifiant contient des doublons et/ou des NA.")
    } else {h3("OK : pas de doublons ou NA dans la varible d'identifiant.")}
  })
  
  #output$DATA_DUPLI<-renderDataTable({
  #  req(control_dupli_na())
  #  if(class(control_dupli_na())=="data.frame"){
  #    control_dupli_na()
  #  }
  #})
  
  output$DELETE_DUPLI_NA<-renderUI({
    ns <- session$ns
    req(control_dupli_na())
    if(class(control_dupli_na())=="data.frame"){
      shiny::checkboxInput(inputId = ns("deleteNA_DUPLI"), label = "Faut-il supprimer les doublons et les NA ? (sinon, choisir une autre variable)", value = TRUE)
    }
  })
  
  #### AJOUTS TIERS 2 ####
  dataCOMP<-reactive({
    if ( is.null(input$FichComp)) return(NULL)
    if(input$AJOUT_TIERS_LOG=="TRUE"){
      userData <- read.csv(file = input$FichComp$datapath, 
                           sep = input$sepcolCOMP, 
                           encoding = input$endodingCOMP,
                           header=input$headerCOMP,
                           dec=input$decCOMP)
      
      names(userData)<-sapply(names(userData), 
                              function(ni){if(substr(ni, 1, 1)%in%c("0", as.character(seq(1, 9, 1)))  ) paste("D", ni, sep=".") else ni })
      names(userData)<-gsub(pattern = "-", replacement = "_", fixed = TRUE, x=names(userData))
      return(userData)
    } else {
      return(NULL)
    }
  })
  
  observe({
    lapply(BIGLIST1(), function(bi){
      names(bi)
    })->lina
    unique(unlist(lina))->glona
    if(input$AJOUT_TIERS_LOG=="TRUE"){
      updateSelectInput(session = session, inputId = "CleBase", choices = glona)
    }
  })
  
  observe({
    names(dataCOMP())->compna
    if(input$AJOUT_TIERS_LOG=="TRUE"){
      updateSelectInput(session = session, inputId = "CleComp", choices = compna)
    }
  })
  
  
  reactive({
    req(BIGLIST1())
    if(is.null(dataCOMP())) {
      return(BIGLIST1())
    } else {
      if(input$AJOUT_TIERS_LOG=="TRUE"){
        req(input$CleBase)
        req(input$CleComp )
        if(input$CleComp %in% names(dataCOMP())){
          lapply(BIGLIST1(), function(bi){
            merge(bi, dataCOMP(), by.x=input$CleBase, by.y=input$CleComp, all.x=TRUE)->df
            df[]<-lapply(df, as.character)
            return(df)
          })->biglist.with.add.variable
          return(biglist.with.add.variable)
        } else {
          return(BIGLIST1())
          
        }
      } else {
        return(BIGLIST1())
      }
    }
  })->BIGLIST1.COMP
  
  
  #### BIGLIST2()####
  BIGLIST2<-reactive({
    req( control_dupli_na() )
    req(input$INDVAR)
    # if(input$DataType=="objet"|input$DataType=="fichier")#{
    if(class(control_dupli_na())=="data.frame"){
      req(input$deleteNA_DUPLI)
      if(input$deleteNA_DUPLI==TRUE){
        lapply(BIGLIST1.COMP(), FUN = function(bi){
          bi1<-subset(bi, !is.na(bi[ , input$INDVAR]))
          bi2<-subset(bi1, !duplicated( bi1[ , input$INDVAR] ) )
          bi2
        })
      }
    } else {
      message("CHERCHEZ l'ERREUR 1")
      print(BIGLIST1.COMP())
      return(BIGLIST1.COMP())
      message("CHERCHEZ l'ERREUR 1")
      
      
    }
    #}
  })
  
  #output$TEXTE_NROW_BIGLIST_AVANT_APRES<-renderUI({
  #  req(BIGLIST2())
  #  req(control_dupli_na())
  #  if(class(control_dupli_na())=="data.frame"){
  #h4("Nombre de lignes avant et après la supression des doublons/NA dans la variable d'identifiant : ")
  #  }
  #})
  
  output$NROW_BIGLIST_AVANT_APRES<-renderUI({
    req(BIGLIST2())
    req(control_dupli_na())
    if(class(control_dupli_na())=="data.frame"){
      data.frame(lapply(BIGLIST1(), nrow))->df1
      data.frame(lapply(BIGLIST2(), nrow))->df2
      message("control 597")
      print(control_dupli_na())
      control_dupli_na()->dfnadoub
      row.names(dfnadoub)<-dfnadoub$DATE
      names(dfnadoub)[names(dfnadoub)=="NA_values"]<-"Nb de NA dans variable d'ID"
      names(dfnadoub)[names(dfnadoub)=="Duplicated"]<-"Nb de doublons dans variable d'ID"
      data.frame(t(dfnadoub), stringsAsFactors = FALSE)->dfnadoub
      message("coucou 588")
      print(dfnadoub)
      
      message(names(dfnadoub))
      message(names(df1))
      rbind(
        dfnadoub,
        "Nb de lignes avant suppression"=df1,
        "Nb de lignes après supression"=df2
      )->df
      output$dtdoublons<-DT::renderDataTable(DT::datatable(df, caption = ""))
      DT::DTOutput(ns("dtdoublons"))
    }
  })
  
  output$NROW_BIGLIST2<-renderDataTable({
    req(BIGLIST2())
    data.frame(lapply(BIGLIST2(), nrow))->df
    DT::datatable(df)
  })
  
  ##### DEBUT BIGLIST 2 #####
  ####
  
  reactive({
    req(input$DATE_FOR_SELECT)
    
    if(length(input$DATE_FOR_SELECT)>1){
      
      BIGLIST2()[names( BIGLIST2())%in%input$DATE_FOR_SELECT]->tempdf
      
    } else {
      
      BIGLIST2()[[input$DATE_FOR_SELECT]]->tempdf
      
    }
    
    
    
    print("COUCOU 456")
    print("req(input$addvar) : ")
    print(input$addvar)
    print("end")
    if(!is.null(input$addvar)){
      if(input$addvar==TRUE){
        
        req(input$MERGEORIGINVAR, input$MERGEADDVAR)
        if((input$MERGEORIGINVAR!=""&input$MERGEADDVAR!="")|(!is.null(input$MERGEORIGINVAR)&!is.null(input$MERGEADDVAR))){
          print("COUCOU 460")
          
          print(input$MERGEORIGINVAR)
          print(input$MERGEADDVAR)
          req(ADDDATA())
          
          if(length(input$DATE_FOR_SELECT)>1){
            lapply(tempdf, function(xi){
              #left_join(xi, ADDDATA(), by=c(input$MERGEORIGINVAR, input$MERGEADDVAR))
              merge(xi, ADDDATA(), by.x=input$MERGEORIGINVAR, by.y=input$MERGEADDVAR, all.x=TRUE)
            })->tempdf2
          } else {
            #left_join(tempdf, ADDDATA(), by=c(input$MERGEORIGINVAR, input$MERGEADDVAR))->tempdf2
            merge(tempdf, ADDDATA(), by.x=input$MERGEORIGINVAR, by.y=input$MERGEADDVAR, all.x=TRUE)->tempdf2
            
          }
          
        } else {
          print("COUCOU 465")
          
          tempdf->tempdf2
        }
      } else {
        if(input$addvar==FALSE){
          tempdf->tempdf2
          print("COUCOU 470")
        }
      }
    } else {
      tempdf->tempdf2
      print("COUCOU 470")
    }
    
    #print("tempdf2")
    #print(tempdf2)
    print("COUCOU 485")
    return(tempdf2)
    
  })->the.df
  
  
  
  output$ADDDATA_CSV<-renderUI({
    ns <- session$ns
    print(input$addvar)
    if(!is.null(input$addvar)){
      if(input$addvar==TRUE){
        #req(input$addvar)
        
        list(
          
          fileInput(inputId=ns("CSVADDDATA"), label="Jeu de données : (CSV, UTF-8)", 
                    multiple = FALSE),
          shiny::selectInput(choices = c("csv (standard)"="csv", "csv2 (microsoft Excel)"="csv2"), 
                             inputId=ns("CSVTYPE"), label="Jeu de données : (CSV, UTF-8)", 
                             multiple = FALSE, selected=NULL),
          selectInput(inputId = ns("MERGEADDVAR"), label = "Variable (clé) dans le jeu de données importées: ", 
                      choices = "", multiple = FALSE, selected = NULL),
          selectInput(inputId = ns("MERGEORIGINVAR"), label = "Variable (clé) dans le jeu de données pour sélection: ", 
                      choices = "", multiple = FALSE, selected = NULL)
        )->res
        return(res)
      }
      #}
    }
  })
  
  reactive({
    if(!is.null(input$addvar)){
      if(req(input$addvar)==TRUE){
        req(input$CSVADDDATA, input$CSVTYPE)
        inFile <- input$CSVADDDATA
        if(input$CSVTYPE=="csv"){
          read.csv(inFile$datapath, header = TRUE)->datr
        }
        if(input$CSVTYPE=="csv2"){
          read.csv2(inFile$datapath, header = TRUE)->datr
        }
        datr
      }
    }
  })->ADDDATA
  
  observe({
    if(!is.null(input$addvar)){
      updateSelectInput(session=session, inputId = "MERGEADDVAR", choices = names(ADDDATA()), selected = NULL)
    }
  })
  
  observe({
    req(input$DATE_FOR_SELECT)
    if(length(input$DATE_FOR_SELECT)<=1){
      input$DATE_FOR_SELECT->dats
    } else {
      input$DATE_FOR_SELECT[1]->dats
    }
    BIGLIST2()[[dats]]->tempdf
    print(names(tempdf))
    updateSelectInput(session=session, inputId = "MERGEORIGINVAR", choices = names(tempdf), selected=NULL)
  })
  
  ####
  
  
  output$UI_VAR_SELECT<-renderUI({
    ns <- session$ns
    #req(the.df())
    #print(head(the.df()))
    print("on est là")
    #mycolumns<-unique(unlist(Reduce(intersect,list(lapply(X = list_csv(), FUN = names))), 
    #                         use.names = FALSE))
    if(class(the.df())=="data.frame"){
      selectInput(inputId = ns("VAR_FOR_SELECT"), label = "Variable pour sélection", 
                  choices = names(the.df()), multiple = FALSE)
    } else {
      if(class(the.df())=="list"){
        
        unique(unlist(Reduce(intersect,lapply(the.df(), names))))->naminun
        
        selectInput(inputId = ns("VAR_FOR_SELECT"), label = "Variable pour sélection", 
                    choices =naminun, multiple = FALSE)
      }
    }
  })
  
  THE_VAR<-reactive({
    #req( the.df() )
    req( input$VAR_FOR_SELECT)
    if(class(the.df())=="list"){
      if(sum(sapply(the.df(), FUN = function(di){!input$VAR_FOR_SELECT%in%names(di)}))==0){
        lapply(the.df(), function(di){
          di[ , input$VAR_FOR_SELECT]
        })->the.var
        
      }
    } else {
      if(class(the.df())=="data.frame"){
        the.df()[ , input$VAR_FOR_SELECT]->the.var
        
      }
      
    }
    
    the.var
  })
  
  output$UI_CLASS_SELECT<-renderUI({
    ns <- session$ns
    req(THE_VAR() )
    
    message("COUCOU 625")
    print(THE_VAR())
    if( class(THE_VAR() )=="list" ){
      class(THE_VAR()[[1]])->CLASS_VAR
    } else {
      class(THE_VAR())->CLASS_VAR
    }
    shiny::selectInput(inputId = ns("classSelect"), label = "Contrôle de la classe", 
                       choices =  c("factor", "character", "numeric", "Date", "integer"), 
                       selected = CLASS_VAR)
  })
  
  output$UI_MOD_SELECT<-renderUI({
    ns <- session$ns
    req(THE_VAR() )
    req(input$classSelect)
    
    if(class(THE_VAR())=="list"){
      unique(unlist(THE_VAR()))->vars
    } else {
      THE_VAR()->vars
    }
    
    if(input$classSelect%in%c("numeric", "integer")){
      
      as.numeric(vars)->temp.num.var
      
      minis <- min(temp.num.var, 
                   na.rm = TRUE)
      maxis <- max(temp.num.var, 
                   na.rm = TRUE)
      
      
      shiny::sliderInput(inputId = ns("NumSelect"), label = "Valeurs sélectionnées", 
                         min = minis, max = maxis, 
                         value = c(minis, maxis))
    } else {
      if(input$classSelect=="factor"){#, "character") ){
        if(length(unique(vars))>100){
          table(vars)->tab
          tab[order(tab, decreasing = TRUE)]->tab
          tab[1:25]->tab
        } else {tab<-unique(vars )}
        shiny::selectInput(inputId = ns("FactSelect"), label="Valeurs sélectionnées", 
                           choices = tab  , multiple = TRUE)
      } else {
        if(input$classSelect=="character"){#, "character") ){
          shiny::textInput(inputId = ns("CharPatSelect"), label="'Paterns' à rechercher (sep by '/'", value = ""
          )
        } else  {
          if(input$classSelect=="Date" ){
            
            renderText(print( unique(vars )[!is.na(unique(vars) )][1]  ) )->output$AFFICHDATE
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
  })
  
  observe({
    req(THE_VAR())
    req(input$DATEformat)
    
    
    if(class(THE_VAR())=="list"){
      unique(unlist(THE_VAR()))->vars
    } else {
      THE_VAR()->vars
    }
    
    print(vars)
    print(as.character(input$DATEformat))
    if(class(vars)=="Date"){
      vars->THE_VAR_DATE
    } else {
      if(class(vars)=="character"&input$DATEformat!=""){
        as.Date(as.character(vars), format=as.character(input$DATEformat) )->THE_VAR_DATE
      } else {
        if(class(vars)=="numeric"|class(vars)=="integer"){
          as.POSIXct(vars,origin="1970-01-01")->THE_VAR_DATE
        }
      }
    }
    print(THE_VAR_DATE)
    print(class(THE_VAR_DATE))
    min(THE_VAR_DATE, na.rm = TRUE)->mindate
    print(mindate)
    max(THE_VAR_DATE, na.rm = TRUE)->maxdate
    print(maxdate)
    shiny::updateDateRangeInput(session = session, inputId = "DATE_RANGE", start = mindate, end=maxdate)
  })
  
  output$UI_VIEW_VAR<-renderText({
    req(THE_VAR() )
    req(input$classSelect)
    req(the.df())
    
    if(class(THE_VAR())=="list"){
      unique(unlist(THE_VAR()))->vars
    } else {
      THE_VAR()->vars
    }
    
    vars[!is.na(vars)]->the.var2
    print(head(the.var2, 20))
  })
  
  ##### ADD ROW #####
  
  
  newEntry <- observeEvent(input$addROW, {
    
    factor_levels<-NA
    text_pattern<-NA
    minus<-NA
    maxus<-NA
    datmin<-NA
    datmax<-NA
    req(input$classSelect)
    
    if(input$classSelect=="factor"){
      
      paste("'", paste(isolate(input$FactSelect), collapse = "','"), "'", sep="")->choix
      paste("c(", choix, ")" )->factor_levels
      
    } else {
      if(isolate(input$classSelect)=="character"){
        isolate(input$CharPatSelect)->text_pattern
      } else {
        if(input$classSelect=="Date"){
          isolate(input$DATE_RANGE)->choix
          message("choix date")
          print(choix)
          as.character(choix[1])->datmin
          as.character(choix[2])->datmax
        } else {
          if(input$classSelect%in%c("numeric", "integer")){
            isolate(input$NumSelect)->choix
            choix[1]->minus
            choix[2]->maxus
          }
        }
      }
    }
    isolate(input$DATE_FOR_SELECT)->datesse
    if(length(datesse)>1){
      paste("c('" ,paste(datesse, collapse="','"), "')", sep = "")->resudatess
    } else {
      datesse->resudatess
    }
    
    data.frame("PAQUET"=isolate(input$PAQUET_FOR_SELECT),
               "DATE"=resudatess,#isolate(input$DATE_FOR_SELECT),
               "VARIABLE"=isolate(input$VAR_FOR_SELECT), 
               "TYPE"=isolate(input$classSelect),
               "TEXT_PATTERN"=text_pattern,
               "FACTOR_LEVELS"=factor_levels,
               "min"=minus,
               "max"=maxus,
               "DATE_min"=datmin,
               "DATE_max"=datmax#,
               #"Action" = buttonInput(
               #   FUN = actionButton,
               #   len = 10,
               #   id = 'button_',
               #   label = "Delete",
               #   onclick = 'Shiny.onInputChange(\"lastClick\",  this.id)'
               # )
               
    )->vecto 
    print("vecto")
    print(vecto)
    
    isolate(values$DF_subset_initial <- rbind(values$DF_subset_initial , vecto))
    
    #  newLine <- isolate(c(input$text1, input$text2))
    #  isolate(values$df <- rbind(values$df, newLine))
  })
  #### DELETE ROWS ####
  
  observeEvent(input$delROW,{
    
    if (!is.null(input$TABLE_POUR_SELECTION_rows_selected)) {
      
      values$DF_subset_initial <- values$DF_subset_initial[-as.numeric(input$TABLE_POUR_SELECTION_rows_selected),]
    }
    
  })
  
  output$TABLE_POUR_SELECTION<-DT::renderDataTable(
    values$DF_subset_initial,
    #    server = FALSE,
    rownames = FALSE,
    filter = "none",
    escape=FALSE,
    editable = list(target = "row"
                    #disable = list(columns = c(1, 2))
    )
  )
  #### MARQUEUR DEFINE SUBSET #### BIGLIST2() ####
  shiny::reactive({
    req(  values$DF_subset_initial )
    data_of_subset <-  values$DF_subset_initial
    
    string_for_sub<-sapply(1:nrow(data_of_subset), function(i){
      if(grepl(pattern = ",", fixed = TRUE, x = data_of_subset$DATE[i])){
        eval(parse(text = data_of_subset$DATE[i]))->vecda
        
        paste(sapply(vecda, function(vecda.i){
          
          paste("BIGLIST2()$", vecda.i, sep="" )->dfvar
          if(data_of_subset$TYPE[i]=="factor"){
            paste( dfvar, "$", data_of_subset$VARIABLE[i], 
                   "%in%",  data_of_subset$FACTOR_LEVELS[i], sep = "")
          } else {
            if(data_of_subset$TYPE[i]=="character"){
              paste("grepl('",  data_of_subset$TEXT_PATTERN[i],"'", 
                    ", ", dfvar, "$", data_of_subset$VARIABLE[i], ", fixed=TRUE)", sep = "")
            } else {
              if(data_of_subset$TYPE[i]%in%c("numeric", "integer") ){
                paste("(", 
                      paste(dfvar, "$", data_of_subset$VARIABLE[i], ">=", data_of_subset$min[i], sep = ""),
                      "&",
                      paste(dfvar, "$", data_of_subset$VARIABLE[i], "<=", data_of_subset$max[i], sep = ""),
                      ")", sep="")
              } else {
                if(data_of_subset$TYPE[i]=="Date" ){
                  
                  paste("(", 
                        paste( "as.Date(",   dfvar, "$", data_of_subset$VARIABLE[i], ",",
                               "format=", "'", input$DATEformat, "')",
                               ">=", 
                               "as.Date('" ,  data_of_subset$DATE_min[i], "',",
                               "format=", "'", "%Y-%m-%d", "')", sep=""
                        ),
                        "&",
                        paste( "as.Date(" ,  dfvar, "$", data_of_subset$VARIABLE[i], ",",
                               "format=", "'", input$DATEformat, "')",
                               "<=", 
                               "as.Date('" ,  data_of_subset$DATE_max[i], "',",
                               "format=", "'", "%Y-%m-%d", "')", sep=""
                        ),
                        ")", sep="")
                  
                } else{ "nosub" }
              }
            }
          }
        }), collapse="&")
        
        
      } else {
        paste("BIGLIST2()$", data_of_subset$DATE[i], sep="" )->dfvar
        if(data_of_subset$TYPE[i]=="factor"){
          paste( dfvar, "$", data_of_subset$VARIABLE[i], 
                 "%in%",  data_of_subset$FACTOR_LEVELS[i], sep = "")
        } else {
          if(data_of_subset$TYPE[i]=="character"){
            paste("grepl('",  data_of_subset$TEXT_PATTERN[i],"'", 
                  ", ", dfvar, "$", data_of_subset$VARIABLE[i], ", fixed=TRUE)", sep = "")
          } else {
            if(data_of_subset$TYPE[i]%in%c("numeric", "integer") ){
              paste("(", 
                    paste(dfvar, "$", data_of_subset$VARIABLE[i], ">=", data_of_subset$min[i], sep = ""),
                    "&",
                    paste(dfvar, "$", data_of_subset$VARIABLE[i], "<=", data_of_subset$max[i], sep = ""),
                    ")", sep="")
            } else {
              if(data_of_subset$TYPE[i]=="Date" ){
                
                paste("(", 
                      paste( "as.Date(",   dfvar, "$", data_of_subset$VARIABLE[i], ",",
                             "format=", "'", input$DATEformat, "')",
                             ">=", 
                             "as.Date('" ,  data_of_subset$DATE_min[i], "',",
                             "format=", "'", "%Y-%m-%d", "')", sep=""
                      ),
                      "&",
                      paste( "as.Date(" ,  dfvar, "$", data_of_subset$VARIABLE[i], ",",
                             "format=", "'", input$DATEformat, "')",
                             "<=", 
                             "as.Date('" ,  data_of_subset$DATE_max[i], "',",
                             "format=", "'", "%Y-%m-%d", "')", sep=""
                      ),
                      ")", sep="")
                
                
                
                
                # paste("(", 
                #       paste( "as.Date(",   dfvar, "$", data_of_subset$VARIABLE[i], ",",
                #              "format=", "'", input$DATEformat, "')",
                #              ">=", 
                #              "as.Date('" ,  data_of_subset$DATE_min[i], "',",
                #              "format=", "'", "%Y-%m-%d", "')", sep=""
                #       ),
                #       "&",
                #       paste( "as.Date(" ,  dfvar, "$", data_of_subset$VARIABLE[i], ",",
                #              "format=", "'", input$DATEformat, "')",
                #              "<=", 
                #              "as.Date('" ,  data_of_subset$DATE_max[i], "',",
                #              "format=", "'", "%Y-%m-%d", "')", sep=""
                #       ),
                #       ")", sep="")
                
                
              } else{ "nosub" }
            }
          }
        }
      }
    })
    
    data.frame("PAQUET"=data_of_subset$PAQUET, DATE=data_of_subset$DATE, "string_for_sub"=string_for_sub, 
               stringsAsFactors = FALSE)->res
    res[res$string_for_sub=="nosub", "PAQUET"]<-0
    res
  })->STRING_FOR_SUB
  observe(print(values$DF_subset_initial))
  observe(print(STRING_FOR_SUB()))
  
  
  INDIVIDUELS<-reactive({
    
    req(INDVAR())
    
    if(input$addCONDS==TRUE){
      req(STRING_FOR_SUB() )
      print(STRING_FOR_SUB())
      if(nrow(STRING_FOR_SUB())<2&STRING_FOR_SUB()$string_for_sub[1]=="nosub"){
        message("COUCOU 790")
        lapply(BIGLIST2(), function(bil){
          bil[ , INDVAR()]
        })->list.ind.no.subset
        unique(unlist(list.ind.no.subset))
        
      } else {
        message("COUCOU 797")
        
        #subset(STRING_FOR_SUB(), STRING_FOR_SUB()$string_for_sub[1]!="nosub")->dfsub
        list.of.inf.by.cond<-lapply(1:nrow(STRING_FOR_SUB() ), function(i){
          if(STRING_FOR_SUB()$string_for_sub[i]=="nosub"){
            BIGLIST2()[[STRING_FOR_SUB()$DATE[i]]][ , INDVAR()]
          } else {
            
            message("coucou 1084")
            print(STRING_FOR_SUB())
            message("coucou 1086")
            print(names(BIGLIST2()))
            message("coucou 1088")
            
            print(STRING_FOR_SUB()$DATE[i])
            print(STRING_FOR_SUB()$DATE[i]%in%names(BIGLIST2()))
            
            if(grepl(pattern = ",", x = STRING_FOR_SUB()$DATE[i], fixed = TRUE)){
              as.character(STRING_FOR_SUB()$string_for_sub[i])->multidatesub
              strsplit(x = multidatesub, split = "&", fixed = TRUE)[[1]]->multconditionsdates
              lapply(multconditionsdates, function(cond.i){
                
                message("coucou1158")
                message(cond.i)
                
                sapply(names(BIGLIST2()), function(dat.i){
                  regexpr (pattern = dat.i, text = cond.i, fixed = TRUE)->datinfo
                  substr(cond.i, start = datinfo[1], stop = ((datinfo[1]+ attributes(datinfo)$match.length)-1) )->thedat
                  thedat
                })->thedat
                
                message("coucou1162")
                if(sum(gregexpr("(", cond.i, fixed=TRUE)[[1]] > 0)!=sum(gregexpr(")", cond.i, fixed=TRUE)[[1]] > 0)){
                  if(startsWith(cond.i, "(")==TRUE&endsWith(cond.i, ")")==FALSE){
                    substr(cond.i, start=2, stop=nchar(cond.i))->cond.i
                  }
                  if(startsWith(cond.i, "(")==FALSE&endsWith(cond.i, ")")==TRUE){
                    substr(cond.i, start=1, stop=(nchar(cond.i)-1) )->cond.i
                  }
                }
                sapply(thedat, function(di){
                  subset(BIGLIST2()[[di]], 
                         eval(parse(text = cond.i )) )[ , INDVAR()]->res.di
                  res.di
                })->resdi
                resdi[lengths(resdi) != 0]->resdi
                Reduce(intersect, resdi)->res
                message("close 1154")
                message("cond.i")
                print(cond.i)
                message("thedat")
                print(thedat)
                message("resdi")
                print(resdi)
                message("res")
                print(res)
                return(res)
                
                
              })->IND.BY.DATE.COND
              
              return(Reduce(intersect, IND.BY.DATE.COND))
              ### CONDIRTON ET
              
              #BIGLIST2()$D2017_01_01$RSA_simple%in%c( 'NO.RSA','RSA' )&BIGLIST2()$D2018_01_01$RSA_simple%in%c( 'NO.RSA','RSA' )
              
            } else {
              subset(BIGLIST2()[[STRING_FOR_SUB()$DATE[i]]], 
                     eval(parse(text = as.character(STRING_FOR_SUB()$string_for_sub[i]) )) )[ , INDVAR()]
            }
            
            
          }
        })
        #names(list.of.inf.by.cond)<-
        
        print(list.of.inf.by.cond)
        message("COUCOU 809")
        
        output$CONTROL_LIST.OF.INF<-renderText({unlist(list.of.inf.by.cond)})
        
        lapply(unique(STRING_FOR_SUB()$PAQUET), function(pi){
          which(STRING_FOR_SUB()$PAQUET==pi)->indexes.pi
          unique(unlist(list.of.inf.by.cond[indexes.pi]))
        })->ind.by.paq
        ind.by.paq[lengths(ind.by.paq) != 0]->ind.by.paq
        print(ind.by.paq)
        message("COUCOU 819")
        
        Reduce(intersect, ind.by.paq)->ind.all.paq
        ind.all.paq
      }
    } else {
      lapply(BIGLIST2(), function(bil){
        bil[ , INDVAR()]
      })->list.ind.no.subset
      unique(unlist(list.ind.no.subset))
    }
  })
  
  reactive({
    req(BIGLIST2() )
    if(input$addCONDS==TRUE){
      req(INDIVIDUELS() )
      lapply(BIGLIST2(), FUN = function(bi){
        subset(bi, bi[ , INDVAR()]%in%INDIVIDUELS())
      })
    } else {
      BIGLIST2()
    }
  })->SUBSETTED_LIST
  
  output$LENGTH_IND_SUBS<-renderText({ length( INDIVIDUELS() )    }) 
  output$LENGTH_BIGLIST<-renderText({ length( BIGLIST2() )    }) 
  output$LENGTH_SUBSETTED<-renderText({ length( SUBSETTED_LIST() )    }) 
  output$LENGTH_BIGLIST2<-renderText({ length( BIGLIST2() )    }) 
  
  
  
  observe({
    SUBSETTED_LIST()->data.to.save 
    output$downlist <- shiny::downloadHandler(filename = "mes_datas.RData", 
                                              content =  function(file) {
                                                save(data.to.save, file = file)
                                              } )
  })
  
  
  #### Chargement des données ####
  
  #### Chargement premier fichier de données ####
  #renderPrint(print(length(SUBSETTED_LIST())))->output$CONTROLDATA
  
  #### Paramétrage des trajectoires ####
  ####  DATE DEBUT ET FIN POUR TRAJ ####
  output$DATA_UI<-shiny::renderUI({
    ns <- session$ns
    #if(input$DataType=="fichier"){
    #  shiny::dateRangeInput(inputId = ns("date.range"), label = "Dates de début et de fin",
    #                        format = "mm-yyyy")->the.ui
    #} else {
    if(input$DataType=="objet"|input$DataType=="fichier"){
      names(SUBSETTED_LIST())->names.pick
      message("coucou1279")
      mycolumns<-unique(unlist(Reduce(intersect,list(lapply(X = SUBSETTED_LIST(), FUN = names))),
                               use.names = FALSE))
      print(mycolumns)
      if(length(SUBSETTED_LIST())>1){
        updateSelectInput(session = session, inputId = "timecol", choices = mycolumns, label = "Choisir la variable commune aux data.frame utilisée pour construire les trajectoires:")
        list(
          shiny::selectInput(inputId = ns("PICKDATE1.deb"), label = "Debut des trajectoires :",
                             choices = names.pick, multiple = FALSE, selected = names.pick[1]),
          shiny::selectInput(inputId = ns("PICKDATE1.fin"), label = "Fin des trajectoires :",
                             choices = names.pick, multiple = FALSE, selected = names.pick[length(names.pick)] ),
          shiny::numericInput(inputId = ns("PAS_TEMPS_TRAJ"), label = "Pas de temps pour les trajectoires : ", 
                              value = 1, min = 1, step = 1, width = '100%'))->the.ui
      } else {
        if(length(SUBSETTED_LIST())==1){
          updateSelectInput(session = session, inputId = "timecol", 
                            choices = mycolumns, 
                            label = "Choisir les variables temporelles (par ordre chronologique):")
          h3("")->the.ui
        }
      }
    } else {h3("error")->the.ui}
    #}
    the.ui
  })
  ####  data.seq ####
  
  DR_POUR_SEQ_OBJ<-reactive({# eventExpr = input$ValidParametres,  {
    req(SUBSETTED_LIST())
    if(input$DataType=="objet"&length(SUBSETTED_LIST())>1){
      req(SUBSETTED_LIST())
      req(input$timecol)
      
      
      lapply(SUBSETTED_LIST(), function(df.i){
        
        names(df.i)[grepl(pattern = input$INDVAR, x = names(df.i), ignore.case = FALSE)]->name.code
        names(df.i)[grepl(pattern = input$timecol, x = names(df.i), ignore.case = FALSE)]->name.RSA_simple
        df.i[ , c(name.code, name.RSA_simple)]
      })->DATAlist.sampled.simple
      
      
      Reduce(function(x, y) merge(x, y, by.x=names(x)[grepl(pattern = input$INDVAR, x = names(x), ignore.case = FALSE)], 
                                  by.y=names(y)[grepl(pattern = input$INDVAR, x = names(y), ignore.case = FALSE)], 
                                  all=TRUE), DATAlist.sampled.simple)->df_RSA.sampled
      print(head(df_RSA.sampled))
      
      
      
      names(df_RSA.sampled)<-c(input$INDVAR, names(SUBSETTED_LIST()))# c(input$INDVAR, paste(1:(ncol(df_RSA.sampled)-1), "_VAR"))
      
      print(head(df_RSA.sampled))
      
      print(names(df_RSA.sampled))
      print(names(df_RSA.sampled))
      
      
      merge(df_RSA.sampled, 
            SUBSETTED_LIST()[[1]][ , !grepl(pattern = input$timecol, x = names(SUBSETTED_LIST()[[1]]))], 
            by.x = input$INDVAR, 
            by.y =  names( SUBSETTED_LIST()[[1]] )[grepl(pattern = input$INDVAR, x=names(SUBSETTED_LIST()[[1]]), fixed = TRUE)][1])->df_RSA.sampled2
      
      df_RSA.sampled2->df.pour.seq
      
      print("THE DATA")
      print(head(df_RSA.sampled2))
      print("INDVAR")
      print(input$INDVAR)
      print("grepl input$timecol")
      print(head(df_RSA.sampled[ , grepl(pattern = input$timecol, x=names(df_RSA.sampled))]))
      
      df.pour.seq
    } else {
      if(input$DataType=="fichier"|(input$DataType=="objet"&length(SUBSETTED_LIST())==1)){
        req(SUBSETTED_LIST())
        req(input$timecol)
        message("COUCOU 1242")
        print(input$timecol)
        
        if (length(input$timecol)<2){
          showModal(modalDialog(
            title = "Important message",
            "Il faut mettre au moins deux variables temporelles.",
            easyClose = TRUE
          ))
        }
        else {
          message("coucou 1259")
          print(head(SUBSETTED_LIST()[["Table.unique"]]))
          SUBSETTED_LIST()[["Table.unique"]][ , c(input$INDVAR,  input$timecol)]->df.pour.seq
          message("coucou 1261")
          print(head(df.pour.seq))
          return(df.pour.seq)
        }
      }
    }
    
  })
  
  observe({
    if(input$DataType=="objet"&length(SUBSETTED_LIST())>1){
      lapply(BIGLIST2(), function(dt){
        dt[ , input$timecol]
      })->li.var
      as.character(unique(unlist(li.var)))->choix.missing
    } else {
      as.character(unique(unlist(DR_POUR_SEQ_OBJ()[,input$timecol])))->choix.missing
    }
    updateSelectInput(session = session, inputId = "MISSING_forseq", 
                      choices = c("aucun", choix.missing), selected = NULL)#"aucun")
  })
  
  missing_codes<-reactive({
    
    if(length(input$MISSING_forseq)<1|is.null(input$MISSING_forseq)){
      NA
    } else {
      if(length(input$MISSING_forseq)==1&input$MISSING_forseq=="aucun"){
        NA
      } else {
        input$MISSING_forseq
      }
    }
  })
  
  
  observe({
    req(DR_POUR_SEQ_OBJ())
    req(input$INDVAR)
    req(input$timecol)
    
    
    if(input$DataType=="fichier"|(input$DataType=="objet"&length(SUBSETTED_LIST())==1)){
      DR_POUR_SEQ_OBJ()[,input$timecol]->dataforseq
    } else {
      if(input$DataType=="objet"&length(SUBSETTED_LIST())>1){
        
        DR_POUR_SEQ_OBJ()[ , names(SUBSETTED_LIST())]->dataforseq#grepl("_VAR", x = names(DR_POUR_SEQ_OBJ()))&names(DR_POUR_SEQ_OBJ())!=input$INDVAR]->dataforseq
      }
    }
    
    updateNumericInput(session = session, inputId = "criterNb", max = (ncol(dataforseq)-1))
  })
  
  data.seq<-eventReactive(eventExpr = input$ValidParametres, {
    
    #### SI FICHOER ####
    if(input$DataType=="fichier"|(input$DataType=="objet"&length(SUBSETTED_LIST())==1)){
      # updateNumericInput(session=session, inputId = "PAStrate",value=1) 
      DR_POUR_SEQ_OBJ()[,input$timecol]->dataforseq
      dataforseq[]<-lapply(dataforseq, as.character)
      for(miss.i in missing_codes()){
        dataforseq[dataforseq==miss.i]<-NA
      }
      s<-seqdef_modgap(data = dataforseq,
                       cpal = NULL,
                       id = DR_POUR_SEQ_OBJ()[,input$INDVAR],
                       gaps = input$TEXT_GAP,
                       right = input$TEXT_RIGHT,
                       left = input$TEXT_LEFT,
                       missing=NA,
                       nr = "RMA", 
                       minimal.gap = input$criterNb, regle.pour.faux.gap = "after")
      return(s)
      
    } else {
      #### SI OBJET ####
      
      if(input$DataType=="objet"&length(SUBSETTED_LIST())>1){
        
        DR_POUR_SEQ_OBJ()->df.pour.seq
        
        
        print(DR_POUR_SEQ_OBJ()[ , input$INDVAR][duplicated(DR_POUR_SEQ_OBJ()[ , input$INDVAR])])
        
        if(!is.null(DR_POUR_SEQ_OBJ())){
          
          # DR_POUR_SEQ_OBJ()[ , grepl("_VAR", x = names(DR_POUR_SEQ_OBJ()))&names(DR_POUR_SEQ_OBJ())!=input$INDVAR]->dataforseq
          DR_POUR_SEQ_OBJ()[ , names(SUBSETTED_LIST())]->dataforseq
          
          
          dataforseq[ , seq(which(names(dataforseq)==input$PICKDATE1.deb), which(names(dataforseq)==input$PICKDATE1.fin), by = input$PAS_TEMPS_TRAJ)]->dataforseq
          
          
          dataforseq[]<-lapply(dataforseq, as.character)
          for(miss.i in missing_codes()){
            dataforseq[dataforseq==miss.i]<-NA
          }
          
          seqdef(id = DR_POUR_SEQ_OBJ()[ , input$INDVAR], 
                 data = dataforseq,
                 gaps = input$TEXT_GAP,
                 right = input$TEXT_RIGHT,
                 left = input$TEXT_LEFT,nr = "RMA", missing=NA
          )->s
        } else {s<-NULL}
      } else {
        #### SI OBJSEQ####
        if(input$DataType=="objseq"){
          req(OBJSEQ())
          df.pour.seq<-OBJSEQ_COMPLEMENT()
          s<-OBJSEQ()
        }
      }
    }
    
    if (!is.null(s)){
      if (length(alphabet(s))<=12&!is.null(s)){
        #permet d'avoir les mêmes couleurs que pour les graphiques de flux
        a<-col_flux(data = df.pour.seq, seq.data = s)
        attr(s, "cpal") <- unname(a[alphabet(s)])
      }
    }
    
    if (!is.null(s)){
      if( sum(is.na(attributes(s)$cpal))==length(attributes(s)$alphabet) ){
        library(wesanderson)
        cpal(s)<-wes_palette(name = "Darjeeling1", 
                             n = length(attributes(s)$alphabet), 
                             type = "continuous")
      }
    }
    
    return(s)
    
  })
  
  
  INDVAR_UNI<-reactive({
    req(data.seq())
    if(input$DataType=="objet"|input$DataType=="objseq"){
      req(input$INDVAR)
      input$INDVAR->idvar
    } else {
      if(input$DataType=="fichier"){
        req(input$rownames_par)
        input$rownames_par->idvar
      }
    }
    idvar
  })
  
  output$CLASS_TRAJ_OBS<-renderPrint({
    class( data.seq() )
    #summary(data.seq())
  })
  
  output$DES_TRAJ_OBJ<-renderUI({
    ns <- session$ns
    h5(paste("nombre d'individus (lignes) : ",  dim( data.seq() )[1], " | longueur des trajectoires (colonnes) : ", dim( data.seq() )[2]), sep="")
    #summary(data.seq())
  })
  
  output$ATTR_TRAJ_OBJ<-renderUI({
    ns <- session$ns
    attributes(data.seq() )->list.attr
    print(list.attr)
    list.attr[names(list.attr)!="row.names"]->list.attr
    lapply(1:length(list.attr), function(li){
      list(
        h5(paste(names(list.attr)[li], " : ", sep = "")),
        renderPrint({list.attr[[li]]})
      )
    })
    #summary(data.seq())
  })
  
  
  nom_var_seq<-reactive({
    names( data.seq() )
  })
  
  data2<-reactive({
    #if(!exists("df_pour_class")&is.null(input$nb_cluster)){
    if(input$DataType=="fichier"){ 
      data()
    } else {
      if(input$DataType=="objet"){ 
        DR_POUR_SEQ_OBJ()
      } else {
        if(input$DataType=="objseq"){
          OBJSEQ_COMPLEMENT()
        }
      }
    }
    #} else {
    #  dataCluster()
    #}
  })
  
  reactive({
    if(input$DataType=="objet"){
      SUBSETTED_LIST()
    } else {
      if(input$DataType=="objseq"){
        if(class(OBJSEQ_COMPLEMENT())=="data.frame"){
          list("DATE_UNIQUE"=OBJSEQ_COMPLEMENT())
        } else {
          if(class(OBJSEQ_COMPLEMENT())=="list"){
            OBJSEQ_COMPLEMENT()
          }
        }
      } else {
        if(input$DataType=="fichier"){
          if(class(data2())=="data.frame"){
            list("DATE_UNIQUE"=data2())
          } else {
            if(class(data2())=="list"){
              data2()
            }
          }
        }
      }
    }
  })->data_for_res
  
  reactive({
    req(input$INDVAR)
    req(data_for_res())
    lapply(data_for_res(), FUN = function(x){
      
      x[ , input$INDVAR]->indpresents
      
      row.names(data.seq())[!row.names(data.seq())%in%indpresents]->notpresents
      print(indpresents)
      print(notpresents)
      
      if(length(notpresents)>0){
        data.frame(
          lapply(names(x), function(xi){rep(NA, times=length(notpresents))}), 
          stringsAsFactors = FALSE)->dfx.add
        names(dfx.add)<-names(x)
        message("coucou1433")
        message(names(dfx.add))
        dfx.add[ , which(names(dfx.add)==input$INDVAR)]<-notpresents
        message("coucou1436")
        message(names(dfx.add))
        message("coucou1438")
        message(names(x))
        data.frame(rbind(x, dfx.add), stringsAsFactors = FALSE)->dfx
      } else {
        x->dfx
      }
      dfx[match(row.names(data.seq()), dfx[ , input$INDVAR]),]->dfx
      return(dfx)
    })
  })->data_for_res2
  
  
  
  res<-reactive({
    req(data.seq())
    req(data_for_res2())
    list("SEQ"=data.seq(), "DATA"=data_for_res2())->res
    res
  })
  
  output$DOWNSEQ <- downloadHandler(
    filename =  function(){
      paste("mes_trajectoires.RData")
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
  
  
  return(#reactive({
    list("SEQ_OBJ"=reactive({data.seq()}), 
         "DATA_COMP"=reactive({data_for_res2()}), 
         "TYPE_SOURCE"=reactive({input$DataType}), 
         "CODAGE_MANQUANT"=reactive({list("GAP"=input$TEXT_GAP, "RIGHT"=input$TEXT_RIGHT, "LEFT"=input$TEXT_LEFT)}),
         "ID_VAR"=reactive({as.character(input$INDVAR)})
    )
    #})
  )
}