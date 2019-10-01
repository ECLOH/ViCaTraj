#' @title   type de file, select of ind and data
#' @description  A shiny Module that imports data, select them and export as output: data.for.seq, data.comp
#'
#' @param id shiny id
#' @param label fileInput label
#' @importFrom stringr str_detect
#' @export
module_data_UI <- function(id){#label = "CSV file") {
  ns <- NS(id)

  
  tabPanel("Les données",
           shiny::actionButton(inputId = ns("ValidParametres"), label = "Je valide ces trajectoires"),
           shiny::downloadButton(outputId = "downseq", label = "Enregistrer les trajectoires et leurs données complémentaires sur le disque : " ),
           hr(),
           #### SUB-PANEL: PARAMETRAGE ####
           tabsetPanel(id = "tabpan",
                       
                       tabPanel("TEST",
                                numericInput(inputId = ns("numinput"), label = "numeric input test", value = 5, min = 1 ,max = 10),
                                uiOutput(ns("ui_server_created")),
                                uiOutput(ns("ui_sever_created2"))
                                         
                       ),
                       
                       tabPanel(title = "Import et formattage des données : ",
                                #### CHARGEMENT FICHIER ####
                                sidebarPanel(
                                  h3("Chargement du fichier"), 
                                  width = 12,
                                  shiny::column(width = 6,
                                                shiny::selectInput(inputId = ns("DataType"), label = "Choix du type de données", 
                                                                   choices = c("Un objet RData contenant de multiples data.frame"="objet", 
                                                                               "Un objet RData contenant un objet seqdata"="objseq",
                                                                               "Un seul fichier.csv contenant des données prêtes à l'emploi"="fichier" 
                                                                   ), 
                                                                   multiple = FALSE, selected = "fichier")
                                  ),
                                  shiny::column(width = 6,
                                                
                                                conditionalPanel(
                                                  condition = paste0("input['", ns("DataType"), "'] == 'fichier'"),
                                                  #condition = "input.DataType == 'fichier'",
                                                  
                                                  fileInput(inputId=ns("file1"), label="Sélectionnez votre fichier source:", 
                                                            multiple = FALSE, accept = c("text/csv",
                                                                                         "text/comma-separated-values,text/plain",
                                                                                         ".csv"), width = NULL)
                                                ), 
                                                conditionalPanel(
                                                  #condition = "input.DataType == 'objet'",
                                                  condition = paste0("input['", ns("DataType"), "'] == 'objet'"),
                                                  h5("INFO: pour des raisons de sécurité il n'est pas possible de charger directement un dossier dans un navigateur web. Vous pouvez utiliser la fonction LIST_MULTIPLE_CSV du package ViCaTraj pour créer l'objet RData à partir de mulitples fichiers .csv"),
                                                  fileInput(inputId=ns("LIST_SOURCE_BIG_DF"), 
                                                            label="Sélectionner l'objet .RData contenant les multiples data.frame", 
                                                            multiple = FALSE, accept = NULL, width = NULL)
                                                  
                                                  #hr()
                                                  #shiny::textOutput("CONTROLDATA"))
                                                ),
                                                conditionalPanel(
                                                  #condition = "input.DataType == 'objseq'",
                                                  condition = paste0("input['", ns("DataType"), "'] == 'objseq'"),
                                                  
                                                  h5("INFO: vous pouvez charger un objet .RData contenant une liste d'objet, dont au moins un objet de type seqdata, et éventuellement un ou des data.frames complémentaires"),
                                                  fileInput(inputId=ns("LIST_SEQ"), 
                                                            label="Sélectionner l'objet .RData contenant l'objet seqdata", 
                                                            multiple = FALSE, accept = NULL, width = NULL)
                                                  
                                                  #hr()
                                                  #shiny::textOutput("CONTROLDATA"))
                                                )
                                  ),
                                  conditionalPanel(
                                    #condition = "input.DataType == 'objet'||input.DataType == 'objseq'",
                                    condition = paste0("input['", ns("DataType"), "'] == 'objet'||input['", ns("DataType"), "'] == 'objseq'"),
                                    
                                    
                                    uiOutput(ns("UI_INDVAR_CHOOSE")),
                                    uiOutput(ns("MSSG_DUPLI")),
                                    DT::dataTableOutput(ns("DATA_DUPLI")),
                                    uiOutput(ns("DELETE_DUPLI_NA"))
                                  )),
                                #hr(),
                                #### FORMAT DONNNEES ####
                                
                                sidebarPanel( h3("Format des données"), 
                                              width = 12,
                                              #### condition = "input.DataType == 'fichier'"  ####
                                              conditionalPanel(
                                                #condition = "input.DataType == 'fichier'",
                                                condition = paste0("input['", ns("DataType"), "'] == 'fichier'"),
                                                
                                                shiny::selectInput(inputId=ns("sepcol"), label= "Separateur de colonnes", choices=c("Virgule" = ",","Point-Virgule" = ";","Tabulation" = "\t"), selected=","),
                                                shiny::selectInput(inputId=ns("dec"), label= "Séparateur décimal", choices=c("Virgule" = ",","Point" = "."), selected="."),
                                                shiny::selectInput(inputId=ns("endoding"), label= "Comment est codé le fichier ? Les accents sont-ils correctement lus ?", choices=c(UTF8 = "UTF-8", Latin1 = "latin1"), selected = "UTF-8", multiple = FALSE, width = "50%"),
                                                shiny::checkboxInput(inputId = ns("header"), label="La première ligne correspond-elle aux noms des variables ?",value=FALSE),  
                                                shiny::checkboxInput(inputId = ns("rowname"), label="Une variable correspond-elle à un identifiant des individus ?",value=FALSE),
                                                conditionalPanel(
                                                  #condition = "input.rowname == true",
                                                  condition = paste0("input['", ns("rowname"), "'] == true"),
                                                  
                                                  shiny::selectInput(inputId=ns("rownames_par"), label="Variable servant d'identifiant", 
                                                                     choices = "", multiple = FALSE,selected = NULL, selectize = TRUE)),
                                                shiny::selectInput(inputId = ns("na"), label = "Codage des valeurs manquantes", choices = c("Vide" , "Espace" = " ", "NA" = "NA"), selected = "NA", multiple = TRUE, selectize = TRUE)
                                              ),
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
                                                shiny::column(width = 6, "Noms des data.frame présents dans la base : "),  
                                                shiny::column(width = 6, textOutput(ns("CONTROLNAMES"))),
                                                shiny::column(width = 6, "Noms des data.frame sélectionnés : "),  
                                                shiny::column(width = 6, textOutput(ns("SLIDERTEXT")))
                                              )
                                ),
                                hr(),
                                
                                #### condition = "input.DataType == 'objet'"  ####
                                sidebarPanel( h3("Sélection des individus:"), 
                                              width = 12,
                                              
                                              shiny::checkboxInput(inputId=ns("addCONDS"), label = "Ajouter des conditions ? ", value = FALSE),
                                              
                                              conditionalPanel(
                                                #condition = "input.DataType == 'objet' && input.addCONDS == 1",
                                                condition = paste0("input['", ns("DataType"), "'] == 'objet' && input['", ns("addCONDS"), "']==1"),
                                                
                                                sidebarPanel( #h3("Sélection des individus:"), 
                                                  width = 12,
                                                  #uiOutput("UI_INDVAR_CHOOSE"),#,
                                                  hr(),
                                                  #shiny::column(width=6, 
                                                  uiOutput(ns("UI_PAQUET_SELECT")),
                                                  h5("INFO: "),
                                                  h5("toutes les conditions ajoutées dans le même paquet ne sont pas additives (utilisation de l'opérateur logique 'ou' ('|')."),
                                                  h5("toutes les conditions ajoutées dans des paquets différents sont additives (utilisation de l'opérateur logique 'et' entre les paquets de conditions ('&'))."),
                                                  
                                                  shiny::uiOutput(ns("UI_DATE_SELECT")),
                                                  
                                                  shiny::checkboxInput(inputId = ns("addvar"), label = "Ajouter une variable d'un jeu de donnée tiers?", value = FALSE),
                                                  
                                                  shiny::uiOutput(ns("ADDDATA_CSV")),
                                                  
                                                  uiOutput(ns("UI_VAR_SELECT")),#), 
                                                  #shiny::column(width=12, 
                                                  uiOutput(ns("UI_CLASS_SELECT")),#),
                                                  uiOutput(ns("UI_MOD_SELECT")),
                                                  hr(),
                                                  
                                                  actionButton(inputId=ns("addROW"), label = "Ajouter la condition"),
                                                  #),
                                                  DT::DTOutput(ns("TABLE_POUR_SELECTION"))
                                                  #actionButton(inputId="APPLICATE_SUBSET", label = "Appliquer les conditions"),
                                                  
                                                  #tableOutput("SUBSET_OUTPUT"),
                                                  #textOutput("SUBSET_BY_PAQUET_OUTPUT"),
                                                  #textOutput("SUBSET_G_OUTPUT"),
                                                  #DTOutput("DATA_OF_SUBSET_CONTROL")
                                                )
                                              ),
                                              textOutput(ns("LENGTH_IND_SUBS")),
                                              textOutput(ns("LENGTH_SUBSETTED")),
                                              textOutput(ns("LENGTH_BIGLIST")),
                                              textOutput(ns("LENGTH_BIGLIST1")),
                                              
                                              shiny::downloadButton(outputId = "downlist", label = "Enregistrer le jeu de données sur le disque (pour réutilisation ultérieure)")
                                              
                                              #)
                                )
                                
                       ),
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
                                                       choices = "", selected = "PrestationRSA.SituationDossierRSA.EtatDossierRSA.ETATDOSRSA", multiple = TRUE, selectize = TRUE),
                                    shiny::uiOutput(ns("DATA_UI")),
                                    shiny::textInput(inputId = ns("TEXT_GAP"), label = "Label pour les 'gaps' : ", value = "GAP"),
                                    shiny::textInput(inputId = ns("TEXT_RIGHT"), label = "Label pour les censures à droite : ", value = "CENSURE"),
                                    shiny::textInput(inputId = ns("TEXT_LEFT"), label = "Label pour les départs tardifs : ", value = "LEFT"),
                                    shiny::numericInput(inputId = ns("criterNb"), label = "Critère de sortie : nombre de mois consécutifs",value = 3, min = 1, max = 36, step = 1),
                                    uiOutput(ns("CONTROL_DUPLICATED_ID"))
                                  ))
                       ),
                       tabPanel(title="Résumé des trajectoires:",
                                
                                sidebarPanel(
                                  width = 12,
                                  textOutput("DES_TRAJ_OBJ"),
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
module_data <- function(input, output, session) {
  library(TraMineR)
  library(RColorBrewer)
  ns <- session$ns
  
  output$ui_server_created<-renderUI({
    print(input$numinput)
    shiny::numericInput(inputId=ns("NEW_SELECT"), value=100*input$numinput, label = "SERVEUR CREATED UI",
                        min = 100*input$numinput-10, max = 100*input$numinput+10, step = 1)
  })
  
  output$ui_sever_created2<-renderUI({
    shiny::selectInput(inputId = ns("SELECT1"), label = "SERVEUR CREATED UI 2", 
                       choices = seq(input$NEW_SELECT-10, input$NEW_SELECT+10, 1), selected = NULL)
  })
    
  
  
  BIGLIST1<-reactive({
    if ( is.null(input$LIST_SOURCE_BIG_DF)) return(NULL)
    inFile <- input$LIST_SOURCE_BIG_DF
    file <- inFile$datapath
    # load the file into new environment and get it from there
    e = new.env()
    name <- load(file, envir = e)
    data <- e[[name]]
    #
    temp.lenght<-length(data)
    # b1<-names(temp.lenght)[1]
    # b.max<-names(temp.lenght)[temp.lenght]
    #print(data)
    return(data)
  })
  
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
  })
  
  
  
  observe({updateSelectInput(session = session, inputId = "MINTIMEBIG", 
                             choices = names(BIGLIST1() ) , 
                             selected= names(BIGLIST1() )[ 1]
  )
    
  })#"names(BIGLIST1() ), selected =  names(BIGLIST1() )) })  
  observe({updateSelectInput(session = session, inputId = "MAXTIMEBIG", 
                             choices = names(BIGLIST1() ) , 
                             selected= names(BIGLIST1() )[length(  names(BIGLIST1() )  )]
  )
  })#"names(BIGLIST1() ), selected =  names(BIGLIST1() )) })
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
                       choices = names.pick, multiple = FALSE)
  })
  #### SELECT VAR et MODLITE ####
  
  
  output$UI_INDVAR_CHOOSE<-renderUI({
    ns <- session$ns
    if(req(input$DataType)!="fichier"){
      if(req(input$DataType)=="objet"){
        lapply(BIGLIST1(), function(bi){
          names(bi)
        })->lina
        unique(unlist(lina))->glona
        message<-h5("Renseignez la variable dans les données qui identifie des individus uniques et qui sera utilisée comme paramètre 'id' de seqdata()")
        
      } else {
        if(input$DataType=="objseq"){
          names( OBJSEQ_COMPLEMENT() )->glona
          message<-h5("Renseignez la variable dans les données complémentaires qui correspond à l'attribut row.names de l'objet seqdata")
        }
      }
      list(
        message,
        selectInput(inputId = ns("INDVAR"), label = "Variable pour identifiant individuel: ", 
                    choices = glona, multiple = FALSE)
      )
    }
  })
  
  INDVAR<-reactive({input$INDVAR})
  
  #### DUPLI / NA
  
  control_dupli_na<-reactive({
    req(input$INDVAR)
    req(BIGLIST1())
    if(req(input$DataType)!="fichier"){
      if(input$DataType=="objet"){
        lapply(1:length(BIGLIST1()), function(i){
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
    } else {"ok"}
  })
  
  output$MSSG_DUPLI<-renderUI({
    ns <- session$ns
    req(control_dupli_na())
    if(class(control_dupli_na())=="data.frame"){
      h3("Attention: la variable d'identifiant contient des foublons et/ou des NA.")
    } else {h3("OK : pas de doublons ou NA dans la varible d'identifiant.")}
  })
  
  output$DATA_DUPLI<-renderDataTable({
    req(control_dupli_na())
    if(class(control_dupli_na())=="data.frame"){
      control_dupli_na()
    }
  })
  
  output$DELETE_DUPLI_NA<-renderUI({
    ns <- session$ns
    req(control_dupli_na())
    if(class(control_dupli_na())=="data.frame"){
      shiny::checkboxInput(inputId = ns("deleteNA_DUPLI"), label = "Faut-il supprimer les doublons et les NA ? (sinon, choisir une autre variable)", value = TRUE)
    }
  })
  
  
  BIGLIST<-reactive({
    req( control_dupli_na() )
    req(input$INDVAR)
    if(input$DataType=="objet"){
      if(class(control_dupli_na())=="data.frame"){
        req(input$deleteNA_DUPLI)
        if(input$deleteNA_DUPLI){
          lapply(BIGLIST1(), FUN = function(bi){
            bi1<-subset(bi, !is.na(bi[ , input$INDVAR]))
            bi2<-subset(bi1, !duplicated( bi1[ , input$INDVAR] ) )
            bi2
          })
        }
      } else {
        BIGLIST1()
      }
    }
  })
  
  ####
  
  reactive({
    req(input$DATE_FOR_SELECT)
    BIGLIST1()[[input$DATE_FOR_SELECT]]->tempdf
    print("COUCOU 456")
    print("req(input$addvar) : ")
    print(input$addvar)
    print("end")
  
    if(input$addvar==TRUE){
      
      req(input$MERGEORIGINVAR, input$MERGEADDVAR)
      if((input$MERGEORIGINVAR!=""&input$MERGEADDVAR!="")|(!is.null(input$MERGEORIGINVAR)&!is.null(input$MERGEADDVAR))){
        print("COUCOU 460")
        
        print(input$MERGEORIGINVAR)
        print(input$MERGEADDVAR)
        req(ADDDATA())
        left_join(tempdf, ADDDATA(), by=c(input$MERGEORIGINVAR, input$MERGEADDVAR))->tempdf2
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
    
    #print("tempdf2")
    #print(tempdf2)
    print("COUCOU 485")
    return(tempdf2)
    
  })->the.df
  
  
  
  output$ADDDATA_CSV<-renderUI({
    ns <- session$ns
    print(input$addvar)
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
  })
  
  reactive({
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
  })->ADDDATA
  
  observe({
    updateSelectInput(session=session, inputId = "MERGEADDVAR", choices = names(ADDDATA()), selected = NULL)
  })
  
  observe({
    req(input$DATE_FOR_SELECT)
    BIGLIST1()[[input$DATE_FOR_SELECT]]->tempdf
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
    selectInput(inputId = ns("VAR_FOR_SELECT"), label = "Variable pour sélection", 
                choices = names(the.df()), multiple = FALSE)
  })
  
  THE_VAR<-reactive({
    #req( the.df() )
    req( input$VAR_FOR_SELECT)
    the.df()[ , input$VAR_FOR_SELECT]->the.var
    the.var
  })
  
  output$UI_CLASS_SELECT<-renderUI({
    ns <- session$ns
    req(THE_VAR() )
    class(THE_VAR())->CLASS_VAR
    shiny::selectInput(inputId = ns("classSelect"), label = "Contrôle de la classe", 
                       choices =  c("factor", "character", "numeric", "Date", "integer"), 
                       selected = CLASS_VAR)
  })
  
  output$UI_MOD_SELECT<-renderUI({
    ns <- session$ns
    req(THE_VAR() )
    req(input$classSelect)
    
    
    if(input$classSelect%in%c("numeric", "integer")){
      
      as.numeric(THE_VAR())->temp.num.var
      
      minis <- min(temp.num.var, 
                   na.rm = TRUE)
      maxis <- max(temp.num.var, 
                   na.rm = TRUE)
      
      
      shiny::sliderInput(inputId = ns("NumSelect"), label = "Valeurs sélectionnées", 
                         min = minis, max = maxis, 
                         value = c(minis, maxis))
    } else {
      if(input$classSelect=="factor"){#, "character") ){
        if(length(unique(THE_VAR()))>100){
          table(THE_VAR())->tab
          tab[order(tab, decreasing = TRUE)]->tab
          tab[1:25]->tab
        } else {tab<-unique(THE_VAR() )}
        shiny::selectInput(inputId = ns("FactSelect"), label="Valeurs sélectionnées", 
                           choices = tab  , multiple = TRUE)
      } else {
        if(input$classSelect=="character"){#, "character") ){
          shiny::textInput(inputId = ns("CharPatSelect"), label="'Paterns' à rechercher (sep by '/'", value = ""
          )
        } else  {
          if(input$classSelect=="Date" ){
            
            renderText(print( unique(THE_VAR() )[!is.na(unique(THE_VAR()) )][1]  ) )->output$AFFICHDATE
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
    print(THE_VAR())
    print(as.character(input$DATEformat))
    if(class(THE_VAR())=="Date"){
      THE_VAR()->THE_VAR_DATE
    } else {
      if(class(THE_VAR())=="character"){
        as.Date(THE_VAR(), format=as.character(input$DATEformat) )->THE_VAR_DATE
      } else {
        if(class(THE_VAR())=="numeric"|class(THE_VAR())=="integer"){
          as.POSIXct(THE_VAR(),origin="1970-01-01")->THE_VAR_DATE
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
    THE_VAR()[!is.na(THE_VAR())]->the.var2
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
    
    data.frame("PAQUET"=isolate(input$PAQUET_FOR_SELECT),
               "DATE"=isolate(input$DATE_FOR_SELECT),
               "VARIABLE"=isolate(input$VAR_FOR_SELECT), 
               "TYPE"=isolate(input$classSelect),
               "TEXT_PATTERN"=text_pattern,
               "FACTOR_LEVELS"=factor_levels,
               "min"=minus,
               "max"=maxus,
               "DATE_min"=datmin,
               "DATE_max"=datmax
               
    )->vecto 
    print("vecto")
    print(vecto)
    
    isolate(values$DF_subset_initial <- rbind(values$DF_subset_initial , vecto))
    
    #  newLine <- isolate(c(input$text1, input$text2))
    #  isolate(values$df <- rbind(values$df, newLine))
  })
  
  
  output$TABLE_POUR_SELECTION<-DT::renderDataTable(
    values$DF_subset_initial,
    #    server = FALSE,
    rownames = FALSE,
    filter = "none",
    editable = list(target = "row"
                    #disable = list(columns = c(1, 2))
    )
  )
  
  shiny::reactive({
    req(  values$DF_subset_initial )
    data_of_subset <-  values$DF_subset_initial
    
    string_for_sub<-sapply(1:nrow(data_of_subset), function(i){
      paste("BIGLIST()$", data_of_subset$DATE[i], sep="" )->dfvar
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
                  paste(dfvar, "$", data_of_subset$VARIABLE[i], ">=", data_of_subset$min[i]),
                  "&",
                  paste(dfvar, "$", data_of_subset$VARIABLE[i], "<=", data_of_subset$max[i]),
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
        lapply(BIGLIST(), function(bil){
          bil[ , INDVAR()]
        })->list.ind.no.subset
        unique(unlist(list.ind.no.subset))
        
      } else {
        message("COUCOU 797")
        
      #subset(STRING_FOR_SUB(), STRING_FOR_SUB()$string_for_sub[1]!="nosub")->dfsub
      list.of.inf.by.cond<-lapply(1:nrow(STRING_FOR_SUB() ), function(i){
        if(STRING_FOR_SUB()$string_for_sub[i]=="nosub"){
          BIGLIST()[[STRING_FOR_SUB()$DATE[i]]][ , INDVAR()]
        } else {
        subset(BIGLIST()[[STRING_FOR_SUB()$DATE[i]]], 
               eval(parse(text = as.character(STRING_FOR_SUB()$string_for_sub[i]) )) )[ , INDVAR()]
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
      lapply(BIGLIST(), function(bil){
        bil[ , INDVAR()]
      })->list.ind.no.subset
      unique(unlist(list.ind.no.subset))
    }
  })
  
  reactive({
    req(BIGLIST() )
    if(input$addCONDS==TRUE){
      req(INDIVIDUELS() )
      lapply(BIGLIST(), FUN = function(bi){
        subset(bi, bi[ , INDVAR()]%in%INDIVIDUELS())
      })
    } else {
      BIGLIST()
    }
  })->SUBSETTED_LIST
  
  output$LENGTH_IND_SUBS<-renderText({ length( INDIVIDUELS() )    }) 
  output$LENGTH_BIGLIST<-renderText({ length( BIGLIST() )    }) 
  output$LENGTH_SUBSETTED<-renderText({ length( SUBSETTED_LIST() )    }) 
  output$LENGTH_BIGLIST1<-renderText({ length( BIGLIST1() )    }) 
  
  
  
  observe({
    SUBSETTED_LIST()->data.to.save 
    output$downlist <- shiny::downloadHandler(filename = "mes_datas.RData", 
                                              content =  function(file) {
                                                save(data.to.save, file = file)
                                              } )
  })
  
  
  #### Chargement des données ####
  
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
  #### Chargement premier fichier de données ####
  renderPrint(print(length(SUBSETTED_LIST())))->output$CONTROLDATA
  #### Un fichier source ####
  data<-reactive({
    req(trajs$dataSource)
    if(input$DataType=="fichier"){ #### Chargement d'un seul fichier CSV ####
      
      if (input$rowname==TRUE && input$rownames_par=="")
      {

        colonneID<- c(colnames(trajs$df)[colId(df = trajs$df)])
        updateSelectInput(session = session, inputId = "rownames_par",choices = c("",colonneID))
      }
      if (input$rowname==TRUE && input$rownames_par!=""){
        userData <- read.csv(file = input$file1$datapath, 
                             sep = input$sepcol, 
                             encoding = input$endoding,
                             #row.names = input$rownames_par,
                             header=input$header,na.strings = argna(),
                             dec=input$dec)
        row.names(userData)<-userData[ , input$rownames_par]
        mycolumns<-c(colnames(userData))
        updateSelectInput(session = session, inputId = "timecol", choices = mycolumns)
        trajs$df<- userData
        
      }
      
      if(input$rowname==FALSE){
        userData <- read.csv(file = input$file1$datapath, sep = input$sepcol, encoding = input$endoding,header=input$header,na.strings = argna(),dec=input$dec)
        mycolumns <- c(colnames(userData))
        updateSelectInput(session = session, inputId = "timecol", choices = mycolumns)
        trajs$df<- userData}
      return(trajs$df)
    } else {
      
    }
  })
  output$contenu<-shiny::renderDataTable({
    req(data())
    data()
  })
  
  #### Paramétrage des trajectoires ####
  ####  DATE DEBUT ET FIN POUR TRAJ ####
  output$DATA_UI<-shiny::renderUI({
    ns <- session$ns
    if(input$DataType=="fichier"){
      shiny::dateRangeInput(inputId = ns("date.range"), label = "Dates de début et de fin",
                            format = "mm-yyyy")->the.ui
    } else {
      if(input$DataType=="objet"){
        names(SUBSETTED_LIST())->names.pick
        mycolumns<-unique(unlist(Reduce(intersect,list(lapply(X = SUBSETTED_LIST(), FUN = names))),
                                 use.names = FALSE))
        updateSelectInput(session = session, inputId = "timecol", choices = mycolumns)
        list(
          shiny::selectInput(inputId = ns("PICKDATE1"), label = "Debut:",
                             choices = names.pick, multiple = FALSE),
          shiny::selectInput(inputId = ns("PICKDATE1"), label = "Fin:",
                             choices = names.pick, multiple = FALSE))->the.ui
      } else {h3("error")->the.ui}
    }
    the.ui
  })
  ####  data.seq ####
  
  DR_POUR_SEQ_OBJ<-reactive({# eventExpr = input$ValidParametres,  {
    if(input$DataType=="objet"){
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
      
      
      
      names(df_RSA.sampled)<-c(input$INDVAR, paste(1:(ncol(df_RSA.sampled)-1), "_VAR"))
      
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
    }
    
  })
  
  
  
  data.seq<-eventReactive(eventExpr = input$ValidParametres, {
    #### SI FICHOER ####
    if(input$DataType=="fichier"){
      req(data())
      data()->df.pour.seq
      
      if (length(input$timecol)<2){
        showModal(modalDialog(
          title = "Important message",
          "Il faut mettre au moins deux variables temporelles.",
          easyClose = TRUE
        ))
      }
      else if ((length(input$timecol)-1)<input$PAStrate){
        showModal(modalDialog(
          title = "Important message",
          "Il faut que le 'Pas de temps pour le calcul des taux de transition' dans l'onglet Statistiques descriptives/taux de transition (et taux de sortie) soit inférieur strictement au nombre de variables temporelles.",
          easyClose = TRUE
        ))
      }
      else   {
        # updateNumericInput(session=session, inputId = "PAStrate",value=1)
        s<-seqdef(df.pour.seq[,input$timecol],cpal = NULL,
                  gaps = input$TEXT_GAP,
                  right = input$TEXT_RIGHT,
                  left = input$TEXT_LEFT,nr = "RMA")
        
      }
      
      return(s)
    } else {
      #### SI OBJET ####
      
      if(input$DataType=="objet"){
        
        DR_POUR_SEQ_OBJ()->df.pour.seq
        
        print(DR_POUR_SEQ_OBJ()[ , input$INDVAR][duplicated(DR_POUR_SEQ_OBJ()[ , input$INDVAR])])
        
        if(!is.null(DR_POUR_SEQ_OBJ())){
          seqdef(id = DR_POUR_SEQ_OBJ()[ , input$INDVAR], 
                 data = DR_POUR_SEQ_OBJ()[ , grepl("_VAR", x = names(DR_POUR_SEQ_OBJ()))&names(DR_POUR_SEQ_OBJ())!=input$INDVAR],
                 gaps = input$TEXT_GAP,
                 right = input$TEXT_RIGHT,
                 left = input$TEXT_LEFT,nr = "RMA"
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
  
  output$DES_TRAJ_OBJ<-renderPrint({
    dim( data.seq() )
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
  
  
  observe({
    list("SEQ"=data.seq(), "DATA"=DR_POUR_SEQ_OBJ())->list.to.save
    output$downseq <- shiny::downloadHandler(filename = "mes_trajectoires.RData", 
                                             content =  function(file) {
                                               save(list.to.save, file = file)
                                             } )
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
  
  return(reactive({
    list("SEQ_OBJ"=data.seq(), 
         "DATA_COMP"=data2(), 
         "TYPE_SOURCE"=input$DataType, 
         "CODAGE_MANQUANT"=list("GAP"=input$TEXT_GAP, "RIGHT"=input$TEXT_RIGHT, "LEFT"=input$TEXT_LEFT) )
  }))
}