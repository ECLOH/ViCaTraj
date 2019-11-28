#### UI ####
fluidRow(#useShinyjs(),
  #column(2,
  hr(),
  shiny::actionButton(inputId = "COMPUTE_GRAPH", label = "Actualiser le graphique"),
  hr(),
  shiny::selectInput(inputId = "plottype", label = "Quel graphique voulez-vous représenter? ", 
                     choices = c("Chronogramme [seqplot(type = 'd')] "="d", "Séquences les plus fréquentes [seqplot(type = 'd')] "="f", 
                                 "Tapis [seqplot(type = 'd')] "="I", "Etat modal [seqplot(type = 'd')] "="ms", 
                                 "Durée moyenne dans chaque état [seqplot(type = 'd')] "="mt",
                                 "Graphique d'entropie [seqplot(type = 'd')] "="Ht", 
                                 "Séquences représentatives  [seqplot(type = 'd')] "="r",
                                 "Graphique de flux"="flux",
                                 "Sous-séquences triées selon leur support  [seqefsub()] "="sous.seq",
                                 "Sous-séquences choisies  [seqefsub()] "="sous.seq.ch"), selected = "d", multiple = FALSE),
  conditionalPanel(condition="input.plottype=='sous.seq.ch'",
                   wellPanel(shiny::selectInput(inputId = "par.sous.seq1",label = "Etat 1",choices = "",multiple = FALSE),
                             shiny::selectInput(inputId = "par.sous.seq2",label = "Etat 2",choices = "",multiple = FALSE),
                             shiny::selectInput(inputId = "par.sous.seq3",label = "Etat 3",choices = "",multiple = FALSE),
                             shiny::numericInput(inputId = "ligne.suppr", label = "Ligne à supprimer", min = 1, max = 100, value = 1),
                             shiny::actionButton(inputId = "add.button", label = "Ajouter", icon = icon("plus")),
                             br(),br(),
                             shiny::actionButton(inputId = "delete.button", label = "Supprimer", icon = icon("minus"))
                   )
  ),
  conditionalPanel(condition="input.plottype=='I' ",
                   shiny::radioButtons(inputId = "TapisSorted",
                                       label="Trier selon : ",
                                       choices = c("Le début"="from.start","La fin"="from.end"),selected = "from.start")),
  #### SELECTION ####
  
  tabPanel("Selections",
           hr(),
           #### SUB-PANEL: PARAMETRAGE ####
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
  
  
  
  
  
  
  
  #shiny::selectInput(inputId = "souspop1", label = "Sous-Population", choices = "Aucune", selected = "Aucune", multiple = FALSE),
  #shiny::uiOutput(outputId= "slider1"),
  #shiny::uiOutput(outputId= "modalite1"),
  
  #### FIN SELECTION ####
  conditionalPanel(condition="input.plottype=='flux'",
                   shiny::selectInput(inputId = "timeseq1", label = "Dates retenues", 
                                      choices = "", selected = "", multiple = TRUE, selectize = TRUE)#
                   
                   #shiny::actionButton(inputId = "graph1", label = "Afficher le graphique")
  ),
  # conditionalPanel(condition="input.plottype=='sous.seq.ch'",
  #                  shiny::actionButton(inputId = "graphSousSeq", label = "Afficher le graphique")),
  conditionalPanel(condition="input.plottype=='sous.seq'",
                   shiny::sliderInput(inputId = "pmin1", label = "Support minimal",min=0,max=1,value=0.15,step = 0.01)
                   
  ),
  #),
  column(10,align="center",
         tags$p("Pour visualiser les mêmes graphiques pour des groupes, allez dans", tags$em("Classification des trajectoires"), "puis faire la" , tags$em("Matrice de distance"), "et la",tags$em("Classification."),"Les graphiques seront dans l'onglet",tags$em("Visualisation des groupes.")),
         uiOutput("txtAjoutSeq"),
         uiOutput("h4_fluxGlobal"),
         uiOutput("PLOT_DES")%>% withSpinner(color="#0dc5c1"),
         uiOutput("TAB_DES")%>% withSpinner(color="#0dc5c1"),
         
         #uiOutput("PLOT3")%>% withSpinner(color="#0dc5c1"),
         downloadButton(outputId="DownGraphGlobal",label="Télécharger le graphique"),
         hidden(p(id="TexteDownloadGraph","Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (.png). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application.")),
         textOutput("TexteGraph")
         
         #,uiOutput("subsTable")
         
         
  )
)
##### SERVER ####
if(class(obj.seq)=="data.frame"){
  obj.seq<-list("Datas"=obj.seq)
}
#### CODE ####

#### ETAPE 1 : SELECTION ####
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
})


output$TABLE_POUR_SELECTION<-DT::renderDataTable(
  values$DF_subset_initial,
  rownames = FALSE,
  filter = "none",
  editable = list(target = "row"
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

#### ETAPE 2 ####






####### Type de graph ####
#mise a jour des input et création de nouveaux input selon le type de sous-population choisi
observe({#eventExpr = input$ValidParametres,{
  # req(DATAs())
  updateSelectInput(session = session, inputId = "timeseq1", choices = names(obj.seq))
  
  colsouspop<-colnames(data.comp)[!(colnames(data.comp) %in% input$timecol)]
  message("COUCOU 184")
  print(colsouspop)
  updateSelectInput(session = session, inputId = "souspop1", choices = c("Aucune",colsouspop))
  
})

observeEvent(input$souspop1,{
  req(input$souspop1)
  if (input$souspop1=="Aucune"){
    updateSelectInput(session = session, inputId = "souspop_modalite1", choices = "" )
  }
})

output$slider1<- renderUI({
  if(input$souspop1!="Aucune"){
    if (is.numeric(data.comp[,input$souspop1])){
      min<-min(data.comp[,input$souspop1],na.rm = TRUE)
      max<-max(data.comp[,input$souspop1],na.rm = TRUE)
      sliderInput(inputId = "sous_pop_num1", label="Slider",min=min,max=max,value = c(min,max))
    }
  }
})
output$modalite1<- renderUI({
  if(input$souspop1!="Aucune"){
    if (is.factor(data.comp[,input$souspop1])){
      selectInput(inputId = "souspop_modalite1",label="Modalité(s)", choices = levels(data.comp[,input$souspop1]),selected="",multiple = TRUE)
    } else {if(is.character(data.comp[,input$souspop1])){
      if(length(unique(data.comp[,input$souspop1]))<25){
        selectInput(inputId = "souspop_modalite1",label="Modalité(s)", choices = unique(data.comp[,input$souspop1]),selected="",multiple = TRUE)
        
      }
    }}
  }
})



####### Selection des sous populations ####
data.select1<-reactive({
  req(input$souspop1)
  print("coucou!! 1110")
  
  
  if (input$souspop1=="Aucune" || input$souspop1==""){
    data.select<-data.comp
  }else{
    
    if (is.factor(data.comp[,input$souspop1])|(is.character(data.comp[,input$souspop1])&length(unique(data.comp[,input$souspop1]))<25)){
      if(length(input$souspop_modalite1)>0){
        data.select<-data.comp[(data.comp[,input$souspop1] %in% c(input$souspop_modalite1)),]
      }else{
        data.select<-NULL
      }
    }else{
      if (is.numeric(data.comp[,input$souspop1])){
        req(input$sous_pop_num1)
        data.select<-data.comp[which(data.comp[,input$souspop1]<= max(input$sous_pop_num1,na.rm=TRUE) & data.comp[,input$souspop1]>= min(input$sous_pop_num1,na.rm=TRUE)),]
        
      }
    }
  }
  return(data.select)
  
})
#### SEQ.SELECT1 ####
seq.select1<-reactive({
  req(input$souspop1)
  print("coucou!! 1135")
  
  if (input$souspop1=="Aucune" || input$souspop1==""){
    print("coucou!! 1140")
    
    seq.select<-obj.seq
  }else{
    if (is.factor(data.comp[,input$souspop1])|is.character(data.comp[,input$souspop1])) {
      print("coucou!! 1145")
      
      if(length(input$souspop_modalite1)<1){
        seq.select<-lapply(X = input$souspop_modalite1, FUN = function(levels.i){
          obj.seq[(data.comp[,input$souspop1] == levels.i ),]
        })
        names(seq.select)<-input$souspop_modalite1
      } else {
        seq.select<-obj.seq[(data.comp[,input$souspop1] %in% c(input$souspop_modalite1)),]
        #seq.select<-obj.seq[(data.comp[,input$souspop1] %in% c(input$souspop_modalite1)),]
      }
    } else {
      
      if (is.numeric(data.comp[,input$souspop1])){
        req(input$sous_pop_num1)
        seq.select<-obj.seq[which(data.comp[,input$souspop1]<= max(input$sous_pop_num1,na.rm=TRUE) & data.comp[,input$souspop1]>= min(input$sous_pop_num1,na.rm=TRUE)),]
      }
      
    }
  }
  print(class(seq.select))
  return(seq.select)
  
})




####### Pas de temps voulue pour les graphiques de flux avec au minimun deux pas de temps ###
col_periode1<-eventReactive(eventExpr = length(input$timeseq1)>=2,{
  input$timeseq1
})

####### Création d'une liste des graphiques de flux pour pouvoir les tracer côte à côte ####
flux1<-eventReactive(eventExpr = input$graph1,{
  req(data.select1(),col_periode1(),seq.select1())
  if (input$souspop1!="Aucune"){#} && is.factor(data.comp[,input$souspop1])) {
    lapply(1:length(input$souspop_modalite1), FUN=function(i){
      print(class(data.select1()))
      print(dim(data.select1()))
      print(class(seq.select1()))
      print()
      graph_flux_grp(data=data.select1(),
                     seq_data=seq.select1(),
                     col_periode=col_periode1(),
                     var_grp=input$souspop1,
                     label_grp= as.character(input$souspop_modalite1[i]))
    })
  }
  else{
    print("coucou!! l1183")
    return(list(graph_flux(data=data.select1(),seq_data=seq.select1(),col_periode=col_periode1())))
  }
  
})

####### Graphique sous séquence ####
####### Création des graphiques pour les deux types de sous-séquences ###

sousSeqPlot<-reactive({
  req(seq.select1())
  if (req(input$plottype) == "sous.seq"){
    #Pour la comparaison des sous-populations, on met les graphiques dans une liste#
    if (input$souspop1!="Aucune" && is.factor(data.comp[,input$souspop1])) {
      if(length(input$souspop_modalite1)>0 && input$souspop_modalite1 %in% levels(data.comp[,input$souspop1]) ){
        lapply(1:length(input$souspop_modalite1), FUN=function(i){
          seq.select1()[data.select1()[,input$souspop1]==input$souspop_modalite1[i],]->seqSouspop
          seqecreate(seqSouspop, tevent="state", use.labels=FALSE)->seqGlobal
          titre<-paste("Graphique des sous-séquneces \n pour la variable",input$souspop1,"\n avec la modalité",input$souspop_modalite1[i])
          sousTitre<-paste("Il y a",nrow(seqSouspop),"individus")
          seqefsub(seqGlobal,pmin.support=input$pmin1)->datas
          p<-graph_sous_sequences(datas)+ggtitle(titre,subtitle = sousTitre)
          
          res<-list("p"=p, "data"=datas)
          
          return(res)
        })
      }   
    } else {
      seqecreate(seq.select1(), tevent="state", use.labels=FALSE)->seqGlobal
      seqefsub(seqGlobal,pmin.support=input$pmin1)->datas
      p<-graph_sous_sequences(datas)
      
      res<-list("p"=p, "data"=datas)
      
      return(res)
      #return(list(graph_sous_sequences(seqefsub(seqGlobal,pmin.support=input$pmin1))))
    }
  }else{
    ## Cas où l'utilisateur choisi les sous-séquences ##
    if (req(input$plottype) == "sous.seq.ch"){
      req(values$df)
      #condition d'un data.frame values non vide pour exécuter la suite du code afin de ne pas avoir d'erreur quand la data.frame est vide
      if(nrow(values$df)>0){
        if (input$souspop1!="Aucune" && is.factor(data.comp[,input$souspop1])) {
          if(length(input$souspop_modalite1)>0 && input$souspop_modalite1 %in% levels(data.comp[,input$souspop1]) ){
            lapply(1:length(input$souspop_modalite1), FUN=function(i){
              
              seq.select1()[data.select1()[,input$souspop1]==input$souspop_modalite1[i],]->seqSouspop
              seqecreate(seqSouspop, tevent="state", use.labels=FALSE)->seqGlobal
              vectSeq<-vect.sous.seq(data = values$df)
              seqefsub(seqGlobal,str.subseq=vectSeq)->datas
              titre<-paste("Graphique des sous-séquneces \n pour la variable",input$souspop1,"\n avec la modalité",input$souspop_modalite1[i])
              sousTitre<-paste("Il y a",nrow(seqSouspop),"individus")
              
              p<-graph_sous_sequences(datas[order(datas$data$Support,decreasing = TRUE),])+
                ggtitle(titre,subtitle = sousTitre)
              
              res<-list("p"=p, "data"=datas)
              
              return(res)
              
              
              #return(graph_sous_sequences(p[order(p$data$Support,decreasing = TRUE),])+ggtitle(titre,subtitle = sousTitre))
              
            })
          }   
        } else {
          
          seqecreate(seq.select1(), tevent="state", use.labels=FALSE)->seqGlobal
          vectSeq<-vect.sous.seq(data = values$df)
          seqefsub(seqGlobal,str.subseq=vectSeq)->datas
          p<-graph_sous_sequences(datas[order(datas$data$Support,decreasing = TRUE),])
          
          res<-list("p"=p, "data"=datas)
          
          return(res)
          
        }
      }
    }
  }
})

output$txtAjoutSeq<-renderUI({
  if (req(input$plottype) == "sous.seq.ch"){
    if(!(nrow(values$df)>0)){
      output$txtAjout<-renderText({
        return("Ajouter une séquence en choissant une succession d'état et en appuyant sur ajouter")
      })
      return(textOutput("txtAjout"))
    }
  }
})

##### Graphique sous-séquences choisies #####
#######  Mise a jour des inputs permettant de choisir des états ###
observe({
  input$plottype
  isolate({
    if (req(input$plottype)=="sous.seq.ch"){
      req(seq.select1())
      updateSelectInput(session = session,inputId = "par.sous.seq1",choices = alphabet(seq.select1()))
      updateSelectInput(session = session,inputId = "par.sous.seq2",choices = alphabet(seq.select1()))
      updateSelectInput(session = session,inputId = "par.sous.seq3",choices = cbind("Aucun",alphabet(seq.select1())))
    }
  })
})

observe({
  updateNumericInput(session = session,inputId = "ligne.suppr",max=nrow(values$df))
})

values <- reactiveValues()
values$df <-  as.data.frame(setNames(replicate(3,character(0), simplify = F),c("Etat1","Etat2","Etat3") ))

####### Mise en action des boutons ajout et suppression ###
observeEvent(input$add.button,{
  req(input$par.sous.seq1,input$par.sous.seq2)
  newRow <- data.frame(input$par.sous.seq1, input$par.sous.seq2,input$par.sous.seq3)
  colnames(newRow)<-colnames(values$df)
  values$df <- rbind(values$df,newRow)
  rownames(values$df)<-(1:nrow(values$df))
})

observeEvent(input$delete.button,{
  if(nrow(values$df)>1){
    values$df[!(vect.sous.seq(values$df) %in% as.character(subsGlobal()$subseq)[input$ligne.suppr]),]->values$df
    rownames(values$df)<-(1:nrow(values$df))
  }else {
    values$df <- values$df[-nrow(values$df), ]
  }
})  
####### Ne grader que des sous-séquences uniques ###
observe({
  req(values$df)
  values$df<-unique(values$df)
})

####### Utilisé pour pouvoir supprimer des sous-sequences ###   
subsGlobal<-reactive({
  if (req(input$plottype) == "sous.seq.ch"){
    req(seq.select1(),values$df)
    if(nrow(values$df)>0){
      vectSeq1<-vect.sous.seq(data = values$df)
      seqefsub(seqecreate(seq.select1(), tevent="state", use.labels=FALSE),str.subseq=vectSeq1)->p1
      return(p1[order(p1$data$Support,decreasing = TRUE),])
    }
    
  }
})
#### Graphiques ####
####### Création d'un ordre de disposition des graphiques selon le nombre de graphiques à afficher ###
ordre1<-reactive({
  #On separe le cas des graphiques de flux car la mise à jour ne doit se faire que lors que l'utilisateur appuie sur le bouton
  input$graph1
  isolate({
    if(input$plottype=="flux"){
      if (input$souspop1!="Aucune" && is.factor(data.comp[,input$souspop1])) {
        if (length(input$souspop_modalite1)>1){
          return(taille_graph_flux(length(input$souspop_modalite1)))
        }else{
          if (length(input$souspop_modalite1)==1){
            return(cbind(1))
          }else{
            return(NULL)
          }  
        }
        
      }else {
        #Cas où on affiche qu'un seul graphique à l'écran
        return(cbind(1))
      }
    } 
  })
  
  if(req(input$plottype) %in% c("d", "f", "I", "ms", "mt", "r","Ht","sous.seq","sous.seq.ch")) {
    if (input$souspop1!="Aucune" && is.factor(data.comp[,input$souspop1])) {
      if (length(input$souspop_modalite1)>1){
        return(taille_graph_flux(length(input$souspop_modalite1)))
      }else{
        if (length(input$souspop_modalite1)==1){
          return(cbind(1))
        }else{
          return(NULL)
        }
      }     
    }else {
      return(cbind(1))
    }
  }
})
####### Rend automatique la hauteur des graphiques pour qu'ils soient lisisbles ###
haut1<-function(){
  req(ordre1())
  ordre3<-ordre1()
  if (req(input$plottype) %in% c("sous.seq","sous.seq.ch")) {
    return(dim(ordre3)[1]*1000)
  }else{
    return(dim(ordre3)[1]*400)
  }
}
tailleGraph<-reactiveValues(height=400)
####### Rend automatique la largeur des graphiques pour qu'ils soient lisisbles ###
large<-function(){
  if (input$souspop1!="Aucune" && is.factor(data.comp[,input$souspop1])){
    req(input$souspop_modalite1)
    if(length(input$souspop_modalite1)>1){
      return(1300)
    }
    if(length(input$souspop_modalite1)==1){
      return(650)
    }
  }else{
    return(650)
  }
}
####### Graphiques des statistiques descriptives/ visualisation des trajectoires ###

observeEvent(input$COMPUTE_GRAPH, {
  ###### PLOT_AND_TAB sous.seq
  if (req(input$plottype) %in% c("sous.seq","sous.seq.ch")){
    
    # output$PLOT_AND_TAB<-renderUI({
    
    output$PLOT_DES<-renderUI({
      
      output$PLOT<-renderPlot({
        if (req(input$plottype) %in% c("sous.seq","sous.seq.ch")){
          req(sousSeqPlot(),ordre1())
          tailleGraph$height<-dim(ordre1())[1]*1000
          return(marrangeGrob(sousSeqPlot()$p, layout_matrix=ordre1()))
        }
      },width = large(),height = haut1())
      
      plotOutput("PLOT",height = tailleGraph$height)#->PLOT
      
    })
    
    output$TAB_DES<-renderUI({
      output$TAB<-renderDataTable({
        sousSeqPlot()$data
      })
      
      shiny::dataTableOutput("TAB")
    })
    
    
    #return(list(PLOT, TAB))
    #})
    
  } else {
    ###### PLOT_AND_TAB flux
    
    if (req(input$plottype) == "flux"){
      print("COUCOU ligne 1441")
      input$graph1
      isolate({
        output$PLOT<-renderPlot({
          if (req(input$plottype) == "flux"){
            req(flux1(),ordre1())
            tailleGraph$height<-dim(ordre1())[1]*400
            return(marrangeGrob(flux1(), layout_matrix=ordre1()))
          }
        },width = large(),height = haut1())
        return(plotOutput("PLOT",height = tailleGraph$height))
      })
    } else {
      
      ###### PLOT_AND_TAB autres
      
      
      if (req(input$plottype) %in% c("d", "f", "I", "ms", "mt", "r","Ht") &req(input$plottype)!="flux") {
        print("COUCOU NON 1463")
        #if (req(input$plottype) %in% c("d", "f", "I", "ms", "mt", "r","Ht")) {
        req(seq.select1(),data.select1(),ordre1())
        tailleGraph$height<-dim(ordre1())[1]*400
        if (input$souspop1!="Aucune" && is.factor(data.comp[,input$souspop1])){
          if(length(input$souspop_modalite1)>0 && input$souspop_modalite1 %in% levels(data.comp[,input$souspop1]) ){
            req(seq.select1(),data.select1(),ordre1())
            if (req(input$plottype) == "I") {
              
              
              output$PLOT_DES<-renderUI({
                
                seqplot(seqdata = seq.select1(), type = input$plottype, group = data.select1()[,input$souspop1],sortv = input$TapisSorted,yaxis=FALSE)->p
                output$PLOT<-renderPlot({p}, width = large(),height = haut1())
                plotOutput("PLOT",height = tailleGraph$height)#->PLOT
              })
              
              
              output$TAB_DES<-renderUI({
                
                seqtab(seqdata = seq.select1() )->data
                
                output$TAB<-renderDataTable({
                  data
                })
                
                DT::dataTableOutput("TAB")#->TAB
                
              })
              
              #return(list(PLOT, TAB))
              
            } else{
              
              output$PLOT_DES<-renderUI({
                
                seqplot(seqdata = seq.select1(), type = input$plottype, group = data.select1()[,input$souspop1])->p
                output$PLOT<-renderPlot({p}, width = large(),height = haut1())
                plotOutput("PLOT",height = tailleGraph$height)#->PLOT
              })
              
              
              output$TAB_DES<-renderUI({
                
                
                DONNEES_POUR_PLOT(TYPE = as.character(input$plottype), objseq=seq.select1())->data
                
                output$TAB<-renderDataTable({
                  data
                })
                
                DT::dataTableOutput("TAB")
              })
              
              #return(list(PLOT, TAB))
              
              
            }
          }else{
            return(NULL)
          }
        } else{
          req(seq.select1())
          if (req(input$plottype) == "I") {
            #return(seqplot(seqdata = seq.select1(), type = input$plottype,sortv = input$TapisSorted,yaxis=FALSE))
            
            output$PLOT_DES<-renderUI({
              
              seqplot(seqdata = seq.select1(), type = input$plottype,sortv = input$TapisSorted,yaxis=FALSE)->p
              output$PLOT<-renderPlot({p}, width = large(),height = haut1())
              plotOutput("PLOT",height = tailleGraph$height)#->PLOT
            })
            
            
            output$TAB_DES<-renderUI({
              
              seqtab(seqdata = seq.select1() )->data
              
              output$TAB<-renderDataTable({
                data
              })
              
              DT::dataTableOutput("TAB")#->TAB
              
            })
            
            
          }else{
            #return(seqplot(seqdata = seq.select1(), type = input$plottype))
            
            output$PLOT_DES<-renderUI({
              
              #->p
              output$PLOT<-renderPlot(seqggplot(TYPE = input$plottype, objseq = seq.select1()), #seqplot(seqdata = seq.select1(), type = input$plottype), 
                                      width = large(),height = haut1())
              plotOutput("PLOT",height = tailleGraph$height)#->PLOT
            })
            
            
            output$TAB_DES<-renderUI({
              
              
              DONNEES_POUR_PLOT(TYPE =input$plottype, objseq =  seq.select1())->data
              
              output$TAB<-renderDataTable({
                data
              })
              
              DT::dataTableOutput("TAB")
            })
            
            
          }
        }
      }
      
      #}#,width = large(),height = haut1())
    }}
  #return(plotOutput("PLOT",height = tailleGraph$height))
})




# observeEvent(input$COMPUTE_GRAPH, {
# output$PLOT3<- renderUI({
#   if (req(input$plottype) %in% c("sous.seq","sous.seq.ch")){
#     output$PLOT<-renderPlot({
#       if (req(input$plottype) %in% c("sous.seq","sous.seq.ch")){
#         req(sousSeqPlot(),ordre1())
#         tailleGraph$height<-dim(ordre1())[1]*1000
#         return(marrangeGrob(sousSeqPlot(), layout_matrix=ordre1()))
#       }
#     },width = large(),height = haut1())
#     return(plotOutput("PLOT",height = tailleGraph$height))
#   }
#   if (req(input$plottype) == "flux"){
#     input$graph1
#     isolate({
#       output$PLOT<-renderPlot({
#         if (req(input$plottype) == "flux"){
#           req(flux1(),ordre1())
#           tailleGraph$height<-dim(ordre1())[1]*400
#           return(marrangeGrob(flux1(), layout_matrix=ordre1()))
#         }
#       },width = large(),height = haut1())
#       return(plotOutput("PLOT",height = tailleGraph$height))
#     })
#   }
#   if (req(input$plottype) %in% c("d", "f", "I", "ms", "mt", "r","Ht")) {
#     output$PLOT<-renderPlot({
#       if (req(input$plottype) %in% c("d", "f", "I", "ms", "mt", "r","Ht")) {
#         req(seq.select1(),data.select1(),ordre1())
#         tailleGraph$height<-dim(ordre1())[1]*400
#         if (input$souspop1!="Aucune" && is.factor(data.comp[,input$souspop1])){
#           if(length(input$souspop_modalite1)>0 && input$souspop_modalite1 %in% levels(data.comp[,input$souspop1]) ){
#             req(seq.select1(),data.select1(),ordre1())
#             if (req(input$plottype) == "I") {
#               return(seqplot(seqdata = seq.select1(), type = input$plottype, group = data.select1()[,input$souspop1],sortv = input$TapisSorted,yaxis=FALSE))
#             }else{
#               return(seqplot(seqdata = seq.select1(), type = input$plottype, group = data.select1()[,input$souspop1]))
#             }
#           }else{
#             return(NULL)
#           }
#         }
#         else{
#           req(seq.select1())
#           if (req(input$plottype) == "I") {
#             return(seqplot(seqdata = seq.select1(), type = input$plottype,sortv = input$TapisSorted,yaxis=FALSE))
#           }else{
#             return(seqplot(seqdata = seq.select1(), type = input$plottype))
#           }
#         }
#       }
#       
#     },width = large(),height = haut1())
#     
#     return(plotOutput("PLOT",height = tailleGraph$height))
#   }
#   
# })
# })

### Titre rappelant la selection choisie #####
reactive({
  if(input$plottype=="flux"){
    input$graph1
    isolate({
      if (input$souspop1=="Aucune" || input$souspop1==""){
        return("Vous avez sélectionné aucune sous population")
      }else{
        
        if (is.factor(data.comp[,input$souspop1])){
          req(input$souspop_modalite1)
          return(paste("Vous avez sélectionné la sous population",input$souspop1, "avec les modalités",paste(input$souspop_modalite1,collapse = ", ")))
        }
        if (is.numeric(data.comp[,input$souspop1])){
          
          return(paste("Vous avez sélectionné la sous population",input$souspop1, "entre",min(input$sous_pop_num1,na.rm=TRUE),"et",max(input$sous_pop_num1,na.rm=TRUE)))
          
        }
      }
    })
    
  } else {
    if (input$souspop1=="Aucune" || input$souspop1==""){
      return("Vous avez sélectionné aucune sous population")
    }else{
      
      if (is.factor(data.comp[,input$souspop1])){
        req(input$souspop_modalite1)
        return(paste("Vous avez sélectionné la sous population",input$souspop1, "avec les modalités",paste(input$souspop_modalite1,collapse = ", ")))
      }
      if (is.numeric(data.comp[,input$souspop1])){
        
        return(paste("Vous avez sélectionné la sous population",input$souspop1, "entre",min(input$sous_pop_num1,na.rm=TRUE),"et",max(input$sous_pop_num1,na.rm=TRUE)))
        
      }
    }
  }
  
})->text1

renderUI({
  req(text1())
  renderText(text1())->output$textGlobal
  return(h4(textOutput("textGlobal")))
})->output$h4_fluxGlobal

####### Télécharger les graphiques ###
####### Seqplot #
seqplot_fonction<-function(){
  if (req(input$plottype) %in% c("d", "f", "I", "ms", "mt", "r","Ht")) {
    req(ordre1())
    if (input$souspop1!="Aucune" && is.factor(data.comp[,input$souspop1])){
      req(input$souspop_modalite1)
      if (req(input$plottype) == "I") {
        return(seqplot(seqdata = seq.select1(), type = input$plottype, group = data.select1()[,input$souspop1],sortv = input$TapisSorted,yaxis=FALSE))
      }else{
        return(seqplot(seqdata = seq.select1(), type = input$plottype, group = data.select1()[,input$souspop1]))
      }
    }else{
      if (req(input$plottype) == "I") {
        return(seqplot(seqdata = seq.select1(), type = input$plottype,sortv = input$TapisSorted,yaxis=FALSE))
      }else{
        return(seqplot(seqdata = seq.select1(), type = input$plottype))
      }
    }
  }
}
####### Sous-sequences #
sousseqGraphique<-function(){
  if (req(input$plottype) %in% c("sous.seq","sous.seq.ch")){
    req(ordre1(),sousSeqPlot())
    return(marrangeGrob(sousSeqPlot(), layout_matrix=ordre1()))
  }
}
####### graphique de flux #
fluxGraph<-function(){
  if (req(input$plottype) == "flux"){
    req(flux1(),ordre1())
    return(marrangeGrob(flux1(), layout_matrix=ordre1()))
  }
} 

widthSousSeq<-function(){
  if (req(input$plottype)=="flux"){
    if (input$souspop1!="Aucune" && is.factor(data.comp[,input$souspop1]) && length(input$souspop_modalite1)>1){
      return(20)
    }else{
      return(12)
    }
  }else{
    if (input$souspop1!="Aucune" && is.factor(data.comp[,input$souspop1]) && length(input$souspop_modalite1)>1){
      return(20)
    }else{
      return(10)
    }
  }
}



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
        if (req(input$plottype) %in% c("d", "f", "I", "ms", "mt", "r","Ht")) {
          png(file,
              width = large(),
              height = haut1()) # open the png device
          # draw the plot
          seqplot_fonction()
          
        }
        if (req(input$plottype) %in% c("sous.seq","sous.seq.ch")){
          # draw the plot
          ggsave(file,plot=sousseqGraphique(),height =15*dim(ordre1())[1],width = widthSousSeq(),device = png()) #méthode pour télécharger les graphiques ggplot
          
        }
        
        if (req(input$plottype) == "flux"){
          ggsave(file,plot=fluxGraph(),height =7.5*dim(ordre1())[1],width = widthSousSeq(),device = png()) #méthode pour télécharger les graphiques ggplot
        }
        dev.off()  # turn the device off
        shiny::incProgress(1)
      })
  } 
)

onevent("mouseenter","DownGraphGlobal",show("TexteDownloadGraph"))
onevent("mouseleave", "DownGraphGlobal", hide("TexteDownloadGraph"))

####### Texte expliquant les graphiques #####
output$TexteGraph<-renderText({
  if (input$plottype=="d"){
    return("Le chronogramme représente la proportion d'individus (ou autres unités statistiques) à chaque pas de temps dans les différentes situations.")
  }
  if (input$plottype=="f"){
    return("Le graphique montre les séquences les plus réprésentées dans les données avec le pourcentage correspondant.")
  }
  if (input$plottype=="I"){
    return("Le tapis représente la séquence de chacun des individus(ou autres unités statistiques). Une ligne correspond à un individu(ou autre unité statistique).")
  }
  if (input$plottype=="ms"){
    return("Le graphique montre la situation la plus représentée pour chaque période, avec la proportion correpondante en ordonnée.")
  }
  if (input$plottype=="mt"){
    return("Le graphique représente le nombre de périodes moyennes pour chacune des situations.")
  }
  if (input$plottype=="Ht"){
    return("Le graphique permet de mesurer l’uniformité, ou non, d’une distribution. Une entropie faible, proche de 0 (forte, proche de 1) marque une forte (faible) uniformité des situations à chaque pas de temps.")
  }
  if (input$plottype=="flux"){
    return("Le graphique de flux montre la répartiton de chaucune des situations à chaque période (rectangles proportionnels). Le graphique permet de visualiser également la part des individus(ou autres unités statistiques) changeant de situations (zone entre les deux périodes).")
  }
  if (input$plottype == "sous.seq"){
    return("Le graphique affiche les sous-séquences les plus fréquentes et dont le support est supérieur au support minimal choisi. Le support correspond au nombre de séquences contenant la sous-séquence.")
  }
  if (input$plottype == "sous.seq.ch"){
    return("Le graphique affiche les sous-séquences choisies avec le support. Le support correspond au nombre de séquences contenant la sous-séquence.")
  }
})