#### DATA ####
#data("trajs")
#trajs<-read.csv("G:/dgsd/Dopro/Projets/2018/E - Etude_RSA/E-RSA_Appli_Shiny_Trajectoires/GitHub/ViCaTraj-master/data/trajs.csv",row.names = 1)

# rev(wesanderson::wes_palette(name = "Darjeeling1", n = length(alphabet(trajs)), type = "discrete"))->cpal.seq
# cpal(seqdata = trajs)<-cpal.seq
# seqtab(trajs[ , ], idxs = 0, format = "STS")->unique.trajs

#### SERVER ####
server <- function(input, output, session) {
  #### CR2EATION JEU DE DONNEES
  BIGLIST<-reactive({
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
    LIST_OBJSEQ()[[which(CLASS_LIST_OBJSEQ()<10)]]->df.pour.seq
  })


  
  observe({updateSelectInput(session = session, inputId = "MINTIMEBIG", 
                                 choices = names(BIGLIST() ) , 
                                selected= names(BIGLIST() )[1]
  )
                                                
                                })#"names(BIGLIST() ), selected =  names(BIGLIST() )) })
  observe({updateSelectInput(session = session, inputId = "MAXTIMEBIG", 
                             choices = names(BIGLIST() ) , 
                             selected= names(BIGLIST() )[length(  names(BIGLIST() )  )]
  )
  })#"names(BIGLIST() ), selected =  names(BIGLIST() )) })
  output$CONTROLNAMES<-renderText({print(names(BIGLIST() ))})
  output$SLIDERTEXT<-renderText({
    req( BIGLIST() )
    which(names(BIGLIST() )==input$MINTIMEBIG)->base
    which(names(BIGLIST() )==input$MAXTIMEBIG)->top
    if(length(base)!=1){
      base<-1
    }
    if(length(top)!=1){
      top<-1
    }
    seq(from=base, to = top, by=input$PAS_TEMPS_BIGDATA)->SEQ
    print( names(BIGLIST())[SEQ]  )
    })
  
  #### CONDITIONS ON INDIDIVUS
 ##### DEFINE DF #####

  values <- reactiveValues()
  values$DF_subset_initial <- data.frame("PAQUET"="", "DATE"="", "VARIABLE"="", "TYPE"="", 
                                         "TEXT_PATTERN"="","FACTOR_LEVELS"="", "min"="",  "max"="")
  
##### DEFINE INPUTS #####
  output$UI_PAQUET_SELECT<-renderUI({
    shiny::numericInput(inputId="PAQUET_FOR_SELECT", value=1, label = "Paquet : ", min = 1, step = 1)
  })
  
  output$UI_DATE_SELECT<-renderUI({
    #reactive({
    names(BIGLIST())->names.pick
    shiny::selectInput(inputId = "DATE_FOR_SELECT", label = "Date pour sélection:",
                       choices = names.pick, multiple = FALSE)
  })
  #### SELECT VAR et MODLITE ####
  reactive({BIGLIST()[[input$DATE_FOR_SELECT]]})->the.df
  
 output$UI_INDVAR_CHOOSE<-renderUI({
   lapply(BIGLIST(), function(bi){
     names(bi)
   })->lina
   unique(unlist(lina))->glona
   selectInput(inputId = "INDVAR", label = "Variable pour identifiant individuel: ", 
               choices = glona, multiple = FALSE)
 })
 INDVAR<-reactive({input$INDVAR})
  
  output$UI_VAR_SELECT<-renderUI({
    req(the.df())
    #mycolumns<-unique(unlist(Reduce(intersect,list(lapply(X = list_csv(), FUN = names))), 
    #                         use.names = FALSE))
    selectInput(inputId = "VAR_FOR_SELECT", label = "Variable pour sélection", 
                choices = names(the.df()), multiple = FALSE)
  })
  
  THE_VAR<-reactive({
    req( the.df() )
    the.df()[ , input$VAR_FOR_SELECT]->the.var
    the.var
  })
  
  output$UI_CLASS_SELECT<-renderUI({
    req(THE_VAR() )
    class(THE_VAR())->CLASS_VAR
    shiny::selectInput(inputId = "classSelect", label = "Contrôle de la classe", 
                       choices =  c("factor", "character", "numeric", "Date", "integer"), 
                       selected = CLASS_VAR)
  })
  
  output$UI_MOD_SELECT<-renderUI({
    req(THE_VAR() )
    req(input$classSelect)
    
    
    if(input$classSelect%in%c("numeric", "integer")){
      
      as.numeric(THE_VAR())->temp.num.var
      
      minis <- min(temp.num.var, 
                   na.rm = TRUE)
      maxis <- max(temp.num.var, 
                   na.rm = TRUE)
      
      
      shiny::sliderInput(inputId = "NumSelect", label = "Valeurs sélectionnées", 
                         min = minis, max = maxis, 
                         value = c(minis, maxis))
    } else {
      if(input$classSelect=="factor"){#, "character") ){
        if(length(unique(THE_VAR()))>100){
          table(THE_VAR())->tab
          tab[order(tab, decreasing = TRUE)]->tab
          tab[1:25]->tab
        } else {tab<-unique(THE_VAR() )}
        shiny::selectInput(inputId = "FactSelect", label="Valeurs sélectionnées", 
                           choices = tab  , multiple = TRUE)
      } else {
        if(input$classSelect=="character"){#, "character") ){
          shiny::textInput(inputId = "CharPatSelect", label="'Paterns' à rechercher (sep by '/'", value = ""
          )
        } else  {
          if(input$classSelect=="Date" ){
            list(
              shiny::textInput(inputId = "DATEformat", label = "Format d'origine", value = "" ),
              shiny::dateRangeInput(inputId = "DATE_RANGE", 
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
    as.Date(THE_VAR(), format=as.character(input$DATEformat) )->THE_VAR_DATE
    print(THE_VAR_DATE)
    class(THE_VAR_DATE)
    min(THE_VAR_DATE, na.rm = TRUE)->mindate
    print(mindate)
    max(THE_VAR_DATE, na.rm = TRUE)->maxdate
    print(maxdate)
    shiny::updateDateRangeInput(session = session, inputId = "DATE_RANGE", start = mindate, end=maxdate )
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
    
    if(input$classSelect=="factor"){
      
      paste("'", paste(isolate(input$FactSelect), collapse = "','"), "'", sep="")->choix
      paste("c(", choix, ")" )->factor_levels
      
    } else {
      if(isolate(input$classSelect)=="character"){
        isolate(input$CharPatSelect)->text_pattern
      } else {
        if(input$classSelect=="Date"){
          isolate(input$DATE_RANGE)->choix
          choix[1]->minus
          choix[2]->maxus
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
               "max"=maxus
    )->vecto 
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
                           "as.Date('" ,  data_of_subset$min[i], "',",
                           "format=", "'", "%Y-%m-%d", "')", sep=""
                           ),
                    "&",
                    paste( "as.Date(" ,  dfvar, "$", data_of_subset$VARIABLE[i], ",",
                           "format=", "'", input$DATEformat, "')",
                           "<=", 
                           "as.Date('" ,  data_of_subset$max[i], "',",
                           "format=", "'", "%Y-%m-%d", "')", sep=""
                    ),
                    ")", sep="")
              
              
        }
      }
      }
        }
        })
    data.frame("PAQUET"=data_of_subset$PAQUET, DATE=data_of_subset$DATE, "string_for_sub"=string_for_sub, 
               stringsAsFactors = FALSE)
    })->STRING_FOR_SUB
  
  INDIVIDUELS<-reactive({
    req(STRING_FOR_SUB() )
    print(STRING_FOR_SUB())
    
    req(INDVAR())
    list.of.inf.by.cond<-lapply(1:nrow(STRING_FOR_SUB() ), function(i){
      subset(BIGLIST()[[STRING_FOR_SUB()$DATE[i]]], 
             eval(parse(text = as.character(STRING_FOR_SUB()$string_for_sub[i]) )) )[ , INDVAR()]
    })
    output$CONTROL_LIST.OF.INF<-renderText({unlist(list.of.inf.by.cond)})
    lapply(unique(STRING_FOR_SUB()$PAQUET), function(pi){
      which(STRING_FOR_SUB()$PAQUET==pi)->indexes.pi
      unique(unlist(list.of.inf.by.cond[indexes.pi]))
    })->ind.by.paq
    
    Reduce(intersect, ind.by.paq)->ind.all.paq
    ind.all.paq
  })
  
  reactive({
    req(BIGLIST() )
    req(INDIVIDUELS())
    lapply(BIGLIST(), FUN = function(bi){
    subset(bi, bi[ , INDVAR()]%in%INDIVIDUELS())
    })
    })->SUBSETTED_LIST
  
  output$LENGTH_IND_SUBS<-renderText({ length( INDIVIDUELS() )    }) 
  output$LENGTH_SUBSETTED<-renderText({ length( SUBSETTED_LIST() )    }) 
  
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
  #### OBJET .RData ####
  # list_csv<-reactive({
  #   if(input$DataType=="objet"){
  #       if ( is.null(input$objetFile)) return(NULL)
  #       inFile <- input$objetFile
  #       file <- inFile$datapath
  #       # load the file into new environment and get it from there
  #       e = new.env()
  #       name <- load(file, envir = e)
  #       data <- e[[name]]
  #       #
  #       mycolumns<-unique(unlist(Reduce(intersect,list(lapply(X = data, FUN = names))), 
  #                                use.names = FALSE))
  #       updateSelectInput(session = session, inputId = "timecol", choices = mycolumns)
  #       #
  #       return(data)
  #     }
  # })
  renderPrint(print(length(SUBSETTED_LIST())))->output$CONTROLDATA
 #### Un fichier source ####
  data<-reactive({
    req(trajs$dataSource)
    if(input$DataType=="fichier"){ #### Chargement d'un seul fichier CSV ####

    if (input$rowname==TRUE && input$rownames_par=="")
    {
      # userData <- read.csv(file = input$file1$datapath, sep = input$sepcol, encoding = input$endoding)
      # trajs$df<- userData
      colonneID<- c(colnames(trajs$df)[colId(df = trajs$df)])
      updateSelectInput(session = session, inputId = "rownames_par",choices = c("",colonneID))
      }
    if (input$rowname==TRUE && input$rownames_par!=""){
      userData <- read.csv(file = input$file1$datapath, 
                           sep = input$sepcol, 
                           encoding = input$endoding,
                           row.names = input$rownames_par,
                           header=input$header,na.strings = argna(),
                           dec=input$dec)
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
    if(input$DataType=="fichier"){
      shiny::dateRangeInput(inputId = "date.range", label = "Dates de début et de fin",
                            format = "mm-yyyy")->the.ui
    } else {
      if(input$DataType=="objet"){
        names(SUBSETTED_LIST())->names.pick
         mycolumns<-unique(unlist(Reduce(intersect,list(lapply(X = SUBSETTED_LIST(), FUN = names))),
                                       use.names = FALSE))
        updateSelectInput(session = session, inputId = "timecol", choices = mycolumns)
        list(
          shiny::selectInput(inputId = "PICKDATE1", label = "Debut:",
                           choices = names.pick, multiple = FALSE),
          shiny::selectInput(inputId = "PICKDATE1", label = "Fin:",
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
  
  
  
  
  output$CONTROL_DUPLICATED_ID<-renderUI({
    req( DR_POUR_SEQ_OBJ())
    req(input$INDVAR)
    if(sum(duplicated(x = DR_POUR_SEQ_OBJ()[ , input$INDVAR]))>1){
      list(
      h3("ATTENTION: la variable d'identifiant individuel sélectionnée comporte des doublons."),
      h3(" Vous pouvez choisir d'éliminer les doublons ou vous pouvez sélectionner une autre variable d'identifiant individuel"),
      shiny::checkboxInput(inputId = "ELIMINATE_DOUBLONS",label = "Faut-il éliminer les doublons dans la variable didentifiant individuel?", value = FALSE)
      )
    } else {
      h3("OK: la variable d'identifiant individuel sélectionnée ne comporte pas de doublons.")
    }
  })
  
  
  DR_POUR_SEQ_OBJ.CONTROL.DOUBLONS<-reactive({
    req( DR_POUR_SEQ_OBJ())
    req(input$INDVAR)
    if(sum(duplicated(x = DR_POUR_SEQ_OBJ()[ , input$INDVAR]))>1){
      if(input$ELIMINATE_DOUBLONS==TRUE){
        drpourseq<-DR_POUR_SEQ_OBJ()[!duplicated(DR_POUR_SEQ_OBJ()[ , input$INDVAR]) , ]
      } else {NULL}
    } else {
      DR_POUR_SEQ_OBJ()
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
      DR_POUR_SEQ_OBJ.CONTROL.DOUBLONS()->df.pour.seq
      if(!is.null(DR_POUR_SEQ_OBJ.CONTROL.DOUBLONS())){
        seqdef(id = DR_POUR_SEQ_OBJ.CONTROL.DOUBLONS()[ , input$INDVAR], 
               data = DR_POUR_SEQ_OBJ.CONTROL.DOUBLONS()[ , grepl("_VAR", x = names(DR_POUR_SEQ_OBJ.CONTROL.DOUBLONS()))&names(DR_POUR_SEQ_OBJ.CONTROL.DOUBLONS())!=input$INDVAR],
               gaps = input$TEXT_GAP,
               right = input$TEXT_RIGHT,
               left = input$TEXT_LEFT,nr = "RMA"
                
               
               #  names(df_RSA.sampled)[names(df_RSA.sampled)!="ID"]], 
               #right = "NO.RSA", 
               #left="NO.RSA", 
               #gaps = "GAP", 
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


      
      
      
        # if(sum(duplicated(x = DR_POUR_SEQ_OBJ()[ , input$INDVAR]))>1){
        # if(input$ELIMINATE_DOUBLONS==TRUE){
        #   drpourseq<-DR_POUR_SEQ_OBJ()[!duplicated(DR_POUR_SEQ_OBJ()[ , input$INDVAR]) , ]
        #   seqdef(id = drpourseq[ , input$INDVAR], 
        #          data = drpourseq[ , grepl("_VAR", x = names(drpourseq))&names(drpourseq)!=input$INDVAR],#  names(df_RSA.sampled)[names(df_RSA.sampled)!="ID"]], 
        #          #right = "NO.RSA", 
        #          #left="NO.RSA", 
        #          #gaps = "GAP", 
        #   )->s
        # } else {
        #   s<-NULL
        # }
        # } else {
        #   seqdef(id = DR_POUR_SEQ_OBJ()[ , input$INDVAR], 
        #          data = DR_POUR_SEQ_OBJ()[ , grepl("_VAR", x = names(DR_POUR_SEQ_OBJ()))&names(DR_POUR_SEQ_OBJ())!=input$INDVAR],#  names(df_RSA.sampled)[names(df_RSA.sampled)!="ID"]], 
        #          #right = "NO.RSA", 
        #          #left="NO.RSA", 
        #          #gaps = "GAP", 
        #   )->s
    
    if (!is.null(s)){
      if (length(alphabet(s))<=12&!is.null(s)){
        #permet d'avoir les mêmes couleurs que pour les graphiques de flux
        a<-col_flux(data = df.pour.seq, seq.data = s)
        attr(s, "cpal") <- unname(a[alphabet(s)])
      }
      }
      return(s)
    
  })
  
  # output$WARNINGS.UNIQUE.ID<-renderText({
  #   if(!is.null(data.seq())){
  #   "OK: la variable d'identifiant individuel sélectionnée est unique"
  #   } else {
  #   "La variable d'identifiant individuel sélectionnée comporte des doublons. Impossible de créer l'objet trajectoire ( seqdef() )"
  #   }
  # })

    output$CLASS_TRAJ_OBS<-renderPrint({
    class( data.seq() )
    #summary(data.seq())
  })
  
  output$DES_TRAJ_OBJ<-renderPrint({
    dim( data.seq() )
    #summary(data.seq())
    })
  
  output$ATTR_TRAJ_OBJ<-renderUI({
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
  if(input$DataType=="fichier"){ 
    data()
  } else {
    if(input$DataType=="objet"){ 
      DR_POUR_SEQ_OBJ.CONTROL.DOUBLONS()
    } else {
      if(input$DataType=="objseq"){
        OBJSEQ_COMPLEMENT()
      }
    }
  }
})

#### STATISTIQUES DESCRIPTIVES ####

  
  ###max PAStrate ####
observeEvent(eventExpr = data.seq(),{
  req(data.seq())
  updateNumericInput(session=session, inputId = "PAStrate",max=length(nom_var_seq())-1)
})

  PASTRAJ<-reactive({
    input$PAStrate
  })
  TYPETRATE<-reactive({
    input$TYPEtrate
  })
  
  data.seq.inv<-reactive({
    req(data.seq())
    #req(input$timecol)
    print("summary(data.seq())")
    print(summary(data.seq()))
    return(
      #seqdef(data2()[,rev(nom_var_seq())],cpal = NULL)
      seqdef(data.seq()[rev(names(data.seq()))], 
             cpal=NULL,
             gaps = input$TEXT_GAP,
             right = input$TEXT_RIGHT,
             left = input$TEXT_LEFT,nr = "RMA")
      )
  })
  
  #On organise la sequence dans l'autre sens afin d'avoir les transitions arrivant et pas seulement celles partant d'une situation
  Seqordre<-reactive({
    req(data.seq(),data.seq.inv())
    if(input$DebArr=="deb"){
      return(data.seq())
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
    req(data.seq(),LISTTRATE())
    
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
      gsub(pattern=" ",replacement="_", x = alphabet(data.seq()))->names.alpha
      names(xx)<-names.alpha
      print("xx names")
      print(xx)
      print("alphabet")
      print(alphabet(data.seq()))
      
      colnames(xx)<-gsub(pattern = "[-> ", replacement = "", fixed = TRUE, x = colnames(xx))
      colnames(xx)<-gsub(pattern = "]", replacement = "", fixed = TRUE, x = colnames(xx))
      
      colnames(xx)->names.alpha
      if(input$DebArr=="deb"){
        cbind(xx, "DEPART"=alphabet(data.seq()))->xx
        #alphabet(data.seq())->xx$DEPART
        xx[, c("DEPART", names.alpha)]->xx
      }else{
        cbind(xx, "ARRIVEE"=alphabet(data.seq()))->xx
        
        #alphabet(data.seq())->xx$ARRIVEE
        xx[, c("ARRIVEE", names.alpha)]->xx
      }
      
      #print(xx)
      output[[id.output]] <- DT::renderDataTable({xx})
      #Affiche les pas de temps considérés dnas le(s) tableau(x)
      paste0('Text_TRAJTRATE', i)->id.output.text
      if (input$TYPEtrate==TRUE){
        output[[id.output.text]] <- renderText(paste("Entre",colnames(data.seq())[i],"et",colnames(data.seq())[i+PASTRAJ()]))
      }else{
        output[[id.output.text]] <- renderText(paste("Sur l'ensemble de la période étudiée (entre",colnames(data.seq())[1],"et",colnames(data.seq())[dim(data.seq())[2]],")"))
      }
      paste0('DownloadTabTrans',i)->id.output.download
      output[[id.output.download]] <- downloadHandler(
        filename = function() {
          paste0('Transition_',colnames(data.seq())[i],'_',colnames(data.seq())[i+PASTRAJ()], input$TypeTrans)
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
  ####### Type de graph ####
  #mise a jour des input et création de nouveaux input selon le type de sous-population choisi
  observeEvent(eventExpr = input$ValidParametres,{
    req(data.seq())
    updateSelectInput(session = session, inputId = "timeseq1", choices = input$timecol)
    
    colsouspop<-colnames(data2())[!(colnames(data2()) %in% input$timecol)]
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
      if (is.numeric(data2()[,input$souspop1])){
        min<-min(data2()[,input$souspop1],na.rm = TRUE)
        max<-max(data2()[,input$souspop1],na.rm = TRUE)
        sliderInput(inputId = "sous_pop_num1", label="Slider",min=min,max=max,value = c(min,max))
      }
    }
  })
  output$modalite1<- renderUI({
    if(input$souspop1!="Aucune"){
      if (is.factor(data2()[,input$souspop1])){
        selectInput(inputId = "souspop_modalite1",label="Modalité", choices = levels(data2()[,input$souspop1]),selected="",multiple = TRUE)
      } else {if(is.character(data2()[,input$souspop1])){
        if(length(unique(data2()[,input$souspop1]))<25){
          selectInput(inputId = "souspop_modalite1",label="Modalité", choices = unique(data2()[,input$souspop1]),selected="",multiple = TRUE)
          
          }
      }}
    }
  })
  
  
  
  ####### Selection des sous populations ####
  data.select1<-reactive({
    req(input$souspop1)
    
    if (input$souspop1=="Aucune" || input$souspop1==""){
      data.select<-data2()
    }else{
      
      if (is.factor(data2()[,input$souspop1])|(is.character(data2()[,input$souspop1])&length(unique(data2()[,input$souspop1]))<25)){
        if(length(input$souspop_modalite1)>0){
          data.select<-data2()[(data2()[,input$souspop1] %in% c(input$souspop_modalite1)),]
        }else{
          data.select<-NULL
        }
      }else{
        if (is.numeric(data2()[,input$souspop1])){
          req(input$sous_pop_num1)
          data.select<-data2()[which(data2()[,input$souspop1]<= max(input$sous_pop_num1,na.rm=TRUE) & data2()[,input$souspop1]>= min(input$sous_pop_num1,na.rm=TRUE)),]
          
        }
      }
    }
    return(data.select)
    
  })
  
  seq.select1<-reactive({
    req(input$souspop1)
    
    if (input$souspop1=="Aucune" || input$souspop1==""){
      seq.select<-data.seq()
    }else{
      
      if (is.factor(data2()[,input$souspop1])){
        if(length(input$souspop_modalite1)>0){
          seq.select<-data.seq()[(data2()[,input$souspop1] %in% c(input$souspop_modalite1)),]
        }else{
          seq.select<-NULL 
        }
      } else {
        if(is.character(data2()[,input$souspop1])&length(unique(data2()[,input$souspop1]))<25){
          if(length(unique(input$souspop_modalite1)>0)){
            seq.select<-data.seq()[(data2()[,input$souspop1] %in% c(input$souspop_modalite1)),]
        }
      } else {
      if (is.numeric(data2()[,input$souspop1])){
        req(input$sous_pop_num1)
        seq.select<-data.seq()[which(data2()[,input$souspop1]<= max(input$sous_pop_num1,na.rm=TRUE) & data2()[,input$souspop1]>= min(input$sous_pop_num1,na.rm=TRUE)),]
      }
      }
      }
    }
    return(seq.select)
    
  })
    
    

  
  
  ####### Pas de temps voulue pour les graphiques de flux avec au minimun deux pas de temps ###
  col_periode1<-eventReactive(eventExpr = length(input$timeseq1)>=2,{
    input$timeseq1
  })
  
  ####### Création d'une liste des graphiques de flux pour pouvoir les tracer côte à côte
  flux1<-eventReactive(eventExpr = input$graph1,{
    req(data.select1(),col_periode1(),seq.select1())
    if (input$souspop1!="Aucune" && is.factor(data2()[,input$souspop1])) {
      lapply(1:length(input$souspop_modalite1), FUN=function(i){
        graph_flux_grp(data=data.select1(),seq_data=seq.select1(),col_periode=col_periode1(),var_grp=input$souspop1,label_grp= as.character(input$souspop_modalite1[i]))
      })
    }
    else{
      return(list(graph_flux(data=data.select1(),seq_data=seq.select1(),col_periode=col_periode1())))
    }
    
  })
  
  ####### Graphique sous séquence ####
  ####### Création des graphiques pour les deux types de sous-séquences ###
  
  sousSeqPlot<-reactive({
    req(seq.select1())
    if (req(input$plottype) == "sous.seq"){
      #Pour la comparaison des sous-populations, on met les graphiques dans une liste#
      if (input$souspop1!="Aucune" && is.factor(data2()[,input$souspop1])) {
        if(length(input$souspop_modalite1)>0 && input$souspop_modalite1 %in% levels(data2()[,input$souspop1]) ){
          lapply(1:length(input$souspop_modalite1), FUN=function(i){
            seq.select1()[data.select1()[,input$souspop1]==input$souspop_modalite1[i],]->seqSouspop
            seqecreate(seqSouspop, tevent="state", use.labels=FALSE)->seqGlobal
            titre<-paste("Graphique des sous-séquneces \n pour la variable",input$souspop1,"\n avec la modalité",input$souspop_modalite1[i])
            sousTitre<-paste("Il y a",nrow(seqSouspop),"individus")
            seqefsub(seqGlobal,pmin.support=input$pmin1)->p
            return(graph_sous_sequences(p)+ggtitle(titre,subtitle = sousTitre))
          })
        }   
      } else {
        seqecreate(seq.select1(), tevent="state", use.labels=FALSE)->seqGlobal
        return(list(graph_sous_sequences(seqefsub(seqGlobal,pmin.support=input$pmin1))))
      }
    }else{
      ## Cas où l'utilisateur choisi les sous-séquences ##
      if (req(input$plottype) == "sous.seq.ch"){
        req(values$df)
        #condition d'un data.frame values non vide pour exécuter la suite du code afin de ne pas avoir d'erreur quand la data.frame est vide
        if(nrow(values$df)>0){
          if (input$souspop1!="Aucune" && is.factor(data2()[,input$souspop1])) {
            if(length(input$souspop_modalite1)>0 && input$souspop_modalite1 %in% levels(data2()[,input$souspop1]) ){
              lapply(1:length(input$souspop_modalite1), FUN=function(i){
                
                seq.select1()[data.select1()[,input$souspop1]==input$souspop_modalite1[i],]->seqSouspop
                seqecreate(seqSouspop, tevent="state", use.labels=FALSE)->seqGlobal
                vectSeq<-vect.sous.seq(data = values$df)
                seqefsub(seqGlobal,str.subseq=vectSeq)->p
                titre<-paste("Graphique des sous-séquneces \n pour la variable",input$souspop1,"\n avec la modalité",input$souspop_modalite1[i])
                sousTitre<-paste("Il y a",nrow(seqSouspop),"individus")
                return(graph_sous_sequences(p[order(p$data$Support,decreasing = TRUE),])+ggtitle(titre,subtitle = sousTitre))
                
              })
            }   
          } else {
            
            seqecreate(seq.select1(), tevent="state", use.labels=FALSE)->seqGlobal
            vectSeq<-vect.sous.seq(data = values$df)
            seqefsub(seqGlobal,str.subseq=vectSeq)->p
            return(list(graph_sous_sequences(p[order(p$data$Support,decreasing = TRUE),])))
            
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
        if (input$souspop1!="Aucune" && is.factor(data2()[,input$souspop1])) {
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
      if (input$souspop1!="Aucune" && is.factor(data2()[,input$souspop1])) {
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
    if (input$souspop1!="Aucune" && is.factor(data2()[,input$souspop1])){
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
  output$PLOT3<- renderUI({
    if (req(input$plottype) %in% c("sous.seq","sous.seq.ch")){
      output$PLOT<-renderPlot({
        if (req(input$plottype) %in% c("sous.seq","sous.seq.ch")){
          req(sousSeqPlot(),ordre1())
          tailleGraph$height<-dim(ordre1())[1]*1000
          return(marrangeGrob(sousSeqPlot(), layout_matrix=ordre1()))
        }
      },width = large(),height = haut1())
      return(plotOutput("PLOT",height = tailleGraph$height))
    }
    if (req(input$plottype) == "flux"){
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
    }
    if (req(input$plottype) %in% c("d", "f", "I", "ms", "mt", "r","Ht")) {
      output$PLOT<-renderPlot({
        if (req(input$plottype) %in% c("d", "f", "I", "ms", "mt", "r","Ht")) {
          req(seq.select1(),data.select1(),ordre1())
          tailleGraph$height<-dim(ordre1())[1]*400
          if (input$souspop1!="Aucune" && is.factor(data2()[,input$souspop1])){
            if(length(input$souspop_modalite1)>0 && input$souspop_modalite1 %in% levels(data2()[,input$souspop1]) ){
              req(seq.select1(),data.select1(),ordre1())
              if (req(input$plottype) == "I") {
                return(seqplot(seqdata = seq.select1(), type = input$plottype, group = data.select1()[,input$souspop1],sortv = input$TapisSorted,yaxis=FALSE))
              }else{
                return(seqplot(seqdata = seq.select1(), type = input$plottype, group = data.select1()[,input$souspop1]))
              }
            }else{
              return(NULL)
            }
          }
          else{
            req(seq.select1())
            if (req(input$plottype) == "I") {
              return(seqplot(seqdata = seq.select1(), type = input$plottype,sortv = input$TapisSorted,yaxis=FALSE))
            }else{
              return(seqplot(seqdata = seq.select1(), type = input$plottype))
            }
          }
        }
        
      },width = large(),height = haut1())
      
      return(plotOutput("PLOT",height = tailleGraph$height))
    }
    
  })
  
  ### Titre rappelant la selection choisie #####
  reactive({
    if(input$plottype=="flux"){
      input$graph1
      isolate({
        if (input$souspop1=="Aucune" || input$souspop1==""){
          return("Vous avez sélectionné aucune sous population")
        }else{
          
          if (is.factor(data2()[,input$souspop1])){
            req(input$souspop_modalite1)
            return(paste("Vous avez sélectionné la sous population",input$souspop1, "avec les modalités",paste(input$souspop_modalite1,collapse = ", ")))
          }
          if (is.numeric(data2()[,input$souspop1])){
            
            return(paste("Vous avez sélectionné la sous population",input$souspop1, "entre",min(input$sous_pop_num1,na.rm=TRUE),"et",max(input$sous_pop_num1,na.rm=TRUE)))
            
          }
        }
      })
      
    } else {
      if (input$souspop1=="Aucune" || input$souspop1==""){
        return("Vous avez sélectionné aucune sous population")
      }else{
        
        if (is.factor(data2()[,input$souspop1])){
          req(input$souspop_modalite1)
          return(paste("Vous avez sélectionné la sous population",input$souspop1, "avec les modalités",paste(input$souspop_modalite1,collapse = ", ")))
        }
        if (is.numeric(data2()[,input$souspop1])){
          
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
      if (input$souspop1!="Aucune" && is.factor(data2()[,input$souspop1])){
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
      if (input$souspop1!="Aucune" && is.factor(data2()[,input$souspop1]) && length(input$souspop_modalite1)>1){
        return(20)
      }else{
        return(12)
      }
    }else{
      if (input$souspop1!="Aucune" && is.factor(data2()[,input$souspop1]) && length(input$souspop_modalite1)>1){
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

  
  
  
  
  
  
  ### NOMBRE DE TRAJECTOIRES: TOTAL ET  SELECTIONNEES  ####
  NB_TRAJS<-shiny::reactive({
    req(data.seq())
    print( nrow(data.seq()))
    nrow(data.seq())
  })
  unique.trajs<-shiny::reactive({
    req(data.seq())
    seqtab(data.seq()[ , ], idxs = 0, format = "STS")
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


    renderUI(expr = #conditionalPanel(condition = "input.selection_rows == 'Sample'",
                                     tags$strong(
                                       if(input$selection_rows == "Sample"){
      paste("Vous avez sélectionné un échantillon de ",NB_SELECT_TRAJS(), "trajectoires", sep=" ")
                                       } else {if(input$selection_rows == "unique.traj"){
                                         paste("Vous avez sélectionné",NB_SELECT_TRAJS(), "trajectoires uniques pondérées", sep=" ")
                                         } else {
                                           paste("Vous avez tout sélectionné, soit ",NB_SELECT_TRAJS(), "trajectoires", sep=" ")
                                         }
                                       }
                                         ))->output$TEXT_NB_SELECTED_TRAJS

    
  DATE_RANGE<-eventReactive(input$ValidParametres, {
    input$date.range
  })
  MOIS_GAP<-eventReactive(input$ValidParametres, {
    input$criterNb
  })
# 
#    ####
  
  
  #### DISTANCE ET CLASSIF ####
   
   #### UPDATE "classtype" input selon input$type_distance ####
  observe({
    x <- input$type_distance
    names(
     seqdist.args[[x]])->listed_names_methods
    # Can also set the label and select items
    updateSelectInput(session, "classtype",
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
  ###trajs.forclass  ####
  trajs.forclass<-reactive({#shiny::eventReactive(eventExpr = input$selection_rows, {
    req(data.seq())
    if(input$selection_rows=="Sample"){
      ###sample ####
      sample(x = 1:nrow(data.seq()), size = input$sample_prop*nrow(data.seq()), replace = FALSE)->vec.sample
      seqdef(data.seq()[vec.sample , ], 
             left = input$TEXT_LEFT, 
             right = input$TEXT_RIGHT, 
             gaps = input$TEXT_GAP, nr = "RMA",
             id = row.names(data.seq()[vec.sample , ]))
    } else {
      if(input$selection_rows=="unique.traj"){
        #### unique.traj ####

        rev(wesanderson::wes_palette(name = "Darjeeling1", n = length(alphabet(data.seq())), type = "continuous"))->cpal.seq
        #cpal(seqdata = data.seq())<-cpal.seq
        #seqtab(data.seq()[ , ], idxs = 0, format = "STS")->unique.trajs
        #data.frame(unique.trajs)->unique.trajs.df
        data.frame(unique.trajs())->unique.trajs.df
        seqdef(data = unique.trajs.df, weights = attributes(unique.trajs())$freq$Percent, cpal = cpal.seq, 
               gaps = input$TEXT_GAP,
               right = input$TEXT_RIGHT,
               left = input$TEXT_LEFT,nr = "RMA")->unique.trajs.seq
        unique.trajs.seq
      } else {
        #### all ####
        data.seq()
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
    #summary(data.seq())
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
             lag=input$lag_subst_cost, weighted = TRUE)
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
   output$PRINTSEQDIST<-renderUI({
     req(SEQDIST())
 renderText(paste("Création d'un objet 'dist' comportant", length(SEQDIST()), "élements. /n La distance minimale est de", min(SEQDIST()), "la distance maximale de", max(SEQDIST()), "et la distance moyenne de", round(sum(SEQDIST())/length(SEQDIST()), 2), sep = " "))->output$cc
     textOutput("cc") %>% withSpinner(color="#0dc5c1")
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
     if (input$cluster_type=="CAHPAM"){
       indicateur<-Creation_indicateur(nb_cluster_min=min(input$SliderGrp,na.rm=TRUE),nb_cluster_max=max(input$SliderGrp,na.rm=TRUE),mat_dist=SEQDIST(),intialclust=SEQCLASS())
       return(data_cluster(indicateur,data2(),input$nb_cluster))
     }
     if(input$cluster_type=="CAH"){
       data2()->dataCopieCAH
       clusterCAH<-as.factor(cutree(SEQCLASS(),k = input$nb_cluster))
       dataCopieCAH[,"Clustering"]<-factor(clusterCAH,labels = paste0("G",1:input$nb_cluster))
       
       return(dataCopieCAH)
     }
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
   
#
   ### PLOT G  ####
        ##### Mise a jour des inputs #####
   observeEvent(eventExpr = input$Bouton_Clustering,{
     #pas de temps pour les graphiques de flux
     updateSelectInput(session = session, inputId = "timeseq2", choices = input$timecol)
     #sous population
     colsouspop2<-colnames(data2())[!(colnames(data2()) %in% input$timecol)]
     updateSelectInput(session = session, inputId = "souspop2", choices = c("Aucune",colsouspop2))
   })
   
   observeEvent(input$souspop2,{
     req(input$souspop2)
     if (input$souspop2=="Aucune"){
       updateSelectInput(session = session, inputId = "souspop_modalite2", choices = "" )
     }
   })
   
   output$slider2<- renderUI({
     if(input$souspop2!="Aucune"){
       if (is.numeric(data2()[,input$souspop2])){
         min<-min(data2()[,input$souspop2],na.rm = TRUE)
         max<-max(data2()[,input$souspop2],na.rm = TRUE)
         sliderInput(inputId = "sous_pop_num2", label="Slider",min=min,max=max,value = c(min,max))
       }
     }
   })
   output$modalite2<- renderUI({
     if(input$souspop2!="Aucune"){
       if (is.factor(data2()[,input$souspop2])){
         selectInput(inputId = "souspop_modalite2",label="Modalité", choices = levels(data2()[,input$souspop2]),selected="",multiple = TRUE)
       }
     }
   })

   
      #### Selection de la sous population ####
   data.select2<-reactive({
     req(input$souspop2,dataCluster())
       if (input$souspop2=="Aucune" || input$souspop2==""){
         data.selectG<-dataCluster()
       }else{
         
         if (is.factor(dataCluster()[,input$souspop2])){
           req(input$souspop_modalite2)
           data.selectG<-dataCluster()[which(dataCluster()[,input$souspop2] %in% c(input$souspop_modalite2)),]
         }
         if (is.numeric(dataCluster()[,input$souspop2])){
           req(input$sous_pop_num2)
           data.selectG<-dataCluster()[which(dataCluster()[,input$souspop2]<= max(input$sous_pop_num2,na.rm=TRUE) & dataCluster()[,input$souspop2]>= min(input$sous_pop_num2,na.rm=TRUE)),]
           
         }
         
       }
       return(data.selectG)
   })
   
   seq.select2<-reactive({
     req(input$souspop2)
     
     if (input$souspop2=="Aucune" || input$souspop2==""){
       seq.selectG<-data.seq()
     }else{
       
       if (is.factor(data2()[,input$souspop2])){
         req(input$souspop_modalite2)
         seq.selectG<-data.seq()[which(data2()[,input$souspop2] %in% c(input$souspop_modalite2)),]
       }
       if (is.numeric(data2()[,input$souspop2])){
         req(input$sous_pop_num2)
         seq.selectG<-data.seq()[which(data2()[,input$souspop2]<= max(input$sous_pop_num2,na.rm=TRUE) & data2()[,input$souspop2]>= min(input$sous_pop_num2,na.rm=TRUE)),]
         
       }
     }
     
     return(seq.selectG)
     
   })
   
   
   ### Proposition des groupes à représenter ### 
   
   grp<-reactive({
     #if (input$plottypeG=="flux"){
     input$souspop_modalite2
     input$souspop2
     input$sous_pop_num2
     isolate({
       req(data.select2())
       unique(data.select2()[,"Clustering"])
     })
   })
   
   observe({
     req(grp())
     updateSelectInput(session = session, inputId = "var_grp", choices = grp())
   })
   
   ### Selectionne de la période temporelle ###
   col_periode2<-eventReactive(eventExpr = length(input$timeseq2)>=2,{
     input$timeseq2
   })

   ### Graphique de flux pour les groupes ###
   
   observeEvent(input$graph2,{
       if(input$plottypeG=="flux"){
         if (input$souspop2!="Aucune" && is.factor(data2()[,input$souspop2])) {
           req(ordre())
           tailleGraph$height<-dim(ordre())[1]*400
           lapply(1:length(input$souspop_modalite2), FUN=function(j){
             paste0('SEQPLOTFLUX', j)->id.output
             output[[id.output]] <- renderPlot({
              input$graph2
               isolate({
                 if (input$souspop2!="Aucune" && is.factor(data2()[,input$souspop2])) {
                   req(data.select2(),seq.select2(),col_periode2(),input$var_grp,input$souspop2,input$souspop_modalite2)
                   return(marrangeGrob(lapply(1:length(input$var_grp), FUN=function(i){
                     dat<-data.select2()[data.select2()[,"Clustering"]==input$var_grp[i],]
                     titre<-paste("Graphique de flux des",nrow(dat[dat[,input$souspop2]==input$souspop_modalite2[j],]),"individus du groupe",input$var_grp[i],"\n ayant pour la variable",input$souspop2,"la modalité",input$souspop_modalite2[j])
                     graph_flux_grp(data = data.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[j],],seq_data = seq.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[j],],col_periode2(),var_grp = "Clustering",label_grp = as.character(input$var_grp[i]))+ggtitle(titre)
                   }),layout_matrix = ordre()))
                 }
                  })
             },width = 1300,height = haut())
           })
         }
         if(input$souspop2=="Aucune"){
           tailleGraph$height<-dim(ordre())[1]*400
           output$SEQPLOTFLUX <- renderPlot({
            input$graph2
             isolate({
               if(input$souspop2=="Aucune"){
                 req(data.select2(),seq.select2(),col_periode2(),input$var_grp,input$souspop2)
                 return(marrangeGrob(lapply(1:length(input$var_grp), FUN=function(i){
                   graph_flux_grp(data = data.select2(),seq_data = seq.select2(),col_periode2(),var_grp = "Clustering",label_grp = as.character(input$var_grp[i]))
                 }),layout_matrix = ordre()))
               }
                  })
             },width = 1300,height = haut())
         }
         if(input$souspop2!="Aucune" && is.numeric(data2()[,input$souspop2])){
           tailleGraph$height<-dim(ordre())[1]*400
           output$SEQPLOTFLUX <- renderPlot({
             input$graph2
             isolate({
               if(input$souspop2!="Aucune" && is.numeric(data2()[,input$souspop2])){
                 req(data.select2(),seq.select2(),col_periode2(),input$var_grp,input$souspop2)
                 return(marrangeGrob(lapply(1:length(input$var_grp), FUN=function(i){
                   titre<-paste("Graphique de flux des",nrow(data.select2()[data.select2()[,"Clustering"]==input$var_grp[i],]),"individus du groupe",input$var_grp[i],"\n ayant pour la variable",input$souspop2,"une valeur entre",min(input$sous_pop_num2,na.rm = TRUE),"et",max(input$sous_pop_num2,na.rm = TRUE))
                   graph_flux_grp(data = data.select2(),seq_data = seq.select2(),col_periode2(),var_grp = "Clustering",label_grp = as.character(input$var_grp[i]))+ggtitle(titre)
                 }),layout_matrix = ordre()))
               }
             })
           },width = 1300,height = haut())
         }
     }   
   })
     ###### Graphique résidus de Pearson ######
   
     subs<-reactive({
       if (req(input$plottypeG)=="Pearson"){
          req(seq.select2(),dataCluster(),valuesG$df)
          return(seqefsub(seqecreate(seq.select2(), tevent="state", use.labels=FALSE),pmin.support=input$pmin))
       }
       if (req(input$plottypeG) == "Pearson.ch"){
         req(seq.select2(),dataCluster(),valuesG$df)
         if(nrow(valuesG$df)>0){
           unique(c(levels(valuesG$df[,1]),levels(valuesG$df[,2]),levels(valuesG$df[,3])))->valCh
           valCh[valCh!="Aucun"]->valCh
           seqecreate(seq.select2(), tevent="state", use.labels=FALSE)->seqGlobal22
           
           if(all(valCh %in% alphabet(seqGlobal22))){
             vectSeqG<-vect.sous.seq(data = valuesG$df)
             seqefsub(seqGlobal22,str.subseq=vectSeqG)->p22
             return(p22[order(p22$data$Support,decreasing = TRUE),])
           }else{
             valCh[!(valCh %in% alphabet(seqGlobal22))]->valnonalphabet
             valuesG$df<-valuesG$df[which(!(valuesG$df[,1] %in% valnonalphabet | valuesG$df[,2] %in% valnonalphabet | valuesG$df[,3] %in% valnonalphabet)),]
             if(nrow(valuesG$df)>0){
               vectSeqG<-vect.sous.seq(data = valuesG$df)
               seqefsub(seqGlobal22,str.subseq=vectSeqG)->p22
               return(p22[order(p22$data$Support,decreasing = TRUE),])
             }
           }
         }

       }
     })
     
     discr<-reactive({
       if (req(input$plottypeG) %in% c("Pearson","Pearson.ch")){
         req(subs(),data.select2())
         if(nrow(valuesG$df)>0){
         seqecmpgroup(subs() , group=data.select2()[,"Clustering"])
         } 
       }
     })

     output$alpabeltTexte<-renderUI({
       output$TexteAlpha<-renderText({
         if (req(input$plottypeG) == "Pearson.ch"){
           req(seq.select2(),dataCluster(),valuesG$df)
           seqecreate(seq.select2(), tevent="state", use.labels=FALSE)->seqGlobalText
           return(paste("Selectionnez des états se trouvant dans la liste suivante :",paste(alphabet(seqGlobalText),collapse = ", ")))
         }
       })
       return(textOutput("TexteAlpha"))
     })
     
     
  observe({
     req(seq.select2(),dataCluster(),ordre())
     if (req(input$plottypeG) == "Pearson"){
       #Pour la comparaison des sous-populations, on met les graphiques dans une liste#
       if (input$souspop2!="Aucune" && is.factor(data2()[,input$souspop2])) {
         req(input$souspop_modalite2)
         tailleGraph$height<-dim(ordre())[1]*400
         lapply(1:length(input$souspop_modalite2), FUN=function(i){
           paste0('SEQPLOTPEARSON', i)->id.output
           output[[id.output]] <- renderPlot({
             if (req(input$plottypeG) == "Pearson"){
               if (input$souspop2!="Aucune" && is.factor(data2()[,input$souspop2])) {
                 if(i<=length(input$souspop_modalite2)){
                   req(input$nbAffiche)
                   seq.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],]->seqSouspop2
                   seqecreate(seqSouspop2, tevent="state", use.labels=FALSE)->seqGlobal2
                   # titre<-paste("Graphique des sous-séquneces \n pour la variable",input$souspop2,"\n avec la modalité",input$souspop_modalite1)
                   # sousTitre<-paste("Il y a",nrow(seqSouspop),"individus")
                   seqefsub(seqGlobal2,pmin.support=input$pmin)->p2
                   return(plot(seqecmpgroup(p2 , group=data.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],"Clustering"])[1:input$nbAffiche]))
                 }
               }
             } 
            },height = haut(),width = 1300)
         })
       } else {
         output$SEQPLOTPEARSON<-renderPlot({
           if (req(input$plottypeG) == "Pearson"){
             if (input$souspop2=="Aucune" || is.numeric(data2()[,input$souspop2])) {
               req(input$nbAffiche)
               tailleGraph$height<-dim(ordre())[1]*400
               seqecreate(seq.select2(), tevent="state", use.labels=FALSE)->seqGlobal2
               seqefsub(seqGlobal2,pmin.support=input$pmin)->p2
               return(plot(seqecmpgroup(p2 , group=data.select2()[,"Clustering"])[1:input$nbAffiche]))
             }
           }  
         },height = haut(),width = 1300)
         
       }
     }else{
       ## Cas où l'utilisateur choisi les sous-séquences ##
       if (req(input$plottypeG) == "Pearson.ch"){
         req(valuesG$df)
         #condition d'un data.frame values non vide pour exécuter la suite du code afin de ne pas avoir d'erreur quand la data.frame est vide
         if(nrow(valuesG$df)>0){
           unique(c(levels(valuesG$df[,1]),levels(valuesG$df[,2]),levels(valuesG$df[,3])))->valCh
           valCh[valCh!="Aucun"]->valCh
           if (input$souspop2!="Aucune" && is.factor(data2()[,input$souspop2])) {
             req(input$souspop_modalite2)
             tailleGraph$height<-dim(ordre())[1]*400
             lapply(1:length(input$souspop_modalite2), FUN=function(i){
               paste0('SEQPLOTPEARSONCH', i)->id.output
               output[[id.output]] <- renderPlot({
                 if (req(input$plottypeG) == "Pearson.ch"){
                   if (input$souspop2!="Aucune" && is.factor(data2()[,input$souspop2])) {
                     if(i<=length(input$souspop_modalite2)){
                       seq.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],]->seqSouspop2
                       seqecreate(seqSouspop2, tevent="state", use.labels=FALSE)->seqGlobal2
                       if(all(valCh %in% alphabet(seqGlobal2))){
                         if(nrow(valuesG$df)>0){
                           vectSeq2<-vect.sous.seq(data = valuesG$df)
                           seqefsub(seqGlobal2,str.subseq=vectSeq2)->p2
                           return(plot(seqecmpgroup(p2[order(p2$data$Support,decreasing = TRUE),] , group=data.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],"Clustering"])))
                          }
                         
                       }else{
                         valCh[!(valCh %in% alphabet(seqGlobal2))]->valnonalphabet
                         valuesG$df<-valuesG$df[which(!(valuesG$df[,1] %in% valnonalphabet | valuesG$df[,2] %in% valnonalphabet | valuesG$df[,3] %in% valnonalphabet)),]
                         if(nrow(valuesG$df)>0){
                           vectSeq2<-vect.sous.seq(data = valuesG$df)
                           seqefsub(seqGlobal2,str.subseq=vectSeq2)->p2
                           return(plot(seqecmpgroup(p2[order(p2$data$Support,decreasing = TRUE),] , group=data.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],"Clustering"])))
                         }
                       }
                     } 
                   }
                 } 
                },height = haut(),width = 1300)
             })
           } else {
             output$SEQPLOTPEARSONCH<-renderPlot({
               if (req(input$plottypeG) == "Pearson.ch"){
                 if (input$souspop2=="Aucune" || is.numeric(data2()[,input$souspop2])) {
                   if(nrow(valuesG$df)>0){
                     tailleGraph$height<-dim(ordre())[1]*400
                     seqecreate(seq.select2(), tevent="state", use.labels=FALSE)->seqGlobal2
                     vectSeq2<-vect.sous.seq(data = valuesG$df)
                     seqefsub(seqGlobal2,str.subseq=vectSeq2)->p2
                     return(plot(seqecmpgroup(p2[order(p2$data$Support,decreasing = TRUE),] , group=data.select2()[,"Clustering"])))
                   }
                 }   
               }
            },height = haut(),width = 1300)
           }
         }
       }
     }
   })
   
   
   
     observe({
       if(nrow(valuesG$df)>0){
          req(discr())
          updateNumericInput(session = session,inputId = "nbAffiche",max=nrow(discr()$data))
       }else{
         updateNumericInput(session = session,inputId = "nbAffiche",max=1)
       }
     })
     
     
     #### Choix de sous-séqueneces ####

     ### Mise a jour des inputs permettant de choisir des états ###
     observe({
       input$plottypeG
       isolate({
         if (req(input$plottypeG)=="Pearson.ch"){
           req(seq.select2())
           updateSelectInput(session = session,inputId = "par.sous.seq1G",choices = alphabet(seq.select2()))
           updateSelectInput(session = session,inputId = "par.sous.seq2G",choices = alphabet(seq.select2()))
           updateSelectInput(session = session,inputId = "par.sous.seq3G",choices = cbind("Aucun",alphabet(seq.select2())))
         }
       })
     })
     
     observe({
       updateNumericInput(session = session,inputId = "ligne.supprG",max=nrow(valuesG$df))
     })
     
     valuesG <- reactiveValues()
     valuesG$df <-  as.data.frame(setNames(replicate(3,character(0), simplify = F),c("Etat1","Etat2","Etat3") ))
     
     observeEvent(input$add.buttonG,{
       req(input$par.sous.seq1G,input$par.sous.seq2G)
       newRow <- data.frame(input$par.sous.seq1G, input$par.sous.seq2G,input$par.sous.seq3G)
       colnames(newRow)<-colnames(valuesG$df)
       valuesG$df <- rbind(valuesG$df,newRow)
       rownames(valuesG$df)<-(1:nrow(valuesG$df))
     })
     
     observeEvent(input$delete.buttonG,{
       if(nrow(valuesG$df)>1){
         valuesG$df[!(vect.sous.seq(valuesG$df) %in% as.character(subs()$subseq)[input$ligne.supprG]),]->valuesG$df
         rownames(valuesG$df)<-(1:nrow(valuesG$df))
       }else {
         valuesG$df <- valuesG$df[-nrow(valuesG$df), ]
       }
     })  
     observe({
       req(valuesG$df)
       valuesG$df<-unique(valuesG$df)
     })

     #### Pour automatiser la taille des graphiques ####

     ordre<-reactive({
       if(input$plottypeG=="flux"){
         input$graph2
         isolate({
           req(input$var_grp)     
           return(taille_graph_flux(length(input$var_grp)))
         })
       }else ({
         req(grp())     
         return(taille_graph_flux(length(grp())))
       })
     })
     
     haut<-function(){
       ordre2<-ordre()
       return(dim(ordre2)[1]*400)}
     
     tailleGraph<-reactiveValues(height=800)
     
     output$PLOTG <- renderUI({
       
         if (req(input$plottypeG)=="flux"){
           input$graph2
           isolate({
             if (input$souspop2!="Aucune" && is.factor(data2()[,input$souspop2])) {
               req(input$souspop_modalite2,col_periode2(),input$var_grp)
               return(lapply(1:length(input$souspop_modalite2), function(i) {
                 tagList(plotOutput(paste0('SEQPLOTFLUX', i),height = tailleGraph$height),
                fluidRow(column(2,
                                downloadButton(outputId=paste0("DownGraphGrp",i),label="Télécharger les graphiques")
                                ),
                         column(10,
                                hidden(p(id=paste0("texteGraphGrp",i),"Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (.png). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application."))
                         )
                        )
               )
               }))
             }
             else{
               req(col_periode2(),input$var_grp)
               return(tagList(plotOutput("SEQPLOTFLUX",height = tailleGraph$height),
                              fluidRow(column(2,
                                              downloadButton(outputId="DownGraphGrp",label="Télécharger les graphiques")
                                              ),
                                       column(10,
                                              hidden(p(id="texteGraphGrp","Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (.png). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application."))
                                              )
                                       )
               ))
             }

           })

         }

       if (req(input$plottypeG) == "Pearson"){
         if (input$souspop2!="Aucune" && is.factor(data2()[,input$souspop2])) {
             return(lapply(1:length(input$souspop_modalite2), function(i) {
               tagList(plotOutput(paste0('SEQPLOTPEARSON', i),height = tailleGraph$height),
                       fluidRow(column(2,
                                       downloadButton(outputId=paste0("DownGraphGrp",i),label="Télécharger les graphiques")
                       ),
                       column(10,
                              hidden(p(id=paste0("texteGraphGrp",i),"Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (.png). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application."))
                       )
                       )
                    )
             }))
         }else{
           return(tagList(plotOutput("SEQPLOTPEARSON",height = tailleGraph$height),
                          fluidRow(column(2,
                                          downloadButton(outputId="DownGraphGrp",label="Télécharger les graphiques")
                          ),
                          column(10,
                                 hidden(p(id="texteGraphGrp","Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (.png). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application."))
                          )
                          )
                      ))
         }
       }
       if (req(input$plottypeG) == "Pearson.ch"){
         if (input$souspop2!="Aucune" && is.factor(data2()[,input$souspop2])) {
           return(lapply(1:length(input$souspop_modalite2), function(i) {
             tagList(plotOutput(paste0('SEQPLOTPEARSONCH', i),height = tailleGraph$height),
                     fluidRow(column(2,
                                     downloadButton(outputId=paste0("DownGraphGrp",i),label="Télécharger les graphiques")
                     ),
                     column(10,
                            hidden(p(id=paste0("texteGraphGrp",i),"Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (.png). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application."))
                     )
                     )
                    )
           }))
         }else{
           return(tagList(plotOutput("SEQPLOTPEARSONCH",height = tailleGraph$height),
                          fluidRow(column(2,
                                          downloadButton(outputId="DownGraphGrp",label="Télécharger les graphiques")
                          ),
                          column(10,
                                 hidden(p(id="texteGraphGrp","Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (.png). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application."))
                          )
                          )
                        ))
         }
       }
       if(req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r","Ht")){
         req(input$souspop2)
         if (input$souspop2=="Aucune"){
           return(tagList(plotOutput("plotGrp",height = tailleGraph$height),
                          fluidRow(column(2,
                                          downloadButton(outputId="DownGraphGrp",label="Télécharger les graphiques")
                          ),
                          column(10,
                                 hidden(p(id="texteGraphGrp","Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (.png). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application."))
                          )
                          )
                      ))
         }else{
          if (is.factor(data2()[,input$souspop2])) {
            req(input$souspop_modalite2)
          
              return(lapply(1:length(input$souspop_modalite2), function(i) {
                tagList(plotOutput(paste0('SEQPLOT', i),height = tailleGraph$height),
                        fluidRow(column(2,
                                        downloadButton(outputId=paste0("DownGraphGrp",i),label="Télécharger les graphiques")
                        ),
                        column(10,
                               hidden(p(id=paste0("texteGraphGrp",i),"Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (.png). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application."))
                        )
                        )
                    )
              }))
           
          }
          if(is.numeric(data2()[,input$souspop2])){
            return(tagList(plotOutput("plotGrp",height = tailleGraph$height),
                           fluidRow(column(2,
                                           downloadButton(outputId="DownGraphGrp",label="Télécharger les graphiques")
                           ),
                           column(10,
                                  hidden(p(id="texteGraphGrp","Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (.png). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application."))
                           )
                           )
                        ))
          }
         }

         
       }
     })
     ### Création des renderplots pour les graphiques seqplot ###
    
     observe({
       req(seq.select2())
       if(req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r","Ht")){
         if(input$souspop2=="Aucune"){
           output$plotGrp <- renderPlot({
             req(seq.select2(),ordre())
             if(req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r","Ht")){
               if(input$souspop2=="Aucune"){
                 tailleGraph$height<-dim(ordre())[1]*400
                 if(req(input$plottypeG)=="I"){
                   seqplot(seqdata = seq.select2(), type = input$plottypeG, group = data.select2()[,"Clustering"],sortv=input$TapisSortedG)
                 }else{
                   seqplot(seqdata = seq.select2(), type = input$plottypeG, group = data.select2()[,"Clustering"])
                 }
               }
             } 
           },height = haut(),width = 1300)
         }else{
           if (is.factor(data2()[,input$souspop2])) {
             req(input$souspop_modalite2)
             
             lapply(1:length(input$souspop_modalite2), FUN=function(i){
               paste0('SEQPLOT', i)->id.output
               output[[id.output]] <- renderPlot({
                 req(seq.select2(),ordre())
                 if(req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r","Ht")){
                   if(input$souspop2!="Aucune" && is.factor(data2()[,input$souspop2])){
                     if(i<=length(input$souspop_modalite2)){
                       tailleGraph$height<-dim(ordre())[1]*400
                       if(req(input$plottypeG)=="I"){
                         seqplot(seqdata = seq.select2()[which(data.select2()[,input$souspop2]==input$souspop_modalite2[i]),], type = input$plottypeG, group = data.select2()[which(data.select2()[,input$souspop2]==input$souspop_modalite2[i]),"Clustering"],main = paste("Graphique de la variable",input$souspop2,"avec la modalité",input$souspop_modalite2[i]),sortv=input$TapisSortedG)
                       }else{
                         seqplot(seqdata = seq.select2()[which(data.select2()[,input$souspop2]==input$souspop_modalite2[i]),], type = input$plottypeG, group = data.select2()[which(data.select2()[,input$souspop2]==input$souspop_modalite2[i]),"Clustering"],main = paste("Graphique de la variable",input$souspop2,"avec la modalité",input$souspop_modalite2[i]))
                       }
                     }
                   }
                 } 
                 },height = haut(),width = 1300)
             })
           
           }
           if(is.numeric(data2()[,input$souspop2])){
             output$plotGrp<-renderPlot({
               req(seq.select2(),ordre(),input$sous_pop_num2)
               if (req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r","Ht")){
                 if(input$souspop2!="Aucune" && is.numeric(data2()[,input$souspop2])){
                   tailleGraph$height<-dim(ordre())[1]*400
                   titre<-paste("Graphique de la variable",input$souspop2,"entre",min(input$sous_pop_num2,na.rm = TRUE),"et",max(input$sous_pop_num2,na.rm = TRUE))
                   if(req(input$plottypeG)=="I"){
                     seqplot(seqdata = seq.select2(), type = input$plottypeG, group = data.select2()[,"Clustering"],main = titre,sortv=input$TapisSortedG)
                   }else{
                     seqplot(seqdata = seq.select2(), type = input$plottypeG, group = data.select2()[,"Clustering"],main = titre)
                   }
                 }
               } 
               },height = haut(),width = 1300)
           }
         }
       }
     })
     
     #### Téléchargement des graphiques ####

     ### Seqplot ###
     observe({
       if(req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r","Ht")){
         req(input$souspop2,seq.select2(),data.select2(),ordre())
         if (input$souspop2!="Aucune" && is.factor(data2()[,input$souspop2])) {
           lapply(1:length(input$souspop_modalite2), FUN=function(i){
             if(i<=length(input$souspop_modalite2)){
               fonctionseqplot<-function(){
                 req(input$souspop_modalite2,seq.select2(),ordre())
                 if(req(input$plottypeG)=="I"){
                   return(seqplot(seqdata = seq.select2()[which(data.select2()[,input$souspop2]==input$souspop_modalite2[i]),], type = input$plottypeG, group = data.select2()[which(data.select2()[,input$souspop2]==input$souspop_modalite2[i]),"Clustering"],main = paste("Graphique de la variable",input$souspop2,"avec la modalité",input$souspop_modalite2[i]),sortv=input$TapisSortedG))
                 }else{
                   return(seqplot(seqdata = seq.select2()[which(data.select2()[,input$souspop2]==input$souspop_modalite2[i]),], type = input$plottypeG, group = data.select2()[which(data.select2()[,input$souspop2]==input$souspop_modalite2[i]),"Clustering"],main = paste("Graphique de la variable",input$souspop2,"avec la modalité",input$souspop_modalite2[i])))
                 }
               }
               idDownloadHandler<-paste0("DownGraphGrp",i)
               output[[idDownloadHandler]] <- downloadHandler(
                   filename = function(){
                     paste0(input$plottypeG,input$souspop2,input$souspop_modalite2[i],".png")
                   },
                   content = function(file){
                     shiny::withProgress(
                       message = "Veuillez patienter, le téléchargement de votre graphique est en cours",
                       value = 0,
                       {
                         png(file,height = haut(),width = 1300)
                         fonctionseqplot()
                         dev.off()
                         shiny::incProgress(1)
                       })
                   },
                   contentType = 'image/png'
                 )
             }
           })
           
         }else{
           if(input$souspop2=="Aucune"){
            fonctionseqplot<-function(){
              req(seq.select2(),ordre())
               if(req(input$plottypeG)=="I"){
                 return(seqplot(seqdata = seq.select2(), type = input$plottypeG, group = data.select2()[,"Clustering"],sortv=input$TapisSortedG))
               }else{
                 return(seqplot(seqdata = seq.select2(), type = input$plottypeG, group = data.select2()[,"Clustering"]))
               }
             }
            }else{
               if(input$souspop2!="Aucune" && is.numeric(data2()[,input$souspop2])){
                 fonctionseqplot<-function(){
                 req(seq.select2(),ordre(),input$sous_pop_num2)
                 titre<-paste("Graphique de la variable",input$souspop2,"entre",min(input$sous_pop_num2,na.rm = TRUE),"et",max(input$sous_pop_num2,na.rm = TRUE))
                 if(req(input$plottypeG)=="I"){
                   return(seqplot(seqdata = seq.select2(), type = input$plottypeG, group = data.select2()[,"Clustering"],main = titre,sortv=input$TapisSortedG))
                 }else{
                   return(seqplot(seqdata = seq.select2(), type = input$plottypeG, group = data.select2()[,"Clustering"],main = titre))
                 }
               }
             }
           }
           output$DownGraphGrp <- downloadHandler(
             filename = function(){
               paste0(input$plottypeG,input$souspop2,".png")
             },
             content = function(file){
               shiny::withProgress(
                 message = "Veuillez patienter, le téléchargement de votre graphique est en cours",
                 value = 0,
                 {
                 png(file,height = haut(),width = 1300)
                 fonctionseqplot()
                 dev.off()
                 shiny::incProgress(1)
                })
             },
             contentType = 'image/png'
           )
         }
       }
     })
     
     ### Graphique de flux ###
     observe({
       if(input$plottypeG=="flux"){
         req(input$souspop2,seq.select2(),data.select2(),ordre())
         if (input$souspop2!="Aucune" && is.factor(data2()[,input$souspop2])) {
           req(input$souspop_modalite2,input$souspop2,seq.select2(),data.select2(),ordre())
           lapply(1:length(input$souspop_modalite2), FUN=function(j){
             fonctionflux<-function(){
               req(data.select2(),seq.select2(),col_periode2(),input$var_grp,input$souspop2,input$souspop_modalite2)
               return(marrangeGrob(lapply(1:length(input$var_grp), FUN=function(i){
                 dat<-data.select2()[data.select2()[,"Clustering"]==input$var_grp[i],]
                 titre<-paste("Graphique de flux des",nrow(dat[dat[,input$souspop2]==input$souspop_modalite2[j],]),"individus du groupe",input$var_grp[i],"\n ayant pour la variable",input$souspop2,"la modalité",input$souspop_modalite2[j])
                 graph_flux_grp(data = data.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[j],],seq_data = seq.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[j],],col_periode2(),var_grp = "Clustering",label_grp = as.character(input$var_grp[i]))+ggtitle(titre)
               }),layout_matrix = ordre()))
             }
             idDownloadHandler<-paste0("DownGraphGrp",j)
             output[[idDownloadHandler]] <- downloadHandler(
               filename = function(){
                 paste0(input$plottypeG,input$souspop2,input$souspop_modalite2[j],".png")
               },
               content = function(file){
                 shiny::withProgress(
                   message = "Veuillez patienter, le téléchargement de votre graphique est en cours",
                   value = 0,
                   {
                     ggsave(file,plot=fonctionflux(),height =7.5*dim(ordre())[1],width = 20,device = png())
                     dev.off()
                     shiny::incProgress(1)
                   })
               }
             )
           })
         }else{
           fonctionflux<-function(){
             if(input$souspop2=="Aucune"){
                 req(data.select2(),seq.select2(),col_periode2(),input$var_grp,input$souspop2,ordre())
                 return(marrangeGrob(lapply(1:length(input$var_grp), FUN=function(i){
                   graph_flux_grp(data = data.select2(),seq_data = seq.select2(),col_periode2(),var_grp = "Clustering",label_grp = as.character(input$var_grp[i]))
                 }),layout_matrix = ordre()))
             }
             if(input$souspop2!="Aucune" && is.numeric(data2()[,input$souspop2])){
                 req(data.select2(),seq.select2(),col_periode2(),input$var_grp,input$souspop2,ordre())
                 return(marrangeGrob(lapply(1:length(input$var_grp), FUN=function(i){
                   titre<-paste("Graphique de flux des",nrow(data.select2()[data.select2()[,"Clustering"]==input$var_grp[i],]),"individus du groupe",input$var_grp[i],"\n ayant pour la variable",input$souspop2,"une valeur entre",min(input$sous_pop_num2,na.rm = TRUE),"et",max(input$sous_pop_num2,na.rm = TRUE))
                   graph_flux_grp(data = data.select2(),seq_data = seq.select2(),col_periode2(),var_grp = "Clustering",label_grp = as.character(input$var_grp[i]))+ggtitle(titre)
                 }),layout_matrix = ordre()))
             }
           }
           output$DownGraphGrp <- downloadHandler(
             filename = function(){
               paste0(input$plottypeG,input$souspop2,".png")
             },
             content = function(file){
               shiny::withProgress(
                 message = "Veuillez patienter, le téléchargement de votre graphique est en cours",
                 value = 0,
                 {
                   ggsave(file,plot=fonctionflux(),height =7.5*dim(ordre())[1],width = 20,device = png())
                   dev.off()
                   shiny::incProgress(1)
                 })
             }
           )
         }
         
        }
     })
     
     ### Sous-séquences Pearson ###
     observe({
       req(seq.select2(),dataCluster(),ordre())
       if (req(input$plottypeG) == "Pearson"){
         if (input$souspop2!="Aucune" && is.factor(data2()[,input$souspop2])) {
           req(input$souspop_modalite2)
           lapply(1:length(input$souspop_modalite2), FUN=function(i){
                  if(i<=length(input$souspop_modalite2)){
                    fonctionPearson<-function(){
                     req(input$nbAffiche)
                     seq.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],]->seqSouspop2
                     seqecreate(seqSouspop2, tevent="state", use.labels=FALSE)->seqGlobal2
                     seqefsub(seqGlobal2,pmin.support=input$pmin)->p2
                     return(plot(seqecmpgroup(p2 , group=data.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],"Clustering"])[1:input$nbAffiche]))
                    }
                    idDownloadHandler<-paste0("DownGraphGrp",i)
                    output[[idDownloadHandler]] <- downloadHandler(
                      filename = function(){
                        paste0(input$plottypeG,input$souspop2,input$souspop_modalite2[i],".png")
                      },
                      content = function(file){
                        shiny::withProgress(
                          message = "Veuillez patienter, le téléchargement de votre graphique est en cours",
                          value = 0,
                          {
                            png(file,height = haut(),width = 1300)
                            fonctionPearson()
                            dev.off()
                            shiny::incProgress(1)
                          })
                      },
                      contentType = 'image/png'
                    )
                  }
           })
         } else {
               if (input$souspop2=="Aucune" || is.numeric(data2()[,input$souspop2])) {
                 fonctionPearson<-function(){
                   req(input$nbAffiche)
                   seqecreate(seq.select2(), tevent="state", use.labels=FALSE)->seqGlobal2
                   seqefsub(seqGlobal2,pmin.support=input$pmin)->p2
                   return(plot(seqecmpgroup(p2 , group=data.select2()[,"Clustering"])[1:input$nbAffiche]))
                 }
                 output$DownGraphGrp <- downloadHandler(
                   filename = function(){
                     paste0(input$plottypeG,input$souspop2,".png")
                   },
                   content = function(file){
                     shiny::withProgress(
                       message = "Veuillez patienter, le téléchargement de votre graphique est en cours",
                       value = 0,
                       {
                         png(file,height = haut(),width = 1300)
                         fonctionPearson()
                         dev.off()
                         shiny::incProgress(1)
                       })
                   },
                   contentType = 'image/png'
                 )
              }
         }
       }
         ## Cas où l'utilisateur choisi les sous-séquences ##
         if (req(input$plottypeG) == "Pearson.ch"){
           req(valuesG$df)
           if(nrow(valuesG$df)>0){
             unique(c(levels(valuesG$df[,1]),levels(valuesG$df[,2]),levels(valuesG$df[,3])))->valCh
             valCh[valCh!="Aucun"]->valCh
             if (input$souspop2!="Aucune" && is.factor(data2()[,input$souspop2])) {
               req(input$souspop_modalite2)
               lapply(1:length(input$souspop_modalite2), FUN=function(i){
                       if(i<=length(input$souspop_modalite2)){
                         fonctionPearsonCh<-function(){
                           seq.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],]->seqSouspop2
                           seqecreate(seqSouspop2, tevent="state", use.labels=FALSE)->seqGlobal2
                           if(all(valCh %in% alphabet(seqGlobal2))){
                             if(nrow(valuesG$df)>0){
                               vectSeq2<-vect.sous.seq(data = valuesG$df)
                               seqefsub(seqGlobal2,str.subseq=vectSeq2)->p2
                               return(plot(seqecmpgroup(p2[order(p2$data$Support,decreasing = TRUE),] , group=data.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],"Clustering"])))
                             } 
                           }else{
                             valCh[!(valCh %in% alphabet(seqGlobal2))]->valnonalphabet
                             valuesG$df<-valuesG$df[which(!(valuesG$df[,1] %in% valnonalphabet | valuesG$df[,2] %in% valnonalphabet | valuesG$df[,3] %in% valnonalphabet)),]
                             if(nrow(valuesG$df)>0){
                               vectSeq2<-vect.sous.seq(data = valuesG$df)
                               seqefsub(seqGlobal2,str.subseq=vectSeq2)->p2
                               return(plot(seqecmpgroup(p2[order(p2$data$Support,decreasing = TRUE),] , group=data.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],"Clustering"])))
                             }
                           }
                         }
                         idDownloadHandler<-paste0("DownGraphGrp",i)
                         output[[idDownloadHandler]] <- downloadHandler(
                           filename = function(){
                             paste0(input$plottypeG,input$souspop2,input$souspop_modalite2[i],".png")
                           },
                           content = function(file){
                             shiny::withProgress(
                               message = "Veuillez patienter, le téléchargement de votre graphique est en cours",
                               value = 0,
                               {
                                 png(file,height = haut(),width = 1300)
                                 fonctionPearsonCh()
                                 dev.off()
                                 shiny::incProgress(1)
                               })
                           },
                           contentType = 'image/png'
                         )
                       } 
               })
             } else {
                   if (input$souspop2=="Aucune" || is.numeric(data2()[,input$souspop2])) {
                     fonctionPearsonCh<-function(){
                       if(nrow(valuesG$df)>0){
                         tailleGraph$height<-dim(ordre())[1]*400
                         seqecreate(seq.select2(), tevent="state", use.labels=FALSE)->seqGlobal2
                         vectSeq2<-vect.sous.seq(data = valuesG$df)
                         seqefsub(seqGlobal2,str.subseq=vectSeq2)->p2
                         return(plot(seqecmpgroup(p2[order(p2$data$Support,decreasing = TRUE),] , group=data.select2()[,"Clustering"])))
                       }
                     }
                     output$DownGraphGrp <- downloadHandler(
                       filename = function(){
                         paste0(input$plottypeG,input$souspop2,".png")
                       },
                       content = function(file){
                         shiny::withProgress(
                           message = "Veuillez patienter, le téléchargement de votre graphique est en cours",
                           value = 0,
                           {
                             png(file,height = haut(),width = 1300)
                             fonctionPearsonCh()
                             dev.off()
                             shiny::incProgress(1)
                           })
                       },
                       contentType = 'image/png'
                     )
                   }  
             }
           }
         }
     })
     
     
     observe({
        req(dataCluster(),input$souspop2)
       if (input$souspop2=="Aucune" || is.numeric(data2()[,input$souspop2])) {
         onevent("mouseleave",'DownGraphGrp', hide("texteGraphGrp"))
         onevent("mouseenter", 'DownGraphGrp', show("texteGraphGrp"))
       }
       if (input$souspop2!="Aucune" && is.factor(data2()[,input$souspop2])) {
         lapply(1:length(input$souspop_modalite2), function(i) {
           onevent("mouseleave", paste0('DownGraphGrp',i), hide(paste0("texteGraphGrp",i)))
           onevent("mouseenter", paste0('DownGraphGrp',i), show(paste0("texteGraphGrp",i)))
         })
       }
     })
     
     #### Texte rappelant la sous-population choisie ####
     
     reactive({
       if (req(input$plottypeG)=="flux"){
         input$graph2
         isolate({
           p<-paste("Vous avez sélectionné les groupes",paste(input$var_grp,collapse = ", "))
           if (input$souspop2=="Aucune" || input$souspop2==""){
             return(paste(p,"et aucune sous population"))
           }else{
             
             if (is.factor(data2()[,input$souspop2])){
               req(input$souspop_modalite2)
               return(paste(p,"et la sous population",input$souspop2, "avec les modalités",paste(input$souspop_modalite2,collapse = ", ")))
             }
             if (is.numeric(data2()[,input$souspop2])){
               
               return(paste(p,"et la sous population",input$souspop2, "entre",min(input$sous_pop_num2,na.rm=TRUE),"et",max(input$sous_pop_num2,na.rm=TRUE)))
               
             }
           }
         })
       }
       
       else {
         if (input$souspop2=="Aucune" || input$souspop2==""){
           return("Vous avez selectionné aucune sous population")
         }else{
           
           if (is.factor(data2()[,input$souspop2])){
             req(input$souspop_modalite2)
             return(paste("Vous avez selectionné la sous population",input$souspop2, "avec les modalités",paste(input$souspop_modalite2,collapse = ", ")))
           }
           if (is.numeric(data2()[,input$souspop2])){
             
             return(paste("Vous avez selectionné la sous population",input$souspop2, "entre",min(input$sous_pop_num2,na.rm=TRUE),"et",max(input$sous_pop_num2,na.rm=TRUE)))
             
           }
         }
       }
     })->text2
     
     
     renderUI({
       req(text2())
       renderText(text2())->output$textGRP
       h4(textOutput("textGRP"))
     })->output$h4_fluxGrp
     
     # Texte expliquant les graphiques
     output$TexteGraphGrp<-renderText({
       if (input$plottypeG=="d"){
         return("Le chronogramme représente la proportion d'individus (ou autres unités statistiques) à chaque pas de temps dans les différentes situations.")
       }
       if (input$plottypeG=="f"){
         return("Le graphique montre les séquences les plus réprésentées dans les données avec le pourcentage correspondant.")
       }
       if (input$plottypeG=="I"){
         return("Le tapis représente la séquence de chacun des individus(ou autres unités statistiques). Une ligne correspond à un individu(ou autre unité statistique).")
       }
       if (input$plottypeG=="ms"){
         return("Le graphique montre la situation la plus représentée pour chaque période, avec la proportion correpondante en ordonnée.")
       }
       if (input$plottypeG=="mt"){
         return("Le graphique représente le nombre de périodes moyennes pour chacune des situations.")
       }
       if (input$plottypeG=="Ht"){
         return("Le graphique permet de mesurer l’uniformité, ou non, d’une distribution. Une entropie faible, proche de 0 (forte, proche de 1) marque une forte (faible) uniformité des situations à chaque pas de temps.")
       }
       if (input$plottypeG=="flux"){
         return("Le graphique de flux montre la répartiton de chaucune des situations à chaque période (rectangles proportionnels). Le graphique permet de visualiser également la part des individus(ou autres unités statistiques) changeant de situations (zone entre les deux périodes).")
       }
       if (input$plottypeG == "Pearson"){
         return("Le graphique affiche les sous-séquences sur-représentées (ou sous-représentées) quand les résidus de Pearson sont positifs (ou négatifs).")
       }
       if (input$plottypeG == "Pearson.ch"){
         return("Le graphique affiche les sous-séquences choisies avec les résidus de Pearson.Les sous-séquences sont sur-représentées (ou sous-représentées) quand les résidus de Pearson sont positifs (ou négatifs).")
       }
     })
     
     #Ce graphique permet de mesurer l’uniformité, ou non, d’une distribution. Une entropie faible, proche de 0 (forte, proche de 1) marque une forte (faible) uniformité des situations à chaque pas de temps.
     #### Statistiques descriptives Groupes ####

     ##### Mise a jour des inputs #####
     observeEvent(eventExpr = input$Bouton_Clustering,{
       #sous population
       colsouspop2<-colnames(data2())[!(colnames(data2()) %in% input$timecol)]
       updateSelectInput(session = session, inputId = "souspop2StatDesc", choices = c("Aucune",colsouspop2))
       
     })
     
     ### Tableau des profils lignes ###
     
     output$outils<-renderUI({
         if (input$souspop2StatDesc=="Aucune"){
           shiny::radioButtons(inputId = "TypeGraph", label = "Graphique",choices=c("Effectif","Pourcentage"))
         }
     })
     
     # Tableau des effectifs et proportions dans chaque groupe
     tableEffectifR<-reactive({
       if (input$souspop2StatDesc=="Aucune"){
         cbind(summary(dataCluster()[,"Clustering"]),summary(dataCluster()[,"Clustering"])/nrow(dataCluster()))->tableEffectif
         colnames(tableEffectif)<-c("Effectif","Proportion")
         apply(tableEffectif,2,sum)->totGlobal #Rajout de la ligne Global
         rbind(tableEffectif,totGlobal)->tableEffectif
         rownames(tableEffectif)[dim(tableEffectif)[1]]<-"Global"
         as.integer(tableEffectif[,1])->tableEffectif[,1] # Pour enlever les décimales dans la colonne effectif
         return(tableEffectif)
       }
     })
     
     
     
     ## Cas des variables numériques
     
     #Ajout de inputs
     output$UIVarNumStatDesc<-renderUI({
       req(dataCluster())
       if(input$souspop2StatDesc!="Aucune" && is.numeric(data2()[,input$souspop2StatDesc])){
         return(tagList(shiny::selectInput(inputId = "ClassBreaks",label = "Méthode de découpage en classe",choices = c("Quantile"="quantile","Egaux"="equal","Jolie rupture"="pretty","Manuel"="fixed")),
                        shiny::uiOutput("nbClasses"),
                        uiOutput("infobulleBornes"),
                        hidden(shiny::textInput(inputId = "Breaks",label=NULL))
                        ))
       }
     })
     
     #On enlève les NA pour discrétiser
     dataSansNA<-reactive({
       req(dataCluster(),input$souspop2StatDesc)
       if(input$souspop2StatDesc!="Aucune" && is.numeric(data2()[,input$souspop2StatDesc])){
         dataCluster()[which(!is.na(dataCluster()[,input$souspop2StatDesc])),]
       }
     })
     
     # input pour la discrétisation avec un nombre de classes donné
     output$nbClasses<-renderUI({
       req(dataSansNA())
       if(input$souspop2StatDesc!="Aucune" && is.numeric(data2()[,input$souspop2StatDesc])){
         if(input$ClassBreaks %in% c("quantile","equal","pretty")){
           #)Methode donnant un nombre de classes selon les effectifs
           nb<-nclass.Sturges(dataSansNA()[,input$souspop2StatDesc])
           max<-length(unique(dataSansNA()[,input$souspop2StatDesc]))
           if (nb>max){
             nb<-max-1
           }
           shiny::numericInput(inputId = "NbClass",label="Nombre de classes",min = 2,value=nb,max = max)
         }
       }
     })
     
     #Mis à jour de l'input permettant de choisir les bornes pour avoir un exemple lorsque l'utilisateur choisira le mode manuel
     observe({
       req(input$NbClass,dataSansNA())
       if(input$souspop2StatDesc!="Aucune" && is.numeric(data2()[,input$souspop2StatDesc])){
         if(input$ClassBreaks %in% c("quantile","equal","pretty")){
           classIntervals(dataSansNA()[,input$souspop2StatDesc], n=input$NbClass, style=input$ClassBreaks)->classVar
           updateTextInput(session = session,inputId = "Breaks",value = classVar$brks )
         }
       }
     })
     
     #Affiche la zone de texte que lorsque le mode manuel est choisi
     observe({
       req(dataSansNA(),input$ClassBreaks,input$Breaks)
       if(input$souspop2StatDesc!="Aucune" && is.numeric(data2()[,input$souspop2StatDesc])){
         if (input$ClassBreaks =="fixed"){
           show("Breaks")
         }else{
           hide("Breaks")
         }
       }
     })
     
     #Infobulle pour l'input texte lors du mode manuel
     output$infobulleBornes<-renderUI({
       req(dataSansNA())
       if(input$souspop2StatDesc!="Aucune" && is.numeric(data2()[,input$souspop2StatDesc])){
         if (input$ClassBreaks =="fixed"){
           span("Classes",popify(el = icon(name = "info-circle", lib = "font-awesome"),trigger="click",placement = "right",title = "Bornes des classes" ,content = "<p>Spécifiez les <strong>bornes des classes</strong> voulues en les séparant par des <strong>virgules</strong>.</p> <p>Pour mettre un <strong>nombre décimal</strong>, mettez un <strong>point</strong></p>"))
         }
       }
     })
     
     # Création des bornes
     BreaksClassInterval<-reactive({
       req(input$NbClass,dataSansNA(),input$ClassBreaks)
       if(input$souspop2StatDesc!="Aucune" && is.numeric(data2()[,input$souspop2StatDesc])){
         if(input$ClassBreaks %in% c("quantile","equal","pretty")){
           return(classIntervals(dataSansNA()[,input$souspop2StatDesc], style=input$ClassBreaks,n=input$NbClass))
         }else{
           if(input$ClassBreaks =="fixed"){
             as.vector(as.numeric(strsplit(input$Breaks,",")[[1]]))->BreakCh
             if(length(BreakCh)>1){
               return(classIntervals(dataSansNA()[,input$souspop2StatDesc], style=input$ClassBreaks,fixedBreaks=BreakCh))
             }
           }
         }
       }
     })
     
     ### Affichage des tableaux et graphiques 
     output$profilLigne<-renderUI({
       req(dataCluster())
       # Variable de type facteur
       if (input$souspop2StatDesc!="Aucune" && is.factor(data2()[,input$souspop2StatDesc])){
         output$tableauProfilLigne <- renderDataTable({
           if (input$souspop2StatDesc!="Aucune" && is.factor(data2()[,input$souspop2StatDesc])){
             tableau_ligne(data = dataCluster(),var_grp = "Clustering",var = input$souspop2StatDesc)
           }
         })
         output$testChi<-renderPrint({
           req(dataCluster(),input$souspop2StatDesc)
           if (input$souspop2StatDesc!="Aucune" && is.factor(data2()[,input$souspop2StatDesc])){
             chisq.test(table(dataCluster()[,"Clustering"],dataCluster()[,input$souspop2StatDesc]))->chi
             if (any(chi$expected<5)){
               return("Les effetifs théoriques ne sont pas tous supérieurs à 5")
             }else{
               return(chi)
             }
           }
         })
         output$tabEffectifGrpF<-renderDataTable({
           if (input$souspop2StatDesc!="Aucune" && is.factor(data2()[,input$souspop2StatDesc])){
             addmargins(table(dataCluster()[,"Clustering"],dataCluster()[,input$souspop2StatDesc]))->TabEffFacteur
             dim(TabEffFacteur)[1]->NumGlobal
             rownames(TabEffFacteur)[NumGlobal]<-"Global"
             colnames(TabEffFacteur)[dim(TabEffFacteur)[2]]<-"Global"
             return(as.data.frame.matrix(TabEffFacteur))
           }
         })
         output$graphStatDescGrpF<-renderPlot({
           if (input$souspop2StatDesc!="Aucune" && is.factor(data2()[,input$souspop2StatDesc])){
             table(dataCluster()[,"Clustering"],dataCluster()[,input$souspop2StatDesc])->dataplot
             return(barplot(dataplot,beside=T,legend=rownames(dataplot),main="Les effectifs par groupe et par modalité"))
           }
         })
         return(tagList(tags$h4("Profils lignes"),
                        dataTableOutput("tableauProfilLigne"),
                        downloadButton("DownloadStatDescProfLig","Télécharger le tableau"),
                        hidden(p(id="texteDownloadPL",paste0("Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (",input$TypeFichierStatDesc," ). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application."))),
                        tags$p("La valeur en vert (en rouge) correspond à une modalité surresprésentée (sous-représentée) dans le groupe"),
                        verbatimTextOutput("testChi"),
                        dataTableOutput("tabEffectifGrpF"),
                        downloadButton("DownloadStatDescEffetif","Télécharger le tableau"),
                        hidden(p(id="texteDownloadEffTab",paste0("Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (",input$TypeFichierStatDesc," ). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application."))),
                        plotOutput("graphStatDescGrpF"),
                        fluidRow(
                          column(2,downloadButton(outputId="DownGraphStatDesc",label="Télécharger le graphique")),
                          column( 10,hidden(p(id="texteDownloadEffGraph","Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (.png). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application.")))
                        )
    
         ))
       }
       # Aucune sous population
       if (input$souspop2StatDesc=="Aucune"){
         output$tableEff<-renderDataTable({
           if (input$souspop2StatDesc=="Aucune"){
             req(tableEffectifR())
             formatPercentage(datatable(tableEffectifR()),columns = 2,digits = 2)
           }
         })
         output$GraphEffectiGrp<-renderPlot({
           if (input$souspop2StatDesc=="Aucune"){
             req(tableEffectifR())
             if (input$TypeGraph=="Effectif"){
               return(barplot(tableEffectifR()[1:(dim(tableEffectifR())[1]-1),1],main = "Répartition des effectifs par groupe"))
             }
             if (input$TypeGraph=="Pourcentage"){
               return(barplot(tableEffectifR()[1:(dim(tableEffectifR())[1]-1),2],main = "Répartition en pourcentage des effectifs par groupe"))
             }
           }
         })
         return(tagList(tags$h4("Effectif pour chaque groupe"),
                        dataTableOutput("tableEff"),
                        downloadButton("DownloadStatDescEffetif","Télécharger le tableau"),
                        hidden(p(id="texteDownloadEffTab",paste0("Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (",input$TypeFichierStatDesc," ). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application."))),
                        plotOutput("GraphEffectiGrp"),
                        fluidRow(
                          column(2, downloadButton(outputId="DownGraphStatDesc",label="Télécharger le graphique")),
                          column(10,hidden(p(id="texteDownloadEffGraph","Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (.png). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application.")))
                        )
    
         ))
       }
       # Variable numérique
       if (input$souspop2StatDesc!="Aucune" && is.numeric(data2()[,input$souspop2StatDesc])) {
         output$tableauProfilLigne <- renderDataTable({
           req(BreaksClassInterval())
           if (input$souspop2StatDesc!="Aucune" && is.numeric(data2()[,input$souspop2StatDesc])) {
             dataSansNA()->dataClusterDisc
             cut(dataSansNA()[,input$souspop2StatDesc],breaks=unique(BreaksClassInterval()$brks),include.lowest = TRUE)->dataClusterDisc$DiscVarNum
             tableau_ligne(data = dataClusterDisc,var_grp = "Clustering",var = "DiscVarNum")
           }
         })
         
         output$testChi<-renderPrint({
           req(BreaksClassInterval())
           if (input$souspop2StatDesc!="Aucune" && is.numeric(data2()[,input$souspop2StatDesc])) {
             chisq.test(table(dataSansNA()[,"Clustering"],cut(dataSansNA()[,input$souspop2StatDesc],breaks=unique(BreaksClassInterval()$brks),include.lowest = TRUE)))->chi
             if (any(chi$expected<5)){
               return("Les effetifs théoriques ne sont pas tous supérieurs à 5")
             }else{
               return(chi)
             }
           }
         })
         
         output$tableEffGrpNum<-renderDataTable({
           req(BreaksClassInterval())
           if (input$souspop2StatDesc!="Aucune" && is.numeric(data2()[,input$souspop2StatDesc])) {
             addmargins(table(dataSansNA()[,"Clustering"],cut(dataSansNA()[,input$souspop2StatDesc],breaks=unique(BreaksClassInterval()$brks),include.lowest = TRUE)))->TabEffNum
             dim(TabEffNum)[1]->NumGlobal
             rownames(TabEffNum)[NumGlobal]<-"Global"
             colnames(TabEffNum)[dim(TabEffNum)[2]]<-"Global"
             return(as.data.frame.matrix(TabEffNum))
           }
         })
         
         output$barplotClass<-renderPlot({
           req(BreaksClassInterval())
           if (input$souspop2StatDesc!="Aucune" && is.numeric(data2()[,input$souspop2StatDesc])) {
             table(dataSansNA()[,"Clustering"],cut(dataSansNA()[,input$souspop2StatDesc],breaks=unique(BreaksClassInterval()$brks),include.lowest = TRUE))->tabNum
             return(barplot(tabNum,beside=TRUE,legend=rownames(tabNum),main="Les effectifs par classe"))
           }
         })
         return(tagList(tags$h4("Profils lignes"),
                        dataTableOutput("tableauProfilLigne"),
                        downloadButton("DownloadStatDescProfLig","Télécharger le tableau"),
                        hidden(p(id="texteDownloadPL",paste0("Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (",input$TypeFichierStatDesc," ). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application."))),
                        tags$p("La valeur en vert (en rouge) correspond à une classe surresprésentée (sous-représentée) dans le groupe"),
                        verbatimTextOutput("testChi"),
                        dataTableOutput("tableEffGrpNum"),
                        downloadButton("DownloadStatDescEffetif","Télécharger le tableau"),
                        hidden(p(id="texteDownloadEffTab",paste0("Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (",input$TypeFichierStatDesc," ). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application."))),
                        plotOutput("barplotClass"),
                        fluidRow(
                          column(2,downloadButton(outputId="DownGraphStatDesc",label="Télécharger le graphique")),
                          column(10,hidden(p(id="texteDownloadEffGraph","Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (.png). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application.")))
                        ) 
                        
                        ))
       }
     })
      
    ###################### Téléchargement ##############################
                         ### Graphique ###
     fonctionGraphStatDesc<- function(){
       if (input$souspop2StatDesc!="Aucune" && is.factor(data2()[,input$souspop2StatDesc])){
         req(dataCluster())
         table(dataCluster()[,"Clustering"],dataCluster()[,input$souspop2StatDesc])->dataplot
         return(barplot(dataplot,beside=T,legend=rownames(dataplot),main="Les effectifs par groupe et par modalité"))
       }
       if (input$souspop2StatDesc!="Aucune" && is.numeric(data2()[,input$souspop2StatDesc])) {
         req(dataSansNA(),BreaksClassInterval())
         table(dataSansNA()[,"Clustering"],cut(dataSansNA()[,input$souspop2StatDesc],breaks=unique(BreaksClassInterval()$brks),include.lowest = TRUE))->tabNum
         return(barplot(tabNum,beside=TRUE,legend=rownames(tabNum),main="Les effectifs par classe"))
       }
       if (input$souspop2StatDesc=="Aucune"){
         req(tableEffectifR())
         if (input$TypeGraph=="Effectif"){
           return(barplot(tableEffectifR()[1:(dim(tableEffectifR())[1]-1),1],main = "Répartition des effectifs par groupe"))
         }
         if (input$TypeGraph=="Pourcentage"){
           return(barplot(tableEffectifR()[1:(dim(tableEffectifR())[1]-1),2],main = "Répartition en pourcentage des effectifs par groupe"))
         }
       }
     }

     largeurGraphStatDesc<-function(){
       if (input$souspop2StatDesc!="Aucune" && is.factor(data2()[,input$souspop2StatDesc])){
         return(session$clientData$output_graphStatDescGrpF_width)
       }
       if (input$souspop2StatDesc!="Aucune" && is.numeric(data2()[,input$souspop2StatDesc])) {
         return(session$clientData$output_barplotClass_width)
       }
       if (input$souspop2StatDesc=="Aucune"){
         return(session$clientData$output_GraphEffectiGrp_width)
       }
     }
     
     output$DownGraphStatDesc <- downloadHandler(
       filename =  function() {
         paste0(input$souspop2StatDesc, "StatDesc.png")
       },
       # content is a function with argument file. content writes the plot to the device
       content = function(file) {
         shiny::withProgress(
           message = "Veuillez patienter, le téléchargement de votre graphique est en cours",
           value = 0,
           {png(file,width = largeurGraphStatDesc(),height = 400) # open the png device
               # draw the plot
             fonctionGraphStatDesc()
             dev.off()  # turn the device off
             shiny::incProgress(1)
           })
       } 
     )
                         ### Tableaux ###
     ### Effectifs
     fonctionTabEffetif<-function(){
       req(dataCluster())
       if (input$souspop2StatDesc!="Aucune" && is.factor(data2()[,input$souspop2StatDesc])){
         addmargins(table(dataCluster()[,"Clustering"],dataCluster()[,input$souspop2StatDesc]))->TabEffFacteur
         dim(TabEffFacteur)[1]->NumGlobal
         rownames(TabEffFacteur)[NumGlobal]<-"Global"
         colnames(TabEffFacteur)[dim(TabEffFacteur)[2]]<-"Global"
         return(as.data.frame.matrix(TabEffFacteur))
       }
       if (input$souspop2StatDesc=="Aucune"){
         req(tableEffectifR())
         return(tableEffectifR())
       }
       if (input$souspop2StatDesc!="Aucune" && is.numeric(data2()[,input$souspop2StatDesc])) {
         req(BreaksClassInterval())
         addmargins(table(dataSansNA()[,"Clustering"],cut(dataSansNA()[,input$souspop2StatDesc],breaks=unique(BreaksClassInterval()$brks),include.lowest = TRUE)))->TabEffNum
         dim(TabEffNum)[1]->NumGlobal
         rownames(TabEffNum)[NumGlobal]<-"Global"
         colnames(TabEffNum)[dim(TabEffNum)[2]]<-"Global"
         return(as.data.frame.matrix(TabEffNum))
       }
     }
     
     output$DownloadStatDescEffetif <- downloadHandler(
       filename = function() {
         paste(paste0("DataEffectif",input$souspop2StatDesc),input$TypeFichierStatDesc,sep = ".")
       },
       content = function(file) {
         shiny::withProgress(
           message = "Veuillez patienter, le téléchargement de votre fichier est en cours",
           value = 0,
           {
             write.table(fonctionTabEffetif(),file,sep = input$sepcol,row.names=TRUE,col.names = NA,dec = input$dec , fileEncoding = input$endoding)
             shiny::incProgress(1)
           })
       }
     )
     
     ### Profils lignes
     
     fonctionTabPL<-function(){
       req(dataCluster())
       if (input$souspop2StatDesc!="Aucune" && is.numeric(data2()[,input$souspop2StatDesc])) {
         req(dataSansNA(),BreaksClassInterval())
         dataSansNA()->dataClusterDisc
         cut(dataSansNA()[,input$souspop2StatDesc],breaks=unique(BreaksClassInterval()$brks),include.lowest = TRUE)->dataClusterDisc$DiscVarNum
         return(tableau_ligne(data = dataClusterDisc,var_grp = "Clustering",var = "DiscVarNum")$x$data[,1:length(unique(BreaksClassInterval()$brks))])
       }
       if (input$souspop2StatDesc!="Aucune" && is.factor(data2()[,input$souspop2StatDesc])){
         return(tableau_ligne(data = dataCluster(),var_grp = "Clustering",var = input$souspop2StatDesc)$x$data[,1:length(levels(dataCluster()[,input$souspop2StatDesc]))+1])
       }
     }
     
     output$DownloadStatDescProfLig <- downloadHandler(
       filename = function() {
         paste(paste0("DataProfilLigne",input$souspop2StatDesc),input$TypeFichierStatDesc,sep = ".")
       },
       content = function(file) {
         shiny::withProgress(
           message = "Veuillez patienter, le téléchargement de votre fichier est en cours",
           value = 0,
           {
             write.table(fonctionTabPL(),file,sep = input$sepcol,row.names=TRUE,col.names = NA,dec = input$dec , fileEncoding = input$endoding)
             shiny::incProgress(1)
           })
       }
     )
     
     #Profils lignes
     onevent("mouseleave", 'DownloadStatDescProfLig', hide("texteDownloadPL"))
     onevent("mouseenter", 'DownloadStatDescProfLig', show("texteDownloadPL"))
     
     #Tableau effectif
     onevent("mouseleave", 'DownloadStatDescEffetif', hide("texteDownloadEffTab"))
     onevent("mouseenter", 'DownloadStatDescEffetif', show("texteDownloadEffTab"))
     
     #Graphique effectif
     onevent("mouseleave", 'DownGraphStatDesc', hide("texteDownloadEffGraph"))
     onevent("mouseenter", 'DownGraphStatDesc', show("texteDownloadEffGraph"))
     
}