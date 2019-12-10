#' @title   select_and_plot
#' @description  A shiny Module qui permet de sélectionner des données et de représenter les trajectoires
#'
#' @param id shiny id
#' @param label fileInput label
#' @importFrom stringr str_detect
#' @export
module_select_and_plot_UI <- function(id){#label = "CSV file") {
  ns <- NS(id)
  shiny::fluidRow(
    shiny::column(width=6,
  shiny::selectInput(inputId = ns("plottype"), label = "Quel graphique voulez-vous représenter? ", 
                     choices = c("Chronogramme [seqplot(type = 'd')] "="d", "Séquences les plus fréquentes [seqplot(type = 'f')] "="f", 
                                 "Tapis [seqplot(type = 'I')] "="I", "Etat modal [seqplot(type = 'ms')] "="ms", 
                                 "Durée moyenne dans chaque état [seqplot(type = 'mt')] "="mt",
                                 "Graphique d'entropie [seqplot(type = 'Ht')] "="Ht", 
                                 "Séquences représentatives  [seqplot(type = 'r')] "="r",
                                 "Graphique de flux"="flux",
                                 "Sous-séquences  [seqefsub()] "="sous.seq"
                                 ), selected = "d", multiple = FALSE),
  uiOutput(ns("DATE_server_created")),
  uiOutput(ns("VAR_server_created")),
  #shiny::checkboxInput(inputId = ns("UserSelect"), label = "Voulez-vous sélectionner certaines modalités ? ", value = FALSE),
  uiOutput(ns("CLASS_SELECT_server_created")),
  uiOutput(ns("MOD_SELECT_server_created"))
  ),
  shiny::column(width=6,
  uiOutput(ns("SELECT_COL_TIME_FLUX")),
  uiOutput(ns("SELECT_EVENTS")),
  uiOutput(ns("MERGED_SELECTED_mod")),
  uiOutput(ns("WARNING_LENGTH"))
  ),
  shiny::column(width=12,
  actionButton(inputId = ns("UPDATE_PLOT"), label = "Mettre à jour le graphique et les données")
  ),
  shiny::column(width=12,
  plotOutput(ns("plot1"))
  ),
  shiny::column(width=12,
                
  uiOutput(ns("DATA_r"))
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
#'
#' @importFrom utils read.csv
#' @importFrom glue glue
#' @export
#' @rdname mod_csv_fileInput
module_select_and_plot <- function(input, output, session, data) {
 #### PARAMETERS ####
  library(dplyr)
  library(TraMineR)
  library(RColorBrewer)
  library(ggplot2)
  library(ggthemes)
  ns <- session$ns
 #return(reactive(class(data() )))
  #renderPlot({
  output$DATE_server_created<-renderUI({
    ns <- session$ns
    shiny::selectInput(inputId=ns("DATE_SELECT_M1"), label = "Date pour groupes : ", 
                       choices = c("Pas de groupes", names(data$DATA_COMP)), 
                       selected = "Pas de groupes", multiple = FALSE)
  })
  reactive({
    req(input$DATE_SELECT_M1)
    message("COUCOU 50")
    print(input$DATE_SELECT_M1)
    if(input$DATE_SELECT_M1!="Pas de groupes"){
      data$DATA_COMP[[as.character(input$DATE_SELECT_M1)]]
    } else {
      data$DATA_COMP[[1]]
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
      ordered.df<-the.df.for.groupe[match(row.names(data$SEQ_OBJ), the.df.for.groupe[ , data$ID_VAR]) , ]#order(the.df.for.groupe[ , 1][order(row.names(data$SEQ_OBJ))]) , ]
      grups<-ordered.df[ , "Ensemble"]
    } else {
      if(input$VAR_SELECT_M1=="Pas de groupes"){
        the.df()->the.df.for.groupe
        the.df.for.groupe$Ensemble<-"Ensemble"
        ordered.df<-the.df.for.groupe[match(row.names(data$SEQ_OBJ), the.df.for.groupe[ , data$ID_VAR]) , ]#order(the.df.for.groupe[ , 1][order(row.names(data$SEQ_OBJ))]) , ]
        grups<-ordered.df[ , "Ensemble"]
      } else {
      #if(input$UserSelect==TRUE){
   #   if(input$UserSelect==FALSE){#&length(the.df()[ , data()$ID_VAR])<20){
        the.df()[ , c(data$ID_VAR, input$VAR_SELECT_M1)]->the.df.for.groupe
        subset(the.df.for.groupe, 
               as.character(the.df.for.groupe[ , data$ID_VAR])%in%row.names(data$SEQ_OBJ))->the.df.for.groupe
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
                   as.character(the.df.for.groupe[ , input$VAR_SELECT_M1])%in%input$FactSelect)->the.df.for.groupe
              the.df.for.groupe$VARgrup<-as.character(the.df.for.groupe[ , input$VAR_SELECT_M1])
              
            } else {
              the.df.for.groupe$VARgrup<-as.character(the.df.for.groupe[ , input$VAR_SELECT_M1])
            }
            } else {the.df.for.groupe$VARgrup<-"HORS SELECTION"}
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
        row.names(data$SEQ_OBJ)[!row.names(data$SEQ_OBJ)%in%as.character(the.df.for.groupe[ , data$ID_VAR])]->id.not.in.groupes
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
        ordered.df<-the.df.for.groupe[match(row.names(data$SEQ_OBJ), the.df.for.groupe[ , 1]) , ]#order(the.df.for.groupe[ , 1][order(row.names(data$SEQ_OBJ))]) , ]
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
    if(input$VAR_SELECT_M1!="Pas de groupes"){
      if(input$classSelect=="factor"|input$classSelect=="character"){
      shiny::checkboxInput(inputId = ns("merge_moda"), label = "Le modalités sélectionnées doivent-elles être fusionnées?", value = FALSE)
    }
    }
  })
  
  output$SELECT_COL_TIME_FLUX<-renderUI({
    #req(input$VAR_SELECT_M1)
    #req(the.df())
    #req(input$classSelect)
    if(input$plottype=="flux"){
        shiny::selectInput(inputId = ns("select_col_time_flux"), 
                           label = "Temporalitées choisies pour le graphique de flux : ", 
                           choices = names(data$SEQ_OBJ), selected = names(data$SEQ_OBJ)[1], multiple = TRUE)
      
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
    seqecreate(data = data$SEQ_OBJ)->sese
    seqefsub(eseq = sese, pmin.support = 0.01 )->sese.sub
    
    shiny::updateNumericInput(session = session, inputId = "pminsup",max = max(susu$data$Support))
  })
  
  observe({
    req(input$pminsup )
    req(input$nb_event)
    seqecreate(data = data$SEQ_OBJ)->sese
    seqefsub(eseq = sese, pmin.support = input$pminsup )->sese.sub
    sese.sub$subseq[order(sese.sub$data$Count, decreasing = TRUE), ]->choices.sese
    shiny::updateSelectInput(session = session, inputId = "select_event", choices = choices.sese)
  })
  
  ##### OUTPUTS #####
  THE_PLOT<-eventReactive(input$UPDATE_PLOT, {
    message("COUCOU 372")
    print(the.grups())
    if(length(unique(the.grups()))<20){
    p<-seqggplot(TYPE = input$plottype, 
                 objseq = data$SEQ_OBJ, 
                 groupes = the.grups(), 
                 merge_mods = input$merge_moda, 
                 col.selected = input$select_col_time_flux, pmin.sup = input$pminsup, str.subs = input$select_event)
                  
    } else {NULL}
   return(p)
  })
  
  output$plot1<-renderPlot({
    THE_PLOT()
  })
  
  THE_DATA<-eventReactive(input$UPDATE_PLOT, {
   
    req(the.grups())
    if(length(unique(the.grups()))<20){
      
      lapply(unique(the.grups()),FUN = function(levi){
        
        data$SEQ_OBJ[the.grups()==levi , ]->seqi
        seqi[!is.na(seqi[ , 1]) , ]->seqi
        DONNEES_POUR_PLOT(TYPE = input$plottype, objseq = seqi,
                          pmin.sup=input$pminsup, STR.SUBS=input$select_event)
        
      })->listdat
      #
      names(listdat)<-unique(the.grups())
      #
      lapply(1:length(listdat), function(li){
        message("coucou 412")
        print(listdat[[li]])
        if(class(listdat[[li]])=="list"){
          message("coucou 409")
          lapply(1:length(listdat[[li]]), function(li2){ 
            output[[ paste("l", li, li2, sep="")]]<-DT::renderDataTable(datatable(listdat[[li]][[li2]]))

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
          
          output[[ paste("l1", li, sep="") ]]<-DT::renderDataTable(datatable(data.frame(listdat[[li]])))
          
          list(
            h3( names(listdat)[li]),
            DT::dataTableOutput(ns(paste("l1", li, sep="")))
          )->res
          
return(res)
        }
      })->liou
      
      print(liou)
      
      return(liou)
    } else {NULL}
  })
    
  
  
  output$DATA_r<-renderUI({
    req(THE_DATA())
    THE_DATA()
  })
  
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