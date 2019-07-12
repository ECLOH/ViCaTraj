#### packages ####
library(shiny)
library(TraMineR)
library(ggplot2)
library(TraMineRextras)
library(RColorBrewer)
library(DT)
library(RColorBrewer)
library(wesanderson)
library(shinythemes)
library(shinycssloaders)
library(cluster)
library(fastcluster)
library(gridExtra)
library(ggalluvial)
library(tidyr)
library(dplyr)
library(shinyWidgets)
library(formattable)
library(WeightedCluster)
library(stringr)
library(shinyjs)
library(classInt)
library(shinyBS)
library(forcats)
options(shiny.maxRequestSize=700*1024^2)

#### UI ####

ui <- shinyUI(navbarPage('ViCaTraj', id="page", collapsible=TRUE, inverse=FALSE,theme=shinytheme("flatly"),#fluidPage(theme = shinytheme("flatly"),
                         tabPanel("Création d'un jeu de données", 
                                  sidebarPanel(
                                    h3("Sélection de l'objet source:"), 
                                    width = 6,
                                    shiny::fileInput(inputId = "LIST_SOURCE_BIG_DF", 
                                                     label = "Objet Rdata à charger:", 
                                                     multiple = FALSE ),
                                    shiny::column(width = 6,
                                    shiny::selectInput( inputId = "MINTIMEBIG", label = "Borne temporelle inférieure:", 
                                                        multiple = FALSE, choices = "" , width = '100%')),
                                    shiny::column(width = 6,
                                    shiny::selectInput( inputId = "MAXTIMEBIG", label = "Borne temporelle supérieure:", 
                                                        multiple = FALSE , choices = "", width = '100%')),
                                    shiny::numericInput(inputId = "PAS_TEMPS_BIGDATA", label = "Pas de temps pour les données:", 
                                                        value = 1, min = 1, step = 1, width = '20%'),
                                    textOutput("CONTROLNAMES"),
                                    textOutput("SLIDERTEXT")
                                    
                                  ),
                                  sidebarPanel(
                                    h3("Sélection des individus:"), 
                                    uiOutput("UI_INDVAR_CHOOSE"),
                                    width = 6,
                                    shiny::column(width=6, 
                                    uiOutput("UI_PAQUET_SELECT")
                                    ), 
                                    shiny::column(width=6, 
                                                  uiOutput("UI_DATE_SELECT")
                                    ), 
                                    uiOutput("UI_VAR_SELECT"), 
                                    uiOutput("UI_CLASS_SELECT"),
                                    uiOutput("UI_MOD_SELECT"),
                                    h3("CONTROL_LIST.OF.INF:"),
                                    uiOutput("CONTROL_LIST.OF.INF"),
                                    h3("UI_VIEW_VAR:"),
                                    uiOutput("UI_VIEW_VAR"),
                                    
                                    
                                   # uiOutput("UI_MOD_SELECT2"),
                                    
                                    actionButton(inputId="addROW", label = "Ajouter la condition"),
                                    #uiOutput("UI_SELECT_DF_TO_SUBSET"),
                                    #filterDataUI(id = "ex"),
                                    #actionButton(inputId="FILTER", label = "Appliquer les filtres"),
                                    hr(),
                                    DT::DTOutput("TABLE_POUR_SELECTION"),
                                   actionButton(inputId="APPLICATE_SUBSET", label = "Appliquer les conditions"),
                                   #tableOutput("SUBSET_OUTPUT"),
                                   #textOutput("SUBSET_BY_PAQUET_OUTPUT"),
                                   #textOutput("SUBSET_G_OUTPUT"),
                                   #DTOutput("DATA_OF_SUBSET_CONTROL")
                                   textOutput("LENGTH_IND_SUBS")
                                    )),
                         tabPanel("Paramètres de la session",
                                  #tabsetPanel(id = "tabpan",
                                  #            tabPanel(title = "Paramètres de la session: ",
                                  sidebarPanel(
                                    h3("Chargement des fichiers de données"), 
                                    width = 6,
                                    shiny::selectInput(inputId = "DataType", label = "Choix du type de données", 
                                                       choices = c("Un objet RData contenant de multiples data.frame"="objet", 
                                                                   "Un seul fichier.csv contenant des données prêtes à l'emploi"="fichier"), 
                                                       multiple = FALSE, selected = "fichier"),
                                    conditionalPanel(
                                      condition = "input.DataType == 'fichier'",
                                
                                      fileInput(inputId="file1", label="Sélectionnez votre fichier source:", 
                                              multiple = FALSE, accept = c("text/csv",
                                                                           "text/comma-separated-values,text/plain",
                                                                           ".csv"), width = NULL),
                                      shiny::selectInput(inputId="sepcol", label= "Separateur de colonnes", choices=c("Virgule" = ",","Point-Virgule" = ";","Tabulation" = "\t"), selected=","),
                                      shiny::selectInput(inputId="dec", label= "Séparateur décimal", choices=c("Virgule" = ",","Point" = "."), selected="."),
                                      shiny::selectInput(inputId="endoding", label= "Comment est codé le fichier ? Les accents sont-ils correctement lus ?", choices=c(UTF8 = "UTF-8", Latin1 = "latin1"), selected = "UTF-8", multiple = FALSE, width = "50%"),
                                      shiny::checkboxInput(inputId = "header", label="La première ligne correspond-elle aux noms des variables ?",value=FALSE),  
                                      shiny::checkboxInput(inputId = "rowname", label="Une variable correspond-elle à un identifiant des individus ?",value=FALSE),
                                      conditionalPanel(
                                        condition = "input.rowname == true",
                                        shiny::selectInput(inputId="rownames_par", label="Variable servant d'identifiant", 
                                                           choices = "", multiple = FALSE,selected = NULL, selectize = TRUE)),
                                      shiny::selectInput(inputId = "na", label = "Codage des valeurs manquantes", choices = c("Vide" , "Espace" = " ", "NA" = "NA"), selected = "NA", multiple = TRUE, selectize = TRUE)),
                                    conditionalPanel(
                                      condition = "input.DataType == 'objet'",
                                      h5("INFO: pour des raisons de sécurité il n'est pas possible de charger directement un dossier dans un navigateur web."),
                                      h5("Vous pouvez utiliser la fonction LIST_MULTIPLE_CSV du package ViCaTraj pour créer l'objet RData à partir de mulitples fichiers .csv"),
                                      fileInput(inputId="objetFile", 
                                                label="Sélectionner l'objet .RData contenant les multiples data.frame", 
                                                multiple = TRUE, accept = NULL, width = NULL),
                                      shiny::textOutput("CONTROLDATA"))
                                  ),
                                  sidebarPanel(
                                    h3("Paramétrage des trajectoires"),
                                    width = 6,
                                  shiny::selectInput(inputId = "timecol", label = "Variables temporelles (mettre dans l'ordre chronologique)", choices = "", selected = "PrestationRSA.SituationDossierRSA.EtatDossierRSA.ETATDOSRSA", multiple = TRUE, selectize = TRUE),
                                  shiny::uiOutput("DATA_UI"),
                                  shiny::selectInput(inputId = "mode", label = "Mode de travail:", 
                                                     choices = c("Flux d'entrants", "Flux en continu"), 
                                                     selected = c("Flux en continu"), multiple = FALSE),
                                  shiny::numericInput(inputId = "criterNb", label = "Critère de sortie : nombre de mois consécutifs",value = 3, min = 1, max = 36, step = 1),
                                  shiny::actionButton(inputId = "ValidParametres", label = "Je valide ces paramètres")
                                  
                                  
                                  #,textOutput("DATE_RANGE_1")
                         )
                         ,
                         mainPanel(
                           shiny::dataTableOutput("contenu")
                           )),


                         tabPanel("Statistiques descriptives",
                                  tabsetPanel(
                                    tabPanel(title = "Taux de transition ( et taux de sortie) ",
                                             useShinyjs(),
                                             shiny::numericInput(inputId = "PAStrate", label = "Pas de temps pour le calcul des taux de transition",value = 1, min = 1, max = 36, step = 1),
                                             shiny::checkboxInput(inputId = "TYPEtrate", label = "Les taux de transitions varient-ils avec le temps?",value = FALSE),
                                             shiny::radioButtons(inputId = "TypeValeur", label = "Afficher en pourcentage ou en effetif",choices=c("Effectif","Pourcentage"),selected = "Pourcentage"),
                                             shiny::radioButtons(inputId = "DebArr",label="Transitions en partant : ",choices = c("Du début"="deb","De la fin"="fin"),selected = "deb"),
                                             shiny::radioButtons(inputId = "TypeTrans",label="Extension des fichiers : ",choices = c(".csv",".txt"),selected = ".csv"),
                                             textOutput("infotrate"),
                                             uiOutput("dt")
                                             #dataTableOutput('TRAJS_TRATE')
                                             
                                    ),
                                    tabPanel(title = "Représentation des trajectoires",
                                             fluidRow(useShinyjs(),
                                               column(2,
                                                shiny::selectInput(inputId = "plottype", label = "Quel graphique voulez-vous représenter? ", choices = c("Chronogramme"="d", "Séquences les plus fréquentes"="f", "Tapis"="I", "Etat modal"="ms", "Durée moyenne dans chaque état"="mt","Graphique d'entropie"="Ht", "Séquences représentatives"="r","Graphique de flux"="flux","Sous-séquences triées selon leur support"="sous.seq","Sous-séquences choisies"="sous.seq.ch"), selected = "d", multiple = FALSE),
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
                                                                 shiny::radioButtons(inputId = "TapisSorted",label="Trier selon : ",choices = c("Le début"="from.start","La fin"="from.end"),selected = "from.start")),
                                                
                                                shiny::selectInput(inputId = "souspop1", label = "Sous-Population", choices = "", selected = "", multiple = FALSE),
                                                shiny::uiOutput(outputId= "slider1"),
                                                shiny::uiOutput(outputId= "modalite1"),
                                                conditionalPanel(condition="input.plottype=='flux'",
                                                  shiny::selectInput(inputId = "timeseq1", label = "Pas de temps", choices = "", selected = "", multiple = TRUE, selectize = TRUE),
                                                  
                                                  shiny::actionButton(inputId = "graph1", label = "Afficher le graphique")
                                                  ),
                                                # conditionalPanel(condition="input.plottype=='sous.seq.ch'",
                                                #                  shiny::actionButton(inputId = "graphSousSeq", label = "Afficher le graphique")),
                                                conditionalPanel(condition="input.plottype=='sous.seq'",
                                                                 shiny::sliderInput(inputId = "pmin1", label = "Support minimal",min=0,max=1,value=0.15,step = 0.01)
                                              
                                               )),
                                               column(10,align="center",
                                                      tags$p("Pour visualiser les mêmes graphiques pour des groupes, allez dans", tags$em("Classification des trajectoires"), "puis faire la" , tags$em("Matrice de distance"), "et la",tags$em("Classification."),"Les graphiques seront dans l'onglet",tags$em("Visualisation des groupes.")),
                                                      uiOutput("txtAjoutSeq"),
                                                      uiOutput("h4_fluxGlobal"),
                                                      uiOutput("PLOT3")%>% withSpinner(color="#0dc5c1"),
                                                      downloadButton(outputId="DownGraphGlobal",label="Télécharger le graphique"),
                                                      hidden(p(id="TexteDownloadGraph","Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (.png). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application.")),
                                                      textOutput("TexteGraph")
                                               
                                                      #,uiOutput("subsTable")
                                                      
                                             
                                               )
                                             )
                                    )
                                  )
                         ),
                         tabPanel("Classification des trajectoires",
                                  tabsetPanel(
                                    #tabsetPanel(
                                    tabPanel(title="Matrice de distance",
                                             fluidRow(
                                               column(3,
                                                      h4("Paramètres généraux de la classification :"),
                                                      shiny::selectInput(inputId = "selection_rows", label = "Sur quelles données voulez-vous travailler?", c("Un echantillon"="Sample", "Des trajectoires uniques avec leurs poids"="unique.traj", "Toutes les trajectoires"="all"), selected = "all", multiple = FALSE),
                                                      shiny::uiOutput("TEXT_NB_UNIQUE_TRAJS"),
                                                      hr(),
                                                      conditionalPanel(condition="input.selection_rows=='Sample'",
                                                                       shiny::numericInput(inputId = "sample_prop", label = "Taille de l'échantillon", value = 0.1, min = 0.05, max = 0.95, step = 0.05),
                                                                       shiny::selectInput(inputId = "sample_var",
                                                                                          label = "Variables utilisées pour la représentativité",
                                                                                          choices = c("Territoire de l'Isère", "Sexe"), multiple = TRUE, selected = NULL)),
                                                      shiny::uiOutput("TEXT_NB_SELECTED_TRAJS") %>% withSpinner(color="#0dc5c1")
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
                                                      conditionalPanel(condition = "input.calculDist",
                                                      uiOutput("PRINTSEQDIST") %>% withSpinner(color="#0dc5c1")
                                                      )
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
                                             )
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
                                             shiny::selectInput(inputId = "cluster_type", label = NULL, choices = c("Hierarchical Clustering"="CAH", "FAST Hierarchical Clustering"="fastCAH", "Partitionning Around Medoid"="PAM","Combinaison de la CAH et de PAM"="CAHPAM"), selected = "CAHPAM", multiple = FALSE)),
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
                                    uiOutput("classif"),
                                    uiOutput("tabind"),
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
                                    )
                                    ),
                                    tabPanel(title="Visualisation des groupes",
                                             
                                             fluidRow(
                                               column(2,
                                                      shiny::selectInput(inputId = "plottypeG", label = "Quel graphique voulez-vous représenter? ", choices = c("Chronogramme"="d", "Séquences les plus fréquentes"="f", "Tapis"="I", "Etat modal"="ms", "Durée moyenne dans chaque état"="mt","Graphique d'entropie"="Ht", "Séquences représentatives"="r","Graphique de flux"="flux","Sous-séquences discriminantes(Pearson)"="Pearson","Sous-séquences choisies (Pearson)"="Pearson.ch"), selected = "d", multiple = FALSE),
                                                          conditionalPanel(condition="input.plottypeG=='Pearson.ch'",
                                                                           wellPanel(shiny::selectInput(inputId = "par.sous.seq1G",label = "Etat 1",choices = "",multiple = FALSE),
                                                                                     shiny::selectInput(inputId = "par.sous.seq2G",label = "Etat 2",choices = "",multiple = FALSE),
                                                                                     shiny::selectInput(inputId = "par.sous.seq3G",label = "Etat 3",choices = "",multiple = FALSE),
                                                                                     shiny::numericInput(inputId = "ligne.supprG", label = "Ligne à supprimer", min = 1, max = 100, value = 1),
                                                                                     shiny::actionButton(inputId = "add.buttonG", label = "Ajouter", icon = icon("plus")),
                                                                                     br(),br(),
                                                                                     shiny::actionButton(inputId = "delete.buttonG", label = "Supprimer", icon = icon("minus"))
                                                                           )
                                                          ),
                                                      conditionalPanel(condition="input.plottypeG=='I' ",
                                                                       shiny::radioButtons(inputId = "TapisSortedG",label="Trier selon : ",choices = c("Le début"="from.start","La fin"="from.end"),selected = "from.start")),
                                                      
                                                          shiny::selectInput(inputId = "souspop2", label = "Sous-population", choices = "", selected = "", multiple = FALSE),
                                                          shiny::uiOutput(outputId= "slider2"),
                                                          shiny::uiOutput(outputId= "modalite2"),
                                                      
                                                      conditionalPanel(condition="input.plottypeG=='flux'",
                                                                       shiny::selectInput(inputId = "timeseq2", label = "Pas de temps", choices = "", selected = "", multiple = TRUE, selectize = TRUE),
                                                                       shiny::selectInput(inputId="var_grp",label="Variable Groupe",choices=c(""),selected ="" ,multiple = TRUE),
                                                                       
                                                                       shiny::actionButton(inputId = "graph2", label = "Afficher les graphiques")
                                                      ),
                                                      conditionalPanel(condition="input.plottypeG=='Pearson'",
                                                        shiny::sliderInput(inputId = "pmin",label = "Support minimal",min=0,max=1,value=0.15,step = 0.01),
                                                        shiny::numericInput(inputId = "nbAffiche",label = "Nombre d'états affichés",min=1,max=1,value=1,step=1))
                                                      
                                               ),
                                               column(10,align="center",
                                                      tags$p("Pour visualiser les mêmes graphiques pour le global, allez dans la partie",tags$em("Statistiques descriptives"),"puis dans l'onglet",tags$em("Représentation des trajectoires")),
                                                      # shiny::uiOutput("TitreGlobal"),
                                                      # shiny::uiOutput("GraphGlobal"),
                                                      shiny::uiOutput("h4_fluxGrp"),
                                                      shiny::uiOutput("alpabeltTexte"),
                                                      shiny::uiOutput("PLOTG")%>% withSpinner(color="#0dc5c1"),
                                                      #downloadButton(outputId="DownGraphGlobalGrp",label="Télécharger le(s) graphique(s)"),
                                                      
                                                      shiny::textOutput("TexteGraphGrp")
                                                      ##,shiny::uiOutput("subsTableG")
                                                      ))
                                             
    
                                        ),
                                    tabPanel(title="Statistiques descriptives",
                                             fluidRow(useShinyjs(),
                                               column(2,
                                                      shiny::selectInput(inputId = "souspop2StatDesc", label = "Sous-population", choices = "", selected = "", multiple = FALSE),
                                                      # uiOutput("slider2StatDesc"),
                                                      # uiOutput("modalite2StatDesc"),
                                                      uiOutput("outils"),
                                                      uiOutput("UIVarNumStatDesc"),
                                                      shiny::radioButtons(inputId = "TypeFichierStatDesc", label = "Extension du fichier",choices=c("csv","txt"),selected = "csv")
                                                      ),
                                               column(10,align="center",
                                                      shiny::uiOutput("profilLigne")%>% withSpinner(color="#0dc5c1")
                                                     )
                                             )
                                             
                                             )

                                  ) 
                         )

                         

 )
 )
