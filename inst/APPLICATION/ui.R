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
library(ggthemes)
options(shiny.maxRequestSize=700*1024^2)

#### UI ####

ui <- shinyUI(navbarPage('ViCaTraj', id="page", collapsible=TRUE, inverse=FALSE,theme=shinytheme("flatly"),#fluidPage(theme = shinytheme("flatly"),
                         tabPanel("Présentation", 
                                  tags$div(
                                    tags$p("Cette application vise à faciliter la manipulation de données de trajectoires à partir du logiciel de traitement statistique 'R'. Elle permet aux personnes qui ne sont pas familières de ce langage de pouvoir utiliser un certain nombre de fonctions particulièrement utiles pour la manipulation de données de trajectoires."), 
                                    h4("1. Construction d'un jeu de données : "),
                                    tags$p("L'application permet dans un premier temps de construire des trajectoires à partir de données 'brutes' (dans l'onglet 'les données'). "),
                                    tags$p("Trois types de fichiers peuvent être chargés dans l'application : "),
                                    tags$ul(
                                        tags$li("un fichier .csv avec des données au format 'wide' : une colonne par variable et par date, et à la suite une colonne par variable complémentaire."),
                                        tags$li("un fichier .Rdata comportant une liste ('list()') avec un objet 'trajectoire' créé par la fonction seqdef() et une data.frrame comportant des données complémentaires. Dans ce cas, les noms de ligne (row.names()) des trajectoires doivent correspondre avec une colonne de variable d'identifiant individuel dans le data.frame."),
                                        tags$li("un fichier .Rdata comportant une liste ('list()') avec pour chaque date (chaque mois, chaque année, chaque jour, etc) un data.frame avec les valeurs correspondantes. Dans ce cas, les trajectoires pourront être générées à partir de n'importe quel nom de variable commun aux différentes dates.")
                                       ),
                                    tags$p("A partir de ces différentes sources, certaines possibilités de sélection d'individus sont offertes. Chaque sélection constituée dans l'application et qui a donné lieu à la constitution de trajectoires peut être enregistrée localement et réutilisée."),
                                    h4("2. Indicateurs statistiques : "),
                                    tags$p("Une fois que les trajectoires sont construites et validées différents onglets permettent d'obtenir des indicateurs statistiques sur les trajectoires et les données complémentaires."),
                                    tags$ul(
                                      tags$li("Les taux de transition : calcul des taux de transitions entre états selon différents paramètres."),
                                        tags$li("Les principaux indicateurs de base fournis dans le package R 'TraMineR', croisés avec les données complémentaires (chronogramme des états, temps moyen passé dans chaque état, événements, états modaux, représentations en tapis, graphiques de flux, etc."),
                                      tags$li("Des tableaux de donnés (tris à plat ou tableau croisés) sur l'ensemble des données générées, y compris les données complémentaires.")
                                    ),
                                    h4("3. Classification des trajectoires : "),
                                    tags$p("L'application permet aussi de générer des classes de trajectoires à partir de méthodes basiques (Optimal Matching et ses dérivés principalement). Néanmoins, pour une classification fine et pleinement paramétrable, il serais préférable d'utiliser directement les diverses fonctions existantes dans une interface R classique."),
                                    tags$hr(),
                                    tags$h4("Licence et références :  "),
                                    img(src='LICENCE_VICATRAJ.png', align = "left"),
                                    tags$p("Application sous licence CC BY-NC-SA."),
                                    tags$p("Adresse du code source : ", tags$a("https://github.com/ECLOH/ViCaTraj")),
                                    tags$p("Application qui utilise massivement les fonctions incluses dans le package 'TraMineR'.  Voir pour les références et l'excellente documentation : ", tags$a("http://traminer.unige.ch/index.shtml")),
                                    tags$p("Application générée grâce au logiciel 'R' et en particulier aux packages 'shiny' et 'ggplot' "),
                                    tags$p()
                                    
  
                                  )
                         ),
                         tabPanel("Les données",
                                  module_data_UI(id = "id1")
                                  ),
                                    #### PANEL: STATISTIQUES ####
                                    tabPanel("Statistiques descriptives",
                                             tabsetPanel(
                                               #### SUB-PANEL: transition ####
                                               tabPanel(title = "Taux de transition ( et taux de sortie ) ",
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
                                               #### SUB-PANEL: représentation ####
                                               tabPanel(title = "Représentation des trajectoires (module)",
                                                        module_select_and_plot_UI(id = "id2")
                                               
                                               ),
                                               tabPanel(title = "Représentation des trajectoires",
                                                        fluidRow(useShinyjs(),
                                                                 column(2,
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
                                                                                         shiny::radioButtons(inputId = "TapisSorted",label="Trier selon : ",choices = c("Le début"="from.start","La fin"="from.end"),selected = "from.start")),
                                                                        
                                                                        shiny::selectInput(inputId = "souspop1", label = "Sous-Population", choices = "Aucune", selected = "Aucune", multiple = FALSE),
                                                                        shiny::uiOutput(outputId= "slider1"),
                                                                        shiny::uiOutput(outputId= "modalite1"),
                                                                        conditionalPanel(condition="input.plottype=='flux'",
                                                                                         shiny::selectInput(inputId = "timeseq1", label = "Pas de temps", 
                                                                                                            choices = "", selected = "", multiple = TRUE, selectize = TRUE),
                                                                                         
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
                                                                        uiOutput("PLOT_DES")%>% withSpinner(color="#0dc5c1"),
                                                                        uiOutput("TAB_DES")%>% withSpinner(color="#0dc5c1"),
                                                                        
                                                                        #uiOutput("PLOT3")%>% withSpinner(color="#0dc5c1"),
                                                                        downloadButton(outputId="DownGraphGlobal",label="Télécharger le graphique"),
                                                                        hidden(p(id="TexteDownloadGraph","Si l'application n'est pas ouverte dans un navigateur internet, il faut ajouter manuellement l'extension du fichier (.png). Pour ouvrir l'application avec un navigateur internet, il faut mettre Run External avant de lancer l'application ou appuyer sur Open in Browser en haut de l'application.")),
                                                                        textOutput("TexteGraph")
                                                                        
                                                                        #,uiOutput("subsTable")
                                                                        
                                                                        
                                                                 )
                                                        )
                                               ),
                                             
                                   
                        #### SUB-PANEL: tables ####
                        tabPanel(title = "Tables et tables de contingence",
                                 module_tabdes_UI(id = "id3")
                                 
                        )
                                    )
                                    ),
              #### PANEL: classif ####
              
                                    tabPanel("Classification des trajectoires",
                                             textOutput("id_module_output"),
                                             tabsetPanel(
                                               #tabsetPanel(
                                               tabPanel(title="Matrice de distance",
                                                        fluidRow(
                                                          column(3,
                                                                 h4("Paramètres généraux de la classification :"),
                                                                 shiny::selectInput(inputId = "selection_rows", label = "Sur quelles données voulez-vous travailler?", 
                                                                                    c("Un echantillon"="Sample", 
                                                                                      "Des trajectoires uniques avec leurs poids"="unique.traj", 
                                                                                      "Toutes les trajectoires"="all"), selected = "all", multiple = FALSE),
                                                                 shiny::uiOutput("TEXT_NB_UNIQUE_TRAJS"),
                                                                 hr(),
                                                                 conditionalPanel(condition="input.selection_rows=='Sample'",
                                                                                  shiny::numericInput(inputId = "sample_prop", label = "Taille de l'échantillon", value = 0.1, min = 0.05, max = 0.95, step = 0.05),
                                                                                  shiny::uiOutput("UI_SAMPLE_VAR")
                                                                 ),
                
                                                                 shiny::uiOutput("TEXT_NB_SELECTED_TRAJS") %>% withSpinner(color="#0dc5c1"),
                                                                 textOutput("CONTROL_ID_MATCHING")
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
                                                                 #conditionalPanel(condition = "input.calculDist",
                                                                                  uiOutput("PRINTSEQDIST") %>% withSpinner(color="#0dc5c1")
                                                                 #)
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
                                                        ),
                                                        uiOutput("ATTR_TRAJ_FORCLASS")
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
                                                                  shiny::selectInput(inputId = "cluster_type", label = NULL, 
                                                                                     choices = c("Hierarchical Clustering"="CAH", "FAST Hierarchical Clustering"="fastCAH", "Partitionning Around Medoid"="PAM","Combinaison de la CAH et de PAM"="CAHPAM"), selected = "CAHPAM", multiple = FALSE)),
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
                                                         ),
                                                         uiOutput("classif"),
                                                         uiOutput("tabind")
                       
                                               ),
                                               tabPanel(title="Visualisation des groupes",
                                                        textOutput("VERIF_SELECT2"),
                                                        #textOutput("VERIF_SELECT2_PRINTVAR"),
                                                        textOutput("VISUAL_CONTROL_VAR2"),
                                                        textOutput( "LENTH_MOD2"),
                                                        #textOutput("ENCODING_MOD2"),
                                                        #plotOutput("SEQPLOT1"),
                                                        #plotOutput("SEQPLOT2"),
                                                        #plotOutput("SEQPLOT3"),
                                                        textOutput("DIM_SELECTED_2"),
                                                        textOutput("CLASS_SELECT2"),
                                                        
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
                                                                                  shiny::selectInput(inputId = "timeseq2", label = "Pas de temps", 
                                                                                                     choices = "", selected = "", multiple = TRUE, selectize = TRUE),
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
                         
                         