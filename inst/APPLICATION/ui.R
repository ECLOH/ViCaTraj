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
                                               tabPanel(title = "Représentation des trajectoires",
                                                        module_select_and_plot_UI(id = "id2")
                                               
                                               ),
                                             
                        #### SUB-PANEL: tables ####
                        tabPanel(title = "Tables et tables de contingence",
                                 module_tabdes_UI(id = "id3")
                                 
                        )
                                    )
                                    ),
              #### PANEL: classif ####
              
                                    tabPanel("Classification des trajectoires",
                                                             shiny::actionButton(inputId = "reactKlass", label = "Je valide la classification et les groupes"),
                                                             tabsetPanel(
                                                               
                                                               shiny::tabPanel(title = "Classification et groupes", 
                                                                               module_classification_UI(id = "id5")),
                                                               shiny::tabPanel(title = "Représentation des trajectoires (groupes)",
                                                                               module_select_and_plot_UI(id = "id25")),
                                                               shiny::tabPanel(title = "Tables et tables de contingence (groupes)",
                                                                               module_tabdes_UI(id = "id35"))
                                                             )
                                             )
)
)
                         
                         