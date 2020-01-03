#' #' @title   select_and_plot
#' #' @description  A shiny Module qui permet de sélectionner des données et de représenter les trajectoires
#' #'
#' #' @param id shiny id
#' #' @param label fileInput label
#' #' @importFrom stringr str_detect
#' #' @export
#' module_select_and_plot_UI <- function(id){#label = "CSV file") {
#'   tabsetPanel(
#'     #tabsetPanel(
#'     tabPanel(title="Matrice de distance",
#'              fluidRow(
#'                column(3,
#'                       h4("Paramètres généraux de la classification :"),
#'                       shiny::selectInput(inputId = "selection_rows", label = "Sur quelles données voulez-vous travailler?", 
#'                                          c("Un echantillon"="Sample", 
#'                                            "Des trajectoires uniques avec leurs poids"="unique.traj", 
#'                                            "Toutes les trajectoires"="all"), selected = "all", multiple = FALSE),
#'                       shiny::uiOutput("TEXT_NB_UNIQUE_TRAJS"),
#'                       hr(),
#'                       conditionalPanel(condition="input.selection_rows=='Sample'",
#'                                        shiny::numericInput(inputId = "sample_prop", label = "Taille de l'échantillon", value = 0.1, min = 0.05, max = 0.95, step = 0.05),
#'                                        shiny::uiOutput("UI_SAMPLE_VAR")
#'                       ),
#'                       
#'                       shiny::uiOutput("TEXT_NB_SELECTED_TRAJS") %>% withSpinner(color="#0dc5c1"),
#'                       textOutput("CONTROL_ID_MATCHING")
#'                ),
#'                column(4,
#'                       h4("Type de distance :"),
#'                       shiny::selectInput(inputId = "type_distance", label = "", c("Edition de trajectoires"="edit", "Attributs communs"="common_attributes", "Distribution d'états"="distrib"), multiple = FALSE),
#'                       hr(),
#'                       conditionalPanel(condition = "input.type_distance=='edit'",
#'                                        h4("Paramètres des coûts :"),
#'                                        shiny::uiOutput("InfobulleCout"),
#'                                        #method [seqcost(method = )]
#'                                        selectInput(inputId = "method_edit_cost", label = NULL,
#'                                                    choices = c("CONSTANT" , "TRATE", "FUTURE" , "FEATURES" , "INDELS", "INDELSLOG"),
#'                                                    selected = "TRATE", multiple = FALSE),
#'                                        uiOutput("SEQCOST_INPUTS") %>% withSpinner(color="#0dc5c1"),
#'                                        shiny::actionButton(inputId = "calculCouts", label = "Calcul des couts"))
#'                ),
#'                
#'                column(4,
#'                       h4("Paramètres de la matrice de distance :"),
#'                       shiny::uiOutput("InfobulleMatDistance"),
#'                       #Quelle méthode voulez-vous choisir pour calculer la matrice de distance entre les trajectoires? 
#'                       shiny::selectInput(inputId = "classtype", label = NULL, choices = c("OM", "LCS", "HAM"), selected = "OM", multiple = FALSE),
#'                       uiOutput("SEQDIST_INPUTS") %>% withSpinner(color="#0dc5c1"),
#'                       hr(),
#'                       uiOutput("PRINTTIMEDIST") %>% withSpinner(color="#0dc5c1"),
#'                       hr(),
#'                       shiny::actionButton(inputId = "calculDist", label = "Calcul de la matrice de distance"),
#'                       #conditionalPanel(condition = "input.calculDist",
#'                       uiOutput("PRINTSEQDIST") %>% withSpinner(color="#0dc5c1")
#'                       #)
#'                )
#'              ),
#'              conditionalPanel(condition = "input.type_distance=='edit'",
#'                               
#'                               fluidRow(
#'                                 h3("Affichage des coûts calculés:"),
#'                                 hr(),
#'                                 h4("Coûts 'indel' :" ),
#'                                 uiOutput("PRINTINDEL") %>% withSpinner(color="#0dc5c1"),
#'                                 h4("Coûts de substitution :" ),
#'                                 uiOutput("PRINTSUBST") %>% withSpinner(color="#0dc5c1")
#'                               )
#'              ),
#'              uiOutput("ATTR_TRAJ_FORCLASS")
#'              # shiny::conditionalPanel(condition = "input.classtype=='OM'",
#'              #                         shiny::sliderInput(label = "Coûts de substitution: rapport aux coûts indel", inputId = "subst_ratio", min = 0.1, max = 5, step = 0.1, value = 1, width = "80%")
#'              # )
#'              
#'     )
#'     ,tabPanel(title="Classification",
#'               fluidRow(
#'                 column(4,
#'                        br(),
#'                        shiny::uiOutput("InfobulleClassif"),
#'                        # Quelle méthode voulez-vous utiliser pour regrouper les séquences ? partir de la matrice de dissemblance?
#'                        shiny::selectInput(inputId = "cluster_type", label = NULL, 
#'                                           choices = c("Hierarchical Clustering"="CAH", "FAST Hierarchical Clustering"="fastCAH", "Partitionning Around Medoid"="PAM","Combinaison de la CAH et de PAM"="CAHPAM"), selected = "CAHPAM", multiple = FALSE)),
#'                 column(4,br(),
#'                        conditionalPanel(condition = "input.cluster_type=='CAH' | input.cluster_type=='CAHPAM'",
#'                                         shiny::uiOutput("InfobulleClassifCAH"),
#'                                         #Choix de la méthode (CAH) :
#'                                         shiny::selectInput(inputId = "agnes_method", choices = c("average", "single", "complete", "ward", "weighted", "flexible", "gaverage"), label = NULL, selected = "ward", multiple = FALSE)),
#'                        conditionalPanel(condition = "input.cluster_type=='fastCAH'",
#'                                         shiny::uiOutput("InfobulleClassiffastCAH"),
#'                                         # Choix de la méthode (FAST CAH) :
#'                                         shiny::selectInput(inputId = "fastclust_method", choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" ,"centroid"), label = NULL, selected = "ward.D2", multiple = FALSE))
#'                 ),
#'                 # 
#'                 
#'                 column(2,
#'                        br(),br(),
#'                        shiny::actionButton(inputId = "calculCLUST", label = "Calcul de la classification")
#'                 )),
#'               fluidRow(useShinyjs(),
#'                        uiOutput("classif_grp"),
#'                        column(2,
#'                               textOutput("textCluster")),
#'                        column(2,
#'                               conditionalPanel(condition = "input.Bouton_Clustering",
#'                                                shiny::radioButtons(inputId = "TypeFichierDownload", label = "Extension du fichier",choices=c("csv","txt"),selected = "csv"),
#'                                                downloadButton('ButtondownloadData', 'Télécharger les données')
#'                               )
#'                        ),
#'                        column(4,
#'                               shiny::uiOutput("TexteClassif"))
#'               ),
#'               uiOutput("classif"),
#'               uiOutput("tabind")
#'               
#'     )
#'   )
#' }
#' #' server function
#' #'
#' #' @param input internal
#' #' @param output internal
#' #' @param session internal
#' #' @param obj.seq
#' #' @param data.comp
#' #'
#' #' @importFrom utils read.csv
#' #' @importFrom glue glue
#' #' @export
#' #' @rdname mod_csv_fileInput
#' module_select_and_plot <- function(input, output, session, data) {
#'   
#'   
#' }