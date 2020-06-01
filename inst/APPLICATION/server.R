#### DATA ####
#data("trajs")
#trajs<-read.csv("G:/dgsd/Dopro/Projets/2018/E - Etude_RSA/E-RSA_Appli_Shiny_Trajectoires/GitHub/ViCaTraj-master/data/trajs.csv",row.names = 1)

# rev(wesanderson::wes_palette(name = "Darjeeling1", n = length(alphabet(trajs)), type = "discrete"))->cpal.seq
# cpal(seqdata = trajs)<-cpal.seq
# seqtab(trajs[ , ], idxs = 0, format = "STS")->unique.trajs

#### SERVER ####
server <- function(input, output, session) {

  library(ViCaTraj)
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
  library(tibble)
  
  
  
  callModule(module = module_data, id = "id1")->DATAs
  


#observe({print(head(DATAs()$DATA_COMP))})


#### STATISTIQUES DESCRIPTIVES ####
  ##### SELECT AND PLOT MODULE #####
  callModule(module = module_select_and_plot2, data = DATAs, id = "id2")
  ##### SELECT AND PLOT ANCIEN #####
  
  
  ####### Type de graph ####
  #mise a jour des input et création de nouveaux input selon le type de sous-population chois

  #### MODULE TABLE ####
  callModule(module = module_tabdes, data = DATAs, id = "id3")
  
  #### MODULE CLASSIF ####
  
  
  callModule(module = module_classification, data=DATAs, id = "id5")->DATAs.c#DATA.CLASSIF$DATAs.c
  
  #observe({
  callModule(module = module_select_and_plot2, data = DATAs.c, #reactive(DATA.CLASSIF$DATAs.c), 
             id = "id25")
  callModule(module = module_tabdes2, data = DATAs.c, id = "id35")
  
 # observe({
#    input$reactKlass
#    isolate({
#    callModule(module = module_select_and_plot, data = DATAs.c(), #reactive(DATA.CLASSIF$DATAs.c), 
#               id = "id25")
#    })
#  })
  #})
 # observe({
#    input$reactKlass
#    isolate({
#    callModule(module = module_tabdes, data = DATAs.c(), id = "id35")
#    })
#  })
  
}