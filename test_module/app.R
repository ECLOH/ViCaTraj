#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(ViCaTraj)
library(DT)
source('C:/Users/elie/Desktop/ViCaTraj/R/module_select_and_plot.R', encoding = 'UTF-8')

source('C:/Users/elie/Desktop/ViCaTraj/R/module_data.R', encoding = 'UTF-8')
source('C:/Users/elie/Desktop/ViCaTraj/R/module_tabdes.R', encoding = 'UTF-8')


source('C:/Users/elie/Desktop/ViCaTraj/R/seqggplot_internal.R', encoding = 'UTF-8')
source('C:/Users/elie/Desktop/ViCaTraj/R/seqggplot.R', encoding = 'UTF-8')

# Define UI for application that draws a histogram
ui <- fluidPage(
#textOutput("control_class_comp"),
#textOutput("control_length_comp"),
#dataTableOutput("control_head_comp"),
textOutput("control_ID"),
textOutput("control_ID2"),
textOutput("control_ID3"),

tabsetPanel(
    shiny::tabPanel(title = "tab1",
    module_data_UI(id = "id1")),
shiny::tabPanel(title = "tab2",
module_select_and_plot_UI(id = "id2")),
shiny::tabPanel(title = "tab3",
module_tabdes_UI(id = "id3")),
shiny::tabPanel(title = "tab4",
                module_tabdes_UI(id = "id4"))
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    callModule(module = module_data, id = "id1")->DATAs
    output$control_class_comp<-renderPrint({class(DATAs()$DATA_COMP)})
    output$control_length_comp<-renderPrint({length(DATAs()$DATA_COMP)})
    output$control_head_comp<-renderDT({DATAs()$DATA_COMP[[1]]})
    output$control_ID<-renderPrint({DATAs()$ID_VAR})
    output$control_ID2<-renderPrint({class(DATAs()$ID_VAR)})
    output$control_ID3<-renderPrint({length(DATAs()$ID_VAR)})
    
    observe({print(DATAs()$ID_VAR)})
    
    callModule(module = module_select_and_plot, data = DATAs(), id = "id2")
    
    callModule(module = module_tabdes, data = DATAs(), id = "id3")
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
