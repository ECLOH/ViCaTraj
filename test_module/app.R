#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ViCaTraj)
library(DT)
source('C:/Users/elie/Desktop/ViCaTraj/R/module_select_and_plot.R', encoding = 'UTF-8')

source('C:/Users/elie/Desktop/ViCaTraj/R/module_data.R', encoding = 'UTF-8')

source('C:/Users/elie/Desktop/ViCaTraj/R/seqggplot_internal.R', encoding = 'UTF-8')
source('C:/Users/elie/Desktop/ViCaTraj/R/seqggplot.R', encoding = 'UTF-8')

# Define UI for application that draws a histogram
ui <- fluidPage(
#textOutput("control_class_comp"),
#textOutput("control_length_comp"),
#dataTableOutput("control_head_comp"),

shiny::tabPanel(title = "tab1",
    module_data_UI(id = "id1")),
shiny::tabPanel(title = "tab2",
module_select_and_plot_UI(id = "id2"))
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    callModule(module = module_data, id = "id1")->DATAs
    output$control_class_comp<-renderPrint({class(DATAs()$DATA_COMP)})
    output$control_length_comp<-renderPrint({length(DATAs()$DATA_COMP)})
    output$control_head_comp<-renderDT({DATAs()$DATA_COMP[[1]]})
    
    callModule(module = module_select_and_plot, data = DATAs(), id = "id2")
    
}

# Run the application 
shinyApp(ui = ui, server = server)
