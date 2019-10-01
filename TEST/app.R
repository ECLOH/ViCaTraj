#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
module_data_UI(id = "id1"),
textOutput("DIMTEST"),
textOutput("DATYPE")


)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2) 
callModule(module = module_data, id = "id1")->RESTEST
output$DIMTEST<-renderPrint({
  print(dim(RESTEST()$DATA_COMP))
})
output$DATYPE<-renderPrint({
  print(input$DataType)
})
}

# Run the application 
shinyApp(ui = ui, server = server)
