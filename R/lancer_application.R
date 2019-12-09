#' @export 
lancer_application <- function() {  
  appDir <- system.file("APPLICATION", package = "ViCaTraj")  
  shiny::runApp(appDir, display.mode = "normal")
  }
