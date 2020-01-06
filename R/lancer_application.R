#' @export 
ViCaTraj <- function() {  
  appDir <- system.file("APPLICATION", package = "ViCaTraj")  
  shiny::runApp(appDir, display.mode = "normal")
  }
