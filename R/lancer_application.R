#' @export 
ViCaTraj <- function( ... ) {  
  appDir <- system.file("APPLICATION", package = "ViCaTraj")  
  shiny::runApp(appDir, launch.browser = TRUE, display.mode = "normal", ... )
  }
