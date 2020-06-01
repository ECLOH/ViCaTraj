#' @title Fonction de lancement de l'appliation
#' @param navigateur lancer dans le navigateur web par défaut? 
#' @export 
ViCaTraj <- function(navigateur=TRUE, ... ) {  
  appDir <- system.file("APPLICATION", package = "ViCaTraj")  
  shiny::runApp(appDir, launch.browser = navigateur, display.mode = "normal", ... )
  }
