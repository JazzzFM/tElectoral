#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  # Portada
  callModule(mod_portada_server, "portada_ui_1")
  # Investigación
  callModule(mod_investigacionAnalisis_server, "investigacionAnalisis_ui_1")
  

}
