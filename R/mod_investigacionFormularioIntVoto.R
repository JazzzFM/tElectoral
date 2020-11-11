#' investigacionFormularioIntVoto UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_investigacionFormularioIntVoto_ui <- function(id){
  ns <- NS(id)
  tagList(
  h3("Form intención de voto")
  )
}
    
#' investigacionFormularioIntVoto Server Function
#'
#' @noRd 
mod_investigacionFormularioIntVoto_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_investigacionFormularioIntVoto_ui("investigacionFormularioIntVoto_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionFormularioIntVoto_server, "investigacionFormularioIntVoto_ui_1")
 
