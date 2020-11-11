#' investigacionFormularioDisMuestral UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_investigacionFormularioDisMuestral_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Form diseño muestral")
  )
}
    
#' investigacionFormularioDisMuestral Server Function
#'
#' @noRd 
mod_investigacionFormularioDisMuestral_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_investigacionFormularioDisMuestral_ui("investigacionFormularioDisMuestral_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionFormularioDisMuestral_server, "investigacionFormularioDisMuestral_ui_1")
 
