#' cuestionario_paso_5 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cuestionario_paso_5_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Cuestionario registrado con éxito"),
    p("Puede descargar la documentación generada de este cuestionario."),
    actionButton(inputId = ns("descargarDoc"), label = "Descargar")
  )
}
    
#' cuestionario_paso_5 Server Function
#'
#' @noRd 
mod_cuestionario_paso_5_server <- function(input, output, session, parent_session = NULL){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_cuestionario_paso_5_ui("cuestionario_paso_5_ui_1")
    
## To be copied in the server
# callModule(mod_cuestionario_paso_5_server, "cuestionario_paso_5_ui_1")
 
