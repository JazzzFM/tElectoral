#' investigacionEncuestas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_investigacionEncuestas_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Encuestas")
  )
}
    
#' investigacionEncuestas Server Function
#'
#' @noRd 
mod_investigacionEncuestas_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_investigacionEncuestas_ui("investigacionEncuestas_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionEncuestas_server, "investigacionEncuestas_ui_1")
 
