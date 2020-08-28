#' portada UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_portada_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("uiPortada"))
 
  )
}
    
#' portada Server Function
#'
#' @noRd 
mod_portada_server <- function(input, output, session){
  ns <- session$ns
  output$uiPortada <- renderUI({
    includeHTML(path = file.path("inst/app/PanelElectoral/index.html"))
    })
 
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# callModule(mod_portada_server, "portada_ui_1")
 
