#' giraPaso3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_giraPaso3_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Paso 3 de gira"),
    actionButton(ns("GuardarPaso3"), "Guardar", class = "btn-primary")
  )
}
    
#' giraPaso3 Server Function
#'
#' @noRd 
mod_giraPaso3_server <- function(input, output, session, gira = NULL){
  ns <- session$ns
  observeEvent(input$GuardarPaso3, {
    gira$paso3 <- "Resultados de paso 3"
    print(gira$paso1)
    print(gira$paso2)
    print(gira$paso3)
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
