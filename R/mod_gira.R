#' gira UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_gira_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(title = "Paso 1",
        mod_giraPaso1_ui(ns("giraPaso1_ui_1"))
      ),
      tabPanel(title = "Paso 3",
               mod_giraPaso3_ui(ns("giraPaso3_ui_1"))
      )
    )
  )
}
    
#' gira Server Function
#'
#' @noRd 
mod_gira_server <- function(input, output, session){
  ns <- session$ns
 
  # Paso 1
  callModule(mod_giraPaso1_server, "giraPaso1_ui_1")
  # Paso 3
  callModule(mod_giraPaso3_server, "giraPaso3_ui_1")
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
