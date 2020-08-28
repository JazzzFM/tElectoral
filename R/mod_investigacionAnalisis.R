#' investigacionAnalisis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_investigacionAnalisis_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("gPrueba"))
 
  )
}
    
#' investigacionAnalisis Server Function
#'
#' @noRd 
mod_investigacionAnalisis_server <- function(input, output, session){
  ns <- session$ns
  output$gPrueba <- renderPlot({
    shinipsum::random_ggplot()
  })
 
}
  
## To be copied in the UI
# 
    
## To be copied in the server
# callModule(mod_investigacionAnalisis_server, "investigacionAnalisis_ui_1")
 
