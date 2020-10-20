#' analisisEventos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analisisEventos_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput("barras")
  )
}
    
#' analisisEventos Server Function
#'
#' @noRd 
mod_analisisEventos_server <- function(input, output, session){
  ns <- session$ns
 
  output$barras <- renderPlot({
    fake_data <- tibble(x = 1, y = 2)
    barras(fake_data)
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
