#' lugaresGira UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import leaflet
mod_lugaresGira_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 3,
             "Info de la gira"
      ),
      column(width = 9,
             "Mapa",
             leafletOutput(ns("mapa")),
             "Recomendaciones",
             DT::DTOutput(ns("recomendaciones")))
    )
  )
}
    
#' lugaresGira Server Function
#'
#' @noRd 
mod_lugaresGira_server <- function(input, output, session){
  ns <- session$ns
  # Mapa de gira
  output$mapa <- renderLeaflet({
    leaflet() %>% 
      addTiles()
  }
  )
  
  # Tabla de sugerencias
  output$recomendaciones <- DT::renderDT({
    DB_Mich2 %>% select(MUNICIPIO, TOTAL_VOTOS)
  })
}
    
## To be copied in the UI
# mod_lugaresGira_ui("lugaresGira_ui_1") # Listo
    
## To be copied in the server
# callModule(mod_lugaresGira_server, "lugaresGira_ui_1") # Listo
 
