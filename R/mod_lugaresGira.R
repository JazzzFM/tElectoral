#' lugaresGira UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import leaflet tidyr TSP
mod_lugaresGira_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 3,
             uiOutput(ns("info"))
             
      ),
      column(width = 9,
             leafletOutput(ns("mapa")),
             DT::DTOutput(ns("recomendaciones")))
    )
  )
}
    
#' lugaresGira Server Function
#'
#' @noRd 
mod_lugaresGira_server <- function(input, output, session){
  ns <- session$ns
  # Temporal seleccion de municipios al azar
  mun_sel <- DB_Mich2$CABECERA_MUNICIPAL %>% sample(size = 3)
  a <- camino_mas_corto(municipios_seleccionados = mun_sel,
                        info=munRPAP,
                        municipios = DB_Mich2)
  # Mapa de gira
  output$mapa <- renderLeaflet({
    a[[1]]
  }
  )
  
  # Tabla de sugerencias
  output$recomendaciones <- DT::renderDT({
    DB_Mich2 %>% select(MUNICIPIO, TOTAL_VOTOS)
  })
  
  # Info de la gira
  output$info <- renderUI({
    a[[2]] %>% paste(collapse = "\n")
  })
}
    
## To be copied in the UI
# mod_lugaresGira_ui("lugaresGira_ui_1") # Listo
    
## To be copied in the server
# callModule(mod_lugaresGira_server, "lugaresGira_ui_1") # Listo
 
