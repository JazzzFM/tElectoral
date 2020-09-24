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
  muns <- reactive({
    DB_Mich2 %>% select(CABECERA_MUNICIPAL, TOTAL_VOTOS)
  })
  a <- reactive({
    camino_mas_corto(municipios_seleccionados = muns()%>% slice(input$recomendaciones_rows_selected) %>% 
                       pull(CABECERA_MUNICIPAL),
                     info=munRPAP,
                     municipios = DB_Mich2)
  })
  # Mapa de gira
  output$mapa <- renderLeaflet({
    req(a()[[1]])
  })
  
  # Info de la gira
  output$info <- renderUI({
    req(a()[[2]]) %>% paste(collapse = "\n")
  })
  
  # Tabla de recomendaciones
  output$recomendaciones <- DT::renderDT({
    muns()
  })
  
  
}

## To be copied in the UI
# mod_lugaresGira_ui("lugaresGira_ui_1") # Listo

## To be copied in the server
# callModule(mod_lugaresGira_server, "lugaresGira_ui_1") # Listo

