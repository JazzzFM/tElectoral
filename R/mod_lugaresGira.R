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
      column(width = 12,
             class = "col-lg-6 text-justify",
             h3("Información de gira"),
             h4("Responsable"),
             p("Jesús Selvas"),
             h4("Descripción"),
             p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed efficitur bibendum molestie. Pellentesque eu ligula augue. Ut eu nisl fermentum, placerat tellus a, viverra ipsum. Nullam maximus vel eros sed efficitur. Mauris aliquam ultrices vulputate. Nullam nisl ligula, eleifend vitae velit eget, venenatis venenatis nibh. Nulla faucibus arcu faucibu"),
             h4("Información de ruta"),
             fluidRow(
               column(width = 6, p("Lugar de inicio: Lugar 1"), p("Hora de inicio: 03:35")),
               column(width = 6, p("Lugar de destino: Lugar 2"), p("Hora de finalización: 13:56"))
             ),
             h4("Información extra"),
             # p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed efficitur bibendum molestie. Pellentesque eu ligula augue. Ut eu nisl fermentum, placerat tellus a, viverra ipsum. Nullam maximus vel eros sed efficitur. Mauris aliquam ultrices vulputate. Nullam nisl ligula, eleifend vitae velit eget, venenatis venenatis nibh. Nulla faucibus arcu faucibu")
             uiOutput(ns("info"))
      ),
      column(width = 12,
             class = "col-lg-6",
             leafletOutput(ns("mapa")),
      ),
      column(width = 12,
             tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: var(--m-tr-selected) !important;}')),
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
    camino_mas_corto(municipios_seleccionados = muns()%>% 
                       slice(input$recomendaciones_rows_selected) %>% 
                       pull(CABECERA_MUNICIPAL),
                     info=munRPAP,
                     municipios = DB_Mich2)
  })
  # Mapa de gira
  output$mapa <- renderLeaflet({
    validate(
      need(
        length(input$recomendaciones_rows_selected)>1,
        message = "Favor de seleccionar al menos dos cabeceras municipales de la tabla.")
    )
    a()[[1]]
  })
  
  # Info de la gira
  output$info <- renderUI({
    validate(
      need(
        length(input$recomendaciones_rows_selected)>1,
        message = "Favor de seleccionar al menos dos cabeceras municipales de la tabla.")
    )
    div(class="masonryContent",
        a()[[2]] %>% map(function(x){
          htmltools::HTML("<p>",x,"</p>")
        })
        )
  })
  
  # Tabla de recomendaciones
  output$recomendaciones <- DT::renderDT({
    fake_visitas <-tibble(CABECERA_MUNICIPAL=sample(size=10, DB_Mich2$CABECERA_MUNICIPAL))
    fake_visitas <-fake_visitas %>% mutate(VISITAS=rpois(n=10,lambda = 1)+1)
    muns() %>% criterio_participacion(DB_VISITAS = fake_visitas)
  })
  
  
}

## To be copied in the UI
# mod_lugaresGira_ui("lugaresGira_ui_1") # Listo

## To be copied in the server
# callModule(mod_lugaresGira_server, "lugaresGira_ui_1") # Listo

