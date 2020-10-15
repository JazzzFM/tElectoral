#' lugaresGira UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import leaflet tidyr TSP shinydashboardPlus
mod_lugaresGira_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      class = "mt-25",
      column(width = 12,
             class = "col-lg-6",
             tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: var(--m-tr-selected) !important;}')),
             DT::DTOutput(ns("recomendaciones"))
      ),
      column(
        width = 12,
        class = "col-lg-6",
        shinydashboardPlus::boxPlus(
          collapsible = T,
          width = 12,
          fluidRow(
            column(width = 12,
                   class = "text-justify",
                   h3("Información de gira"),
                   h4("Responsable"),
                   textOutput(ns("responsable")),
                   h4("Descripción"),
                   textOutput(ns("descripcion")),
                   h4("Información de ruta"),
                   fluidRow(
                     column(width = 6, p(paste0("Lugar de inicio: "), textOutput(ns("lugarInicio"))), p(paste0("Fecha de inicio: "), textOutput(ns("fechaInicio"))), p(paste0("Hora de inicio: "), textOutput(ns("horaInicio")))),
                     column(width = 6, p(paste0("Lugar de destino: "), textOutput(ns("lugarFinal"))), p(paste0("Fecha de finalización: "), textOutput(ns("fechaFinal"))), p(paste0("Hora de finalización: "), textOutput(ns("horaFinal")))),
                   )
            ),
            column(width = 12,
                   h4("Información extra"),
                   uiOutput(ns("info"))
            )
          )
        ),
        shinydashboardPlus::boxPlus(
          collapsible = T,
          width = 12,
          fluidRow(
            column(
              width = 12,
              leafletOutput(ns("mapa")),
            ),
          )
        )
      )
    ),
    actionButton(ns("GuardarPaso2"), "Guardar", class = "btn-primary")
  )
}

#' lugaresGira Server Function
#'
#' @noRd 
mod_lugaresGira_server <- function(input, output, session, gira = NULL){
  ns <- session$ns
  output$responsable <- renderText({gira$paso1$Responsable})
  output$descripcion <- renderText({gira$paso1$Descripcion})
  output$lugarInicio <- renderText({gira$paso1$LugarInicio})
  output$horaInicio <- renderText({gira$paso1$HorarioInicio})
  output$lugarFinal <- renderText({gira$paso1$LugarFinal})
  output$horaFinal <- renderText({gira$paso1$HorarioFinal})
  output$fechaInicio <- renderText({as.character(gira$paso1$FechaInicio)})
  output$fechaFinal <- renderText({as.character(gira$paso1$FechaFinal)})
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
  observeEvent(input$GuardarPaso2,{
    if(length(input$recomendaciones_rows_selected)>1){
      gira$paso2 <- tibble(lugares = sort(muns()%>% slice(input$recomendaciones_rows_selected) %>% 
                                            pull(CABECERA_MUNICIPAL)))
    }else{
      shinyalert::shinyalert(title = "Debe seleccionar al menos un origen y un destino")
    }
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

