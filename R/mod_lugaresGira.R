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
      class = "mt-25 lugaresGiraContainer",
      column(width = 12,
             class = "col-lg-6",
             tags$style(HTML('table.dataTable tbody tr.selected {background: var(--gradient-tr-selected) !important; background-attachment: fixed !important; color: #333} table.dataTable tbody tr.selected td {background-color: transparent !important}')),
             DT::DTOutput(ns("recomendaciones"))
      ),
      column(
        width = 12,
        class = "col-lg-6",
        shinydashboardPlus::boxPlus(
          collapsible = T,
          width = 12,
          fluidRow(
            column(width = 12, class="infoExtraLugaresGira",
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
            )
          )
        ),
        shinydashboardPlus::boxPlus(
          collapsible = T,
          width = 12,
          tabsetPanel(id = "tabsMapaInfoExtra", 
              tabPanel(title = "Mapa (rutas)", 
                       leafletOutput(ns("mapa")),
               ),
              tabPanel(title = "Información extra",
                       uiOutput(ns("info"))
               )
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
mod_lugaresGira_server <- function(input, output, session, gira = NULL, parent_session, reseted){

  ns <- session$ns
  output$responsable <- renderText({gira$paso1$Responsable})
  output$descripcion <- renderText({gira$paso1$Descripcion})
  output$lugarInicio <- renderText({gira$paso1$LugarInicio})
  output$horaInicio <- renderText({gira$paso1$HorarioInicio})
  output$lugarFinal <- renderText({gira$paso1$LugarFinal})
  output$horaFinal <- renderText({gira$paso1$HorarioFinal})
  output$fechaInicio <- renderText({as.character(gira$paso1$FechaInicio)})
  output$fechaFinal <- renderText({as.character(gira$paso1$FechaFinal)})
  actualTableData <- reactiveValues(data = NULL)
  muns <- reactive({
    DB_Mich2 %>% select(CABECERA_MUNICIPAL, TOTAL_VOTOS)
  })
  
  a <- reactive({
    municipios <- actualTableData$data %>% slice(input$recomendaciones_rows_selected) %>% pull(CABECERA_MUNICIPAL)
    camino_mas_corto(municipios_seleccionados = municipios,
                     info=munRPAP, # Info externa
                     municipios = DB_Mich2, # Info externa
                     gira$paso1$LugarInicio,
                     gira$paso1$LugarFinal)
  })
  # Mapa de gira
  output$mapa <- renderLeaflet({
    # Se renderizan lugares 1 y 2
    
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
      showTab(inputId = "TabsGira", target = "paso3", session = parent_session)
      temp <- a()[[3]]
      
      tiempos <- a()[[4]]
      gira$paso2 <- tibble(lugares = temp)
      gira$paso2Tiempos <- tiempos
      
    }else{
      shinyalert::shinyalert(title = "Debe seleccionar al menos un origen y un destino")
    }
  })
  
  # Tabla de recomendaciones
  output$recomendaciones <- DT::renderDT({
    if(!is.null(gira$paso1)){
      fake_visitas <-tibble(CABECERA_MUNICIPAL=sample(size=10, DB_Mich2$CABECERA_MUNICIPAL))
      fake_visitas <-fake_visitas %>% mutate(VISITAS=rpois(n=10,lambda = 1)+1)
      temp <- muns() %>%
        criterio_participacion(DB_VISITAS = fake_visitas, n=5)
      temp <- temp[!(temp$CABECERA_MUNICIPAL== gira$paso1$LugarInicio | temp$CABECERA_MUNICIPAL== gira$paso1$LugarFinal),]
      actualTableData$data <- temp
      DT::datatable(data = temp)
    }
  })
  proxy = DT::dataTableProxy(ns("recomendaciones"))
  observe({
    if(reseted$value){
      proxy %>% DT::selectRows(NULL)
      reseted$resPaso2 <- T
    }
  })
  
  
}

## To be copied in the UI
# mod_lugaresGira_ui("lugaresGira_ui_1") # Listo

## To be copied in the server
# callModule(mod_lugaresGira_server, "lugaresGira_ui_1") # Listo

