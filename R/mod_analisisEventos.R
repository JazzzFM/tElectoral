#' analisisEventos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import ggfittext
#' @import scales
#' @import ggradar
#' @import sf
#' @import glue
#' @import leaflet
#' @import magrittr
#' @import stringr

mod_analisisEventos_ui <- function(id){
  ns <- NS(id)
  # Gráfico de Calificación Promedio
  tagList(
    h3(style="text-align:center", "Evaluación general de la gira"),
    fluidRow(
      column(width = 12,
             fluidRow(
               column(width = 12, 
                      div(class="topBoxInfo bordered-white ft-sm",
                          plotOutput(outputId = ns("pGauge")),
                          p("Calificación promedio")
                      )
               ),
               column(width = 4, 
                      div(class="topBoxInfo bordered-green",
                          p("26"),
                          p("Eventos")
                      )
               ),
               column(width = 4, 
                      div(class="topBoxInfo default",
                          p("52%"),
                          p("Visitas prioritarias realizadas"),
                          p("16 eventos")
                      )
               ),
               column(width = 4, 
                      div(class="topBoxInfo red",
                          p("4"),
                          p("Incidentes")
                      )
               ),
             )
       )
    ),
    fluidRow(
      column(width = 6,
             div(class="topBoxInfo tableForAnalisisGira",
                 DT::DTOutput(ns("tableMun"))
             )
      ),
      column(width = 6, plotOutput(ns("eAnimo")))
    ),
    # Gráfico de Estado de Ánimo
    fluidRow(
      column(width = 12,
             highchartOutput(ns("lCalif"))
      )
    ),
    fluidRow(
      column(width = 6,
             plotOutput(ns("cRecursos"))),
      column(width = 6,
             plotOutput(ns("nAsistentes")))
    ),
    fluidRow(
      column(width = 12,
             fluidRow(
               column(width = 12, 
                      div(class="topBoxInfo bordered-white ft-sm",
                          h3(style="text-align:left", "Representantes de Casilla por Municipio"),
                          leafletOutput(ns("llmapa")))
                    ))
            )) 
  )}
#' analisisEventos Server Function
#'
#' @noRd 
mod_analisisEventos_server <- function(input, output, session, bd){
  ns <- session$ns
  
  output$pGauge <- renderPlot({
    # bd <- tibble(x = sample(0:10, size = 20, replace = T))
    promedioGauge(bd$evaluacionEventos, calificacion = expectativas)
  })
  
  output$tableMun <- DT::renderDT({
    mun <- tibble(CABECERA_MUNICIPAL = sample(size=3, DB_Mich2$CABECERA_MUNICIPAL)) 
    a <- tibble(Municipio = mun$CABECERA_MUNICIPAL, Calificación = c(a = 10, b = 5, c= 8))
     DT::datatable(data = a)
   }, escape = F, options = list(dom = 't'))
  
  output$eAnimo <- renderPlot({
    distRadar(bd$evaluacionEvento, pregunta = actitud, otro = actitud_otro, x = 0, titulo = "Ánimo de los Asistentes") 
  })
  
  output$cRecursos <- renderPlot({
    
    paletaRecursos(bd$evaluacionEventos, pregunta = calidadTecno, 
                   titulo = str_wrap("Nivel de calidad de los recursos tecnológicos", 30))
  })
  
  output$lCalif <- renderHighchart({
    
    lineaCalificacion(bd$evaluacionEventos, 
                      fecha = fechaAlta, calificacion = expectativas, 
                      lugar = actitud, asistentes = asistentes)
  })
  
  output$nAsistentes <- renderPlot({
    # fake data as the real structure
    burbujas(bd$evaluacionEventos, pregunta1 = asistentes, pregunta2 = duracion)
  })
  
  output$llmapa <- renderLeaflet({
    llMapaEstado(DB_MichGeograf)
  })
  
}

## To be copied in the UI
# 

## To be copied in the server
# 

