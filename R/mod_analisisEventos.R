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
    fluidRow(
      column(width = 5,
             fluidRow(
                 column(width = 12,
                        div(class="shadowForm",
                            h3(style="text-align:center", "Evaluación general de la gira"),
                            div(class="topBoxInfo bordered-white ft-sm",
                                plotOutput(outputId = ns("pGauge"))
                            )
                        )
                 )
             )
       ),
      column(width = 7,
             fluidRow(
               column(width = 12,
                      div(class="shadowForm",
                          h3(style="text-align:center", "Clasificación promedio"),
                          div(class="calificacionContainer",
                              div(class="topBoxInfo default",
                                  textOutput(ns('numEventos')),
                                  p("Eventos")
                              ),
                              div(class="topBoxInfo default",
                                  textOutput(ns('eventosPct')),
                                  p("Municipios visitados")
                              ),
                              div(class="topBoxInfo default",
                                  textOutput(ns('numIncidentes')),
                                  p("Incidentes")
                              )
                          )
                      )
                )
             )
       )
    ),
    fluidRow(
      column(width = 6,
             div(class="topBoxInfo tableForAnalisisGira",
                 DT::DTOutput(ns("tableMun"))
             )
      ),
      column(width = 6, 
             div(class="shadowForm",
                 plotOutput(ns("eAnimo"))
             )
       )
    ),
    # Gráfico de Estado de Ánimo
    fluidRow(
      column(width = 12,
             div(class="shadowForm",
                 highchartOutput(ns("lCalif"))
             )
      )
    ),
    fluidRow(
      column(width = 6,
             div(class="shadowForm",
                 plotOutput(ns("cRecursos"))
             )
       ),
      column(width = 6,
             div(class="shadowForm",
                 plotOutput(ns("nAsistentes"))
            )
       )
    ),
    fluidRow(
      column(width = 12,
             fluidRow(
               column(width = 12, 
                      div(class="topBoxInfo bordered-white ft-sm shadowForm",
                          h3(style="text-align:center", "Número de Eventos Realizados y en Proceso"),
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
  
  output$numEventos <- renderText({
    as.character( bd$eventos %>%  nrow())
  })
  
  output$eventosPct <- renderText({
    as.character( bd$eventos %>%count(lugar) %>%  nrow()  )
  })
  
  output$numIncidentes <- renderText({
    as.character( bd$evaluacionEventos %>% summarise(n =sum(incidentes)) %>% pull(n)  )
  })
  
  output$tableMun <- DT::renderDT({
    mun <- tibble(CABECERA_MUNICIPAL = sample(size=3, DB_Mich2$CABECERA_MUNICIPAL)) 
    a <- tibble(Municipio = mun$CABECERA_MUNICIPAL, Calificación = c(a = 10, b = 5, c= 8))
     DT::datatable(data = a)
   }, escape = F, options = list(dom = 't', language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json',searchPlaceholder = "Buscar...")))
  
  output$eAnimo <- renderPlot({
    distRadar(bd$evaluacionEventos, pregunta = actitud, otro = actitudOtro, x = 0, titulo = "Ánimo de los asistentes") 
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

