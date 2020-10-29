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

mod_analisisEventos_ui <- function(id){
  ns <- NS(id)
  # Gráfico de Calificación Promedio
  tagList(
    h3("Evaluación general de la gira"),
    fluidRow(
      column(width = 4, plotOutput(outputId = ns("pGauge"))),
      column(width = 4, 
             div(class="topBoxInfo",
                 p("52%"),
                 p("Visitas prioritarias realizadas"),
                 p("16 eventos")
                 )
       ),
      column(width = 4,
             div(class="tableMunicipios",
                 DT::DTOutput(ns("tableMun"))
             )
       )
    ),
    fluidRow(
      column(width = 6,
             div(class="ViewBoxCustom",
                 div(class="topInfo",
                     p("Eventos"),
                     p("32")
                 ),
                 div(class="wallInfo forOneItem",
                     p("100% de los municipios")
                 )
             ),
             div(class="ViewBoxCustom",
                 div(class="topInfo",
                     p("Incidentes"),
                     p("4")
                 ),
                 div(class="wallInfo",
                     p("Evento 1"),
                     p("Evento 3"),
                     p("Evento 8"),
                     p("Evento 12"),
                 )
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
           plotOutput(ns("ggmapa")))
    # column(width = 6,
    #        plotOutput(ns("llmapa")))
    )
  )
}
    
#' analisisEventos Server Function
#'
#' @noRd 
mod_analisisEventos_server <- function(input, output, session){
  ns <- session$ns
 
  output$pGauge <- renderPlot({
  bd <- tibble(x = sample(0:10, size = 20, replace = T))
  promedioGauge(bd, calificacion = x)
  })
  
  a <- tibble(Municipio = c(a = "Municipio 1", b = "Municipio 2", c= "Municipio 3"), Calificación = c(a = 10, b = 5, c= 8))
  output$tableMun <- DT::renderDT({
    DT::datatable(
      data = a,
      options = list(dom = 't')
    )
  }, escape = F)
  
  output$eAnimo <- renderPlot({
    # fake data as the real structure
    bd <- select(DB_Mich, c(MUNICIPIO, lugar =CABECERA_MUNICIPAL)) %>%
      mutate(fecha= seq(from = dmy_hm("06-01-21 11:00"), to = dmy_hm("06-02-21 11:00"), length.out =113),
             asistentes = sample(c("Debían de haber sido más personas",
                                   "Adecuado",
                                   "Debían haber sido menos personas"),
                                 prob = c(.3, .7, .2), size = 113, replace= T ),
             animo = sample(c('Interesados',
                              'Participativos',
                              'Emocionados',
                              'Desesperados',
                              'Molestos',
                              'Aburridos',
                              'Otro'),
                            size=113, replace=T,
                            prob=c(.1,.1,.2,.1,.1,.1,.2)),
             
             animo_otro = sample(c('deprimido',
                                   'triste',
                                   'ansioso',
                                   'borracho'),
                                 size=113, replace=T),
             
             incidente = sample(c('Perdida del sonido o audio', 'Perdida de corriente eléctrica',
                                  'Los asistentes no llegaron a tiempo','El recinto no era adecuado',
                                  'Insultos por parte de los participantes', 'Enfrentamiento entre participantes',
                                  'Salida repentina de participantes antes de que finalice el evento',
                                  'Constantes interrupciones al discurso del candidato',
                                  'Hackeo del evento (en caso de ser virtual)', 'Otro'),
                                size=113, replace=T),
             incidente_otro = sample(c('Se congelaba la pantalla',
                                       'Hubo balazos',
                                       'Se rompió el templete',
                                       'Las autoridad local no lo permitió'),
                                     size=113, replace=T,
                                     prob=c(.3,.1,.2,.2)),
             
             duracion = sample(c("Debía de haber durado menos tiempo",
                                 "Adecuado", "Debía de haber durado más tiempo"),
                               prob = c(.3, .7, .2), size = 113, replace= T ),
             
             calidad =  sample(c("Muy buena calidad",
                                 "Buena calidad",
                                 "Mala calidad" ,
                                 "Muy mala calidad"),
                               prob = c(.5, .7, .3, .2), size = 113, replace= T ),
             calif = sample(c(0:10), size = 113, replace = T,
                            prob=c(.005,.01,.02,.1,.2,.3,.4,.6,.5,.4,.3))
      )
    
    distRadar(bd, pregunta = animo, otro = animo_otro, x = 30, titulo = "Animo de los asistentes") 
      })
  
  output$cRecursos <- renderPlot({
    bd <- select(DB_Mich, c(MUNICIPIO, lugar =CABECERA_MUNICIPAL)) %>%
      mutate(fecha= seq(from = dmy_hm("06-01-21 11:00"), to = dmy_hm("06-02-21 11:00"), length.out =113),
             asistentes = sample(c("Debían de haber sido más personas",
                                   "Adecuado",
                                   "Debían haber sido menos personas"),
                                 prob = c(.3, .7, .2), size = 113, replace= T ),
             animo = sample(c('Interesados',
                              'Participativos',
                              'Emocionados',
                              'Desesperados',
                              'Molestos',
                              'Aburridos',
                              'Otro'),
                            size=113, replace=T,
                            prob=c(.1,.1,.2,.1,.1,.1,.2)),
             
             animo_otro = sample(c('deprimido',
                                   'triste',
                                   'ansioso',
                                   'borracho'),
                                 size=113, replace=T),
             
             incidente = sample(c('Perdida del sonido o audio', 'Perdida de corriente eléctrica',
                                  'Los asistentes no llegaron a tiempo','El recinto no era adecuado',
                                  'Insultos por parte de los participantes', 'Enfrentamiento entre participantes',
                                  'Salida repentina de participantes antes de que finalice el evento',
                                  'Constantes interrupciones al discurso del candidato',
                                  'Hackeo del evento (en caso de ser virtual)', 'Otro'),
                                size=113, replace=T),
             incidente_otro = sample(c('Se congelaba la pantalla',
                                       'Hubo balazos',
                                       'Se rompió el templete',
                                       'Las autoridad local no lo permitió'),
                                     size=113, replace=T,
                                     prob=c(.3,.1,.2,.2)),
             
             duracion = sample(c("Debía de haber durado menos tiempo",
                                 "Adecuado", "Debía de haber durado más tiempo"),
                               prob = c(.3, .7, .2), size = 113, replace= T ),
             
             calidad =  sample(c("Muy buena calidad",
                                 "Buena calidad",
                                 "Mala calidad" ,
                                 "Muy mala calidad"),
                               prob = c(.5, .7, .3, .2), size = 113, replace= T ),
             calif = sample(c(0:10), size = 113, replace = T,
                            prob=c(.005,.01,.02,.1,.2,.3,.4,.6,.5,.4,.3))
      )
    
  paletaRecursos(bd, pregunta = calidad, titulo = "Nivel de calidad de los recursos tecnológicos empleados en el evento")
  })
  
  output$lCalif <- renderHighchart({
    # Fake data with the real data structure
    bd <- select(DB_Mich, c(MUNICIPIO, lugar =CABECERA_MUNICIPAL)) %>%
      mutate(fecha= seq(from = dmy_hm("06-01-21 11:00"), to = dmy_hm("06-02-21 11:00"), length.out =113),
             asistentes = sample(c("Debían de haber sido más personas",
                                   "Adecuado",
                                   "Debían haber sido menos personas"),
                                 prob = c(.3, .7, .2), size = 113, replace= T ),
             animo = sample(c('Interesados',
                              'Participativos',
                              'Emocionados',
                              'Desesperados',
                              'Molestos',
                              'Aburridos',
                              'Otro'),
                            size=113, replace=T,
                            prob=c(.1,.1,.2,.1,.1,.1,.2)),
             
             animo_otro = sample(c('deprimido',
                                   'triste',
                                   'ansioso',
                                   'borracho'),
                                 size=113, replace=T),
             
             incidente = sample(c('Perdida del sonido o audio', 'Perdida de corriente eléctrica',
                                  'Los asistentes no llegaron a tiempo','El recinto no era adecuado',
                                  'Insultos por parte de los participantes', 'Enfrentamiento entre participantes',
                                  'Salida repentina de participantes antes de que finalice el evento',
                                  'Constantes interrupciones al discurso del candidato',
                                  'Hackeo del evento (en caso de ser virtual)', 'Otro'),
                                size=113, replace=T),
             incidente_otro = sample(c('Se congelaba la pantalla',
                                       'Hubo balazos',
                                       'Se rompió el templete',
                                       'Las autoridad local no lo permitió'),
                                     size=113, replace=T,
                                     prob=c(.3,.1,.2,.2)),
             
             duracion = sample(c("Debía de haber durado menos tiempo",
                                 "Adecuado", "Debía de haber durado más tiempo"),
                               prob = c(.3, .7, .2), size = 113, replace= T ),
             
             calidad =  sample(c("Muy buena calidad",
                                 "Buena calidad",
                                 "Mala calidad" ,
                                 "Muy mala calidad"),
                               prob = c(.5, .7, .3, .2), size = 113, replace= T ),
             calif = sample(c(0:10), size = 113, replace = T,
                            prob=c(.005,.01,.02,.1,.2,.3,.4,.6,.5,.4,.3))
      )
    
    lineaCalificacion(bd, fecha = fecha, calificacion = calif, lugar = lugar, asistentes = asistentes)
  })

    output$nAsistentes <- renderPlot({
    # fake data as the real structure
      bd <- select(DB_Mich, c(MUNICIPIO, lugar =CABECERA_MUNICIPAL)) %>%
        mutate(fecha= seq(from = dmy_hm("06-01-21 11:00"), to = dmy_hm("06-02-21 11:00"), length.out =113),
               asistentes = sample(c("Debían de haber sido más personas",
                                     "Adecuado",
                                     "Debían haber sido menos personas"),
                                   prob = c(.3, .7, .2), size = 113, replace= T ),
               animo = sample(c('Interesados',
                                'Participativos',
                                'Emocionados',
                                'Desesperados',
                                'Molestos',
                                'Aburridos',
                                'Otro'),
                              size=113, replace=T,
                              prob=c(.1,.1,.2,.1,.1,.1,.2)),
               
               animo_otro = sample(c('deprimido',
                                     'triste',
                                     'ansioso',
                                     'borracho'),
                                   size=113, replace=T),
               
               incidente = sample(c('Perdida del sonido o audio', 'Perdida de corriente eléctrica',
                                    'Los asistentes no llegaron a tiempo','El recinto no era adecuado',
                                    'Insultos por parte de los participantes', 'Enfrentamiento entre participantes',
                                    'Salida repentina de participantes antes de que finalice el evento',
                                    'Constantes interrupciones al discurso del candidato',
                                    'Hackeo del evento (en caso de ser virtual)', 'Otro'),
                                  size=113, replace=T),
               incidente_otro = sample(c('Se congelaba la pantalla',
                                         'Hubo balazos',
                                         'Se rompió el templete',
                                         'Las autoridad local no lo permitió'),
                                       size=113, replace=T,
                                       prob=c(.3,.1,.2,.2)),
               
               duracion = sample(c("Debía de haber durado menos tiempo",
                                   "Adecuado", "Debía de haber durado más tiempo"),
                                 prob = c(.3, .7, .2), size = 113, replace= T ),
               
               calidad =  sample(c("Muy buena calidad",
                                   "Buena calidad",
                                   "Mala calidad" ,
                                   "Muy mala calidad"),
                                 prob = c(.5, .7, .3, .2), size = 113, replace= T ),
               calif = sample(c(0:10), size = 113, replace = T,
                              prob=c(.005,.01,.02,.1,.2,.3,.4,.6,.5,.4,.3))
        )
    burbujas(bd, pregunta1 = asistentes, pregunta2 = duracion)
  })
    
    output$ggmapa <- renderPlot({
      #MICH <- st_read("~/GerenciaPoder/Mapa/MUNICIPIO.shp",options = "ENCODING=WINDOWS-1252")
      ggMapaEstado(MICH)
    })
    
    output$llmapa <- renderLeaflet({
      #MICH <- st_read("~/GerenciaPoder/Mapa/MUNICIPIO.shp",options = "ENCODING=WINDOWS-1252")
      llMapaEstado(MICH)
    })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
