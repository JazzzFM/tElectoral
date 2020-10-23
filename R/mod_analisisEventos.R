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

mod_analisisEventos_ui <- function(id){
  ns <- NS(id)
  # Gráfico de Calificación Promedio
  tagList(
    plotOutput(outputId = ns("pGauge")),
  # Gráfico de Estado de Ánimo
    fluidRow(
      column(width = 6,
            plotOutput(ns("eAnimo"))),
      column(width = 6,
             plotOutput(ns("cRecursos")))
    ),
    fluidRow(
      column(width = 6,
             highchartOutput(ns("lCalif"))),
      column(width = 6,
             plotOutput(ns("nAsistentes")))
  ),
  
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
  
  output$eAnimo <- renderPlot({
    # fake data as the real structure
    animo_asist = sample(c('Interesados',
                           'Participativos',
                           'Emocionados',
                           'Desesperados',
                           'Molestos',
                           'Aburridos',
                           'Otro'),
                         size=113, replace=T,
                         prob=c(.1,.1,.2,.1,.1,.1,.2))
    Otro_Anim = sample(c('deprimido',
                         'triste',
                         'ansioso',
                         'borracho'),
                       size=113, replace=T,
                       prob=c(.1,.5,.3,.1))
    
    
    bd<- data.frame(animo_asist, Otro_Anim)
    barras_animo(bd, animo_asist, Otro_Anim, 30)
  })
  
  output$cRecursos <- renderPlot({
    bd <- tibble(asistentes =sample(c("Debían de haber sido más personas", "Adecuado", "Debían haber sido menos personas"),
                                    prob = c(.3, .7, .2), size = 150, replace= T ),
                 recursos =sample(c("Muy buena calidad","Buena calidad", "Mala calidad" , "Muy mala calidad"),
                                  prob = c(.5, .7, .3, .2), size = 150, replace= T ),
                 tiempo =sample(c("Debía de haber durado menos tiempo", "Adecuado", "Debía de haber durado más tiempo"),
                                prob = c(.3, .7, .2), size = 150, replace= T ) )
    paletaRecursos(bd, pregunta = recursos, titulo = "Nivel de calidad de los recursos tecnológicos empleados en el evento")    
  })
  
  output$lCalif <- renderHighchart({
    # Fake data with the real data structure
    bd <- tibble(fecha = seq(from = today(), to = today()+90, by = 10 ),
                 calif = sample(6:10, 10, replace = T),
                 lugares = sample( c("Apodaca", "Cadereyta Jiménez", "El Carmen", "García",
                                     "San Pedro Garza García", "General Escobedo", "Monterrey"),
                                   10, replace = T),
                 personas = sample(500:3000, 10))
    lineaCalificacion(bd , fecha = fecha, calificacion = calif, lugar = lugares, asistentes = personas)
  })

    output$nAsistentes <- renderPlot({
    # fake data as the real structure
    Mun <- select(DB_Mich, c(MUNICIPIO, CABECERA_MUNICIPAL))
    Mun <- Mun %>% mutate(lugar_evento = CABECERA_MUNICIPAL)
    Mun <- select(Mun, c(MUNICIPIO, lugar_evento))
    
    considera_num_asist = sample(c('Debían de haber sido más personas',
                                   'En número de asistentes fue adecuado',
                                   'Debían de haber sido menos personas'),
                                 size=113, replace=T,
                                 prob=c(.3,.3,.3))
    
    db<- data.frame(Mun, considera_num_asist)
    BD <- select(db, c("considera_num_asist"))
    barras_n_assist(BD)
  })
  
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
