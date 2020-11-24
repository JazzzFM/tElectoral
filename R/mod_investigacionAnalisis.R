#' investigacionAnalisis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import dplyr ggplot2 highcharter tidyr

mod_investigacionAnalisis_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Letreros
    fluidRow(
      column(width = 4,
             valueBoxOutput(ns("caja1"), width = "100%")),
       column(width = 4,
              valueBoxOutput(ns("caja2"), width = "100%")),
       column(width = 4,
              valueBoxOutput(ns("caja3"), width = "100%")),
             ),
    # Gráficos
    fluidRow(
      column(width = 12,
             highchartOutput(ns("intervalos")))
    ),
    fluidRow(
      column(width = 6,
             plotOutput(ns("intencion"))),
      column(width = 6,
             plotOutput(ns("gPdt")))
    ),
    h3("Resultados Diseño Muestral"),
    tags$hr()
  )
}

#' investigacionAnalisis Server Function
#'
#' @noRd 
mod_investigacionAnalisis_server <- function(input, output, session){
  ns <- session$ns
  #Letreros
  output$caja1 <- renderValueBox({
    DB_MichEncuesta %>% nrow() %>% 
      valueBox(subtitle = "Encuestas Realizadas", icon = icon("address-book-o"), color = "light-blue")
  })
  output$caja2 <- renderValueBox({
    start <- datetime <- ymd_hms(now("GMT"))
    end <- ymd_hms("2021-06-06 5:21:00", tz = "GMT")
    d <- as.numeric(round(end - start)) 
    d %>% valueBox(subtitle = "Días para la Elección", icon = icon("calendar"), color = "light-blue")
  })
  output$caja3 <- renderValueBox({
    DB_MichEncuesta %>% select(fecha_final) %>% tail(1) %>%
      valueBox(subtitle = "Fecha de Última Encuesta", icon = icon("calendar-o"), color = "light-blue")
  })
  # Probabilidad de triunfo
  # Pendiente a donde quedar
  output$caja4 <- renderPlot({
    BB <- tibble(x = rnorm(n = 30, sd = .06, mean = .3), y = rnorm(n = 30, sd = .06, mean = .10))
    cajaResume(BB, 4)
  })
  # Prueba
  output$intervalos <- renderHighchart({
    # real data
    bd <- procesamiento_graph(DB_MichEncuesta)
    hPollofPolls(bd)
  })
  # Probabilidad de triunfo
  output$intencion <- renderPlot({
    # Real Data
    bd <- procesamiento_graph(DB_MichEncuesta)
    iVotoBarras(bd)
    })
  #Probabilidad de triunfo
  
  output$gPdt <- renderPlot({
    cand <- procesamiento_graph(DB_MichEncuesta) %>%
            group_by(candidato, colores) %>% summarise() %>%
            mutate(prob = runif(1, min = 0, max = 26))  %>%
            arrange(desc(prob)) %>% ungroup() %>% 
            mutate(cand = candidato, rw = seq(1:9), prob = round(prob)) 
    # Función
    probGanar(cand, candidato = "MORENA", 5)
    })
  
  # output$votopopu <- renderPlot({
  #   # Temporal: Fake data!!!!!!
  #   bd <- tibble(cand1 = rnorm(n = 30, sd = .06, mean = .3),
  #                cand2 = rnorm(n = 30, sd = .05, mean = .20),
  #                cand3 = rnorm(n = 30, sd = .06, mean = .10),
  #                cand4 = rnorm(n = 30, sd = .04, mean = .25),
  #                fecha = seq(from = as.Date("2020/12/01"),as.Date("2021/06/25"), by = "week" )) %>%
  #     gather(candidato, votacion, cand1:cand4) %>%
  #     mutate(min = votacion-rnorm(mean = .03, sd = .01, n =120),
  #            max = votacion+rnorm(mean = .03, sd = .01, n =120))
  #   
  #   hVotoPopu(bd)
  # })
}
## To be copied in the UI
# 

## To be copied in the server
# callModule(mod_investigacionAnalisis_server, "investigacionAnalisis_ui_1")

