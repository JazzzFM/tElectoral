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
      column(width = 3,
             plotOutput(ns("caja1"))),
      column(width = 3,
            plotOutput(ns("caja2"))),
      column(width = 3,
            plotOutput(ns("caja3"))),
      column(width = 3,
             plotOutput(ns("caja4")))
      
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
    )
  )
}

#' investigacionAnalisis Server Function
#'
#' @noRd 
mod_investigacionAnalisis_server <- function(input, output, session){
  ns <- session$ns
  # Prueba
  output$intervalos <- renderHighchart({
    # fake data
    bd <- tibble(cand1 = rnorm(n = 30, sd = .06, mean = .3),
                 cand2 = rnorm(n = 30, sd = .05, mean = .20),
                 cand3 = rnorm(n = 30, sd = .06, mean = .10),
                 cand4 = rnorm(n = 30, sd = .04, mean = .25),
                 fecha = seq(from = as.Date("2020/12/01"),as.Date("2021/06/25"), by = "week" )) %>%
      gather(candidato, votacion, cand1:cand4) %>%
      mutate(min = votacion-rnorm(mean = .03, sd = .01, n =120),
             max = votacion+rnorm(mean = .03, sd = .01, n =120))
    
    hPollofPolls(bd)
  })
  # Probabilidad de triunfo
  output$intencion <- renderPlot({
    # Temporal: Fake data!!!!!!
    bd <- tibble(cand1 = rnorm(n = 30, sd = .06, mean = .3),
                 cand2 = rnorm(n = 30, sd = .05, mean = .20),
                 cand3 = rnorm(n = 30, sd = .06, mean = .10),
                 cand4 = rnorm(n = 30, sd = .04, mean = .25),
                 fecha = seq(from = as.Date("2020/12/01"),as.Date("2021/06/25"), by = "week" )) %>%
      gather(candidato, votacion, cand1:cand4) %>%
      mutate(min = votacion-rnorm(mean = .03, sd = .01, n =120),
             max = votacion+rnorm(mean = .03, sd = .01, n =120))
    
    iVotoBarras(bd)
  })
  #Probabilidad de triunfo
  
  output$gPdt <- renderPlot({
    # Temporal: Fake data!!!!!!
    nCand <- 3+rpois(1,2)
    cand <- tibble(prob=abs(rnorm(n = nCand,18, 25))) %>% 
      mutate(prob=round(100*prob/sum(prob)), 
             rw=row_number(),
             cand=paste("Candidato", rw))
    # Función
    cand %>% probGanar(candidato = "Candidato 2")
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
  
   output$caja1 <- renderPlot({
     BB <- tibble(x = rnorm(n = 30, sd = .06, mean = .3), y = rnorm(n = 30, sd = .06, mean = .10))
     cajaResume(BB, 1)
   })
  
   output$caja2 <- renderPlot({
     BB <- tibble(x = rnorm(n = 30, sd = .06, mean = .3), y = rnorm(n = 30, sd = .06, mean = .10))
     
     cajaResume(BB, 2)
   })
   
   output$caja3 <- renderPlot({
     BB <- tibble(x = rnorm(n = 30, sd = .06, mean = .3), y = rnorm(n = 30, sd = .06, mean = .10))
     
     cajaResume(BB, 3)
   })
   
   output$caja4 <- renderPlot({
     BB <- tibble(x = rnorm(n = 30, sd = .06, mean = .3), y = rnorm(n = 30, sd = .06, mean = .10))
     
     cajaResume(BB, 4)
   })
}


## To be copied in the UI
# 

## To be copied in the server
# callModule(mod_investigacionAnalisis_server, "investigacionAnalisis_ui_1")

