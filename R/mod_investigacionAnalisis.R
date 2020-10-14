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
    # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
    fluidRow(
      valueBox(value = paste(round(rpois(1,90)), "%", sep=""), subtitle = "Probabilidad de Triunfo", color = "black"),
      valueBox(value = paste(round(rpois(1,30)), "%", sep=""),
               subtitle = "21 municipios 96 secciones", color = "red"),
      valueBox(value = paste(round(rpois(1,30)), "%", sep=""),
               subtitle = "21 municipios 96 secciones", color = "orange"),
      valueBox(value = paste(round(rpois(1,30)), "%", sep=""),
               subtitle = "21 municipios 96 secciones", color = "green")
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
           plotOutput(ns("votopopu")))
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
  
  output$votopopu <- renderPlot({
    # Temporal: Fake data!!!!!!
    bd <- tibble(cand1 = rnorm(n = 30, sd = .06, mean = .3),
                 cand2 = rnorm(n = 30, sd = .05, mean = .20),
                 cand3 = rnorm(n = 30, sd = .06, mean = .10),
                 cand4 = rnorm(n = 30, sd = .04, mean = .25),
                 fecha = seq(from = as.Date("2020/12/01"),as.Date("2021/06/25"), by = "week" )) %>%
      gather(candidato, votacion, cand1:cand4) %>%
      mutate(min = votacion-rnorm(mean = .03, sd = .01, n =120),
             max = votacion+rnorm(mean = .03, sd = .01, n =120))
    
  hVotoPopu(bd)
  })
}
  
## To be copied in the UI
# 
    
## To be copied in the server
# callModule(mod_investigacionAnalisis_server, "investigacionAnalisis_ui_1")
 
