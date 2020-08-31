#' investigacionAnalisis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_investigacionAnalisis_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Letreros
    fluidRow(
      valueBox(value = rpois(1,7),subtitle = "encuestas realizadas"),
      valueBox(value = sample(seq(as.Date('1999/01/01'), 
                                  as.Date('2000/01/01'), by="day"), 1),
               subtitle = "última día con encuesta"),
      valueBox(value=as.numeric(as.Date("2021-06-06")-lubridate::today()) ,
               subtitle = "días para la elección")
      ),
    # Gráficos
    fluidRow(
      column(width = 8,
             plotOutput(ns("gPrueba"))),
      column(width = 4,
             plotOutput(ns("gPdt")))
    ),
    
 
  )
}
    
#' investigacionAnalisis Server Function
#'
#' @noRd 
mod_investigacionAnalisis_server <- function(input, output, session){
  ns <- session$ns
  # Prueba
  output$gPrueba <- renderPlot({
    shinipsum::random_ggplot(type = "ribbon")
  })
  # Probabilidad de triunfo
  output$gPdt <- renderPlot({
    # 
    nCand <- 3+rpois(1,2)
    cand <- tibble(prob=abs(rnorm(n = nCand,18, 25))) %>% 
      mutate(prob=round(100*prob/sum(prob)), 
             rw=row_number(),
             cand=paste("Candidato", rw))
    cand %>% probGanar(candidato = "Candidato 2")
  })
}
  
## To be copied in the UI
# 
    
## To be copied in the server
# callModule(mod_investigacionAnalisis_server, "investigacionAnalisis_ui_1")
 
