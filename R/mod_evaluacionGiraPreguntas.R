#' evaluacionGiraPreguntas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_evaluacionGiraPreguntas_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Evaluación de gira"),
    p("Seleccione una gira y llene los siguientes campos para completar la evaluación"),
    hr(),
    prettyRadioButtons(inputId = ns("pregunta1"), label = "1.- ¿Considera que el número de asistentes del evento fue adecuado, debían de haber sido más o menos personas?", inline = F, choices = c("Debían de haber sido más personas", "En número de asistentes fue adecuado", "Debían de haber sido menos personas")),
    prettyRadioButtons(inputId = ns("pregunta2"), label = "2.- ¿Con cuál de las siguientes oraciones describiría la actitud de los asistentes?", inline = F, choices = c("La mayoría se percibía concentrada", "La mayoría se percibía ansiosa, incómoda", "La mayoría se percibía comprometida", "La mayoría se percibía distraída", "La mayoría se percibía con ánimo, cómoda", "La mayoría se percibía aburrida")),
    prettyRadioButtons(inputId = ns("pregunta3"), label = "3.- ¿Considera que la duración del evento fue adecuada, debía de haber durado más o menos tiempo?", inline = F, choices = c("Debía de haber durado más tiempo", "La duración fue adecuada", "Debía de haber durado menos tiempo")),
    prettyRadioButtons(inputId = ns("pregunta4"), label = "4.- ¿Cómo percibió el nivel de calidad de los recursos tecnológicos empleados en el evento (audio, internet, iluminación, etc.)?", inline = F, choices = c("Muy buena calidad", "Buena calidad", "Mala calidad", "Muy mala calidad")),
    pickerInput(inputId = ns("pregunta5"), label = "5.- De manera general, en una escala del 0 al 10, donde 0 es no cumplió con ninguna de mis expectativas y 10 cumplió con todas mis expectativas, ¿qué tanto el evento cumplió con sus expectativas previas?", choices = c("Seleccione" = "","1","2","3","4","5","6","7","8","9","10"))
  )
}
    
#' evaluacionGiraPreguntas Server Function
#'
#' @noRd 
mod_evaluacionGiraPreguntas_server <- function(input, output, session, valores = NULL, parent_session = NULL, readonly = 0){
  ns <- session$ns
  out <- reactive({
    tibble(
      pregunta1 = input$pregunta1, 
      pregunta2 = input$pregunta2,
      pregunta3 = input$pregunta3, 
      pregunta4 = input$pregunta4,
      pregunta5 = input$pregunta5)
  })
  
  observeEvent(valores,{
    if(!is.null(valores)){
      updatePrettyRadioButtons(session = parent_session, inputId = ns("pregunta1"), selected = valores$pregunta1)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("pregunta2"), selected = valores$pregunta2)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("pregunta3"), selected = valores$pregunta3)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("pregunta4"), selected = valores$pregunta4)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("pregunta5"), selected = valores$pregunta5)
    }
  })
  
  observeEvent(readonly, {
    if(readonly == 1){
      shinyjs::disable(selector = paste0("#",ns("pregunta1")))
      shinyjs::disable(selector = paste0("#",ns("pregunta2")))
      shinyjs::disable(selector = paste0("#",ns("pregunta3")))
      shinyjs::disable(selector = paste0("#",ns("pregunta4")))
      shinyjs::disable(selector = paste0("#",ns("pregunta5")))
      disable()
    }
  })
  return (out)
}
    
## To be copied in the UI
# mod_evaluacionGiraPreguntas_ui("evaluacionGiraPreguntas_ui_1")
    
## To be copied in the server
# callModule(mod_evaluacionGiraPreguntas_server, "evaluacionGiraPreguntas_ui_1")
 
