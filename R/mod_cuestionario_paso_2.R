#' cuestionario_paso_2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyWidgets magrittr
mod_cuestionario_paso_2_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    h3("Creación de preguntas"),
    p("De click en Agregar pregunta para añadir una pregunta a un bloque."),
    uiOutput(ns("outPreguntas")),
    actionButton(ns("guardar"), "Guardar", class = "btn-definitive")
  )
}

#' cuestionario_paso_2 Server Function
#'
#' @noRd 
mod_cuestionario_paso_2_server <- function(input, output, session, cuestionario = NULL, parent_session){
  ns <- session$ns
 
  
  listaPreguntas <- reactiveValues(preguntas = NULL)
  output$outPreguntas <- renderUI({
    if(!is.null(cuestionario$paso1)){
      lapply(seq_along(cuestionario$titulos), function(i) {
        mod_cuestionario_bloques_ui(ns(glue::glue("cuestionario_bloques_ui_{i}")), bloque = cuestionario$titulos[i])
      }) 
    }
  })
  observe({
    listaPreguntas$preguntas <- seq_along(cuestionario$titulos) %>% map(~callModule(mod_cuestionario_bloques_server,
                                                                              glue::glue("cuestionario_bloques_ui_{.x}"),
                                                                              bloque = cuestionario$titulos[.x],
                                                                              parent_session = parent_session))
  })
  
  observeEvent(input$guardar, {
    browser()
    if(is.null(seq_along(listaPreguntas$preguntas) %>% detect(~is.null(listaPreguntas$preguntas[[.x]]())))){
      if(is.null(seq_along(listaPreguntas$preguntas) %>% detect(~nrow(listaPreguntas$preguntas[[.x]]()) == 0))){
        seq_along(listaPreguntas$preguntas) %>% map(~listaPreguntas$preguntas[[.x]]()) %>% do.call(rbind,.)  
      }else{
        shinyalert::shinyalert(title = "Debe añadir al menos una pregunta por bloque")  
      } 
    }else{
      shinyalert::shinyalert(title = "Debe añadir al menos una pregunta por bloque")  
    }
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 