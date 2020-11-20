#' cuestionario_paso_3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyWidgets magrittr
mod_cuestionario_paso_3_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    h3("Creación de preguntas"),
    p("De click en Agregar pregunta para añadir una pregunta a un bloque."),
    uiOutput(ns("outPreguntas")),
    actionButton(ns("guardar"), "Guardar")
  )
}
    
#' cuestionario_paso_3 Server Function
#'
#' @noRd 
mod_cuestionario_paso_3_server <- function(input, output, session, cuestionario = NULL, parent_session){
  ns <- session$ns
  #listaPreguntas <- reactiveValues(preguntas = NULL)
  output$outPreguntas <- renderUI({
    if(!is.null(cuestionario$paso1)){
      lapply(seq_along(cuestionario$titulos), function(i) {
        mod_cuestionario_bloques_ui(ns(glue::glue("cuestionario_bloques_ui_{i}")), bloque = cuestionario$titulos[i])
      })
    }
  })
  # observe({
  #   listaPreguntas$preguntas <- seq_along(cuestionario$titulos) %>% map(~callModule(mod_cuestionario_bloques_server,
  #                                                                                   glue::glue("cuestionario_bloques_ui_{.x}"),
  #                                                                                   bloque = cuestionario$titulos[.x],
  #                                                                                   parent_session = parent_session))
  # })
  
  observeEvent(input$guardar, {
    print("paso3")
  })
}
    
## To be copied in the UI
# mod_cuestionario_paso_3_ui("cuestionario_paso_3_ui_1")
    
## To be copied in the server
# callModule(mod_cuestionario_paso_3_server, "cuestionario_paso_3_ui_1")
 
