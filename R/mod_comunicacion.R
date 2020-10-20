#' comunicacion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyWidgets magrittr
mod_comunicacion_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(id = "tabsCuestionario",
    tabPanel(title = "Paso 1", value = "paso1",
             mod_cuestionario_paso_1_ui(ns("cuestionario_paso_1_ui_1"))
    ),
    tabPanel(title = "Paso 2", value = "paso2",
             shinyjs::hidden(div(id = ns("Paso2Cuestionario"),
                 mod_cuestionario_paso_2_ui(ns("cuestionario_paso_2_ui_1")),
                 )),
             tags$hr()
       )
    )
  )
}

#' comunicacion Server Function
#'
#' @noRd 
mod_comunicacion_server <- function(input, output, session, parent_session = NULL){
  ns <- session$ns
  cuestionario <- reactiveValues(titulos = c(), paso1 = NULL, paso2 = NULL)
  # Paso 1
  callModule(mod_cuestionario_paso_1_server, "cuestionario_paso_1_ui_1", cuestionario)
  # Paso 2
  callModule(mod_cuestionario_paso_2_server, "cuestionario_paso_2_ui_1", cuestionario, parent_session)
  observe({
    if(!is.null(cuestionario$paso1)){
      shinyjs::show(selector = paste0("#", ns("Paso2Cuestionario")))
      updateTabsetPanel(inputId = "tabsCuestionario", selected = "paso2", parent_session)
    }
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 

