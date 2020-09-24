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
    tabsetPanel(id = ns("tabsCuestionario"),
    tabPanel(title = "Paso 1", value = "paso1",
             mod_cuestionario_paso_1_ui(ns("cuestionario_paso_1_ui_1"))
    ),
    tabPanel(title = "Paso 2", value = "paso2",
             mod_cuestionario_paso_2_ui(ns("cuestionario_paso_2_ui_1")),
             tags$hr()
       )
    )
  )
}

#' comunicacion Server Function
#'
#' @noRd 
mod_comunicacion_server <- function(input, output, session){
  ns <- session$ns
  tituloBloques <- reactiveValues(titulos = c())
  callModule(mod_cuestionario_paso_1_server, "cuestionario_paso_1_ui_1", tituloBloques)
  callModule(mod_cuestionario_paso_2_server, "cuestionario_paso_2_ui_1", tituloBloques)
}

## To be copied in the UI
# 

## To be copied in the server
# 

