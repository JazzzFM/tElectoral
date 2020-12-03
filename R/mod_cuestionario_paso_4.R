#' cuestionario_paso_4 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cuestionario_paso_4_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class="shadowForm",
        fluidRow(
          column(width = 12,
                 p("Aquí irá gráfico de reporte")
          ),
          column(width = 12,
                 textAreaInput(inputId = ns("observacionesGenerales"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
          )
        ),
        hr(),
        actionButton(ns("guardarCuestionario"), "Guardar cuestionario", class="btn btn-primary")
    )
  )
}
    
#' cuestionario_paso_4 Server Function
#'
#' @noRd 
mod_cuestionario_paso_4_server <- function(input, output, session, cuestionario = c(), parent_session = NULL){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_cuestionario_paso_4_ui("cuestionario_paso_4_ui_1")
    
## To be copied in the server
# callModule(mod_cuestionario_paso_4_server, "cuestionario_paso_4_ui_1")
 
