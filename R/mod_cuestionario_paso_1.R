#' cuestionario_paso_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyWidgets magrittr
mod_cuestionario_paso_1_ui <- function(id){
  ns <- NS(id)
  tagList(
    hr(),
    # fluidRow(
    #   column(width = 12, 
    #          textInput(inputId = ns("nombreCuestionario"), placeholder = "Ingrese un nombre", label = "Nombre del cuestionario")
    #   ),
    # ),
    div(class="shadowForm",
        fluidRow(
          column(width = 12,
                 fileInput(inputId = ns("urlArchivo"), label = "Adjunte en PDF el archivo sujeto a análisis", accept = "application/pdf",buttonLabel = icon("search"))
          ),
          column(width = 6,
                 pickerInput(inputId = ns("nivelClaridad"), label = "¿Cuál es el nivel de claridad de los objetivos de investigación?", choices = c("Seleccione" = "" , "Alta", "Intermedio", "Bajo", "No hay"))
          ),
          column(width = 6,
                 textAreaInput(inputId = ns("obsNivelClaridad"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
          ),
          column(width = 6,
                 pickerInput(inputId = ns("operacionalizacion"), label = "¿Se operacionalizan todos los objetivos de investigación en los bloques del cuestionario?", choices = c("Seleccione" = "" , "Sí, completos", "Sí, parcialmente", "No"))
          ),
          column(width = 6,
                 textAreaInput(inputId = ns("obsOperacionalizacion"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
          ),
          column(width = 6,
                 pickerInput(inputId = ns("poblacionObjetivo"), label = "¿Hay filtros que permitan identificar correctamente a la población objetivo?", choices = c("Seleccione" = "" , "Sí, completos", "Sí, parcialmente", "No"))
          ),
          column(width = 6,
                 textAreaInput(inputId = ns("obsPoblacionObjetivo"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
          )
        ),
        hr(),
        actionButton(inputId = ns("GuardarPaso1"), "Guardar paso 1", class = "btn-primary"),
    )
    # End bloques del cuestionario
  )
}
    
#' cuestionario_paso_1 Server Function
#'
#' @noRd 
mod_cuestionario_paso_1_server <- function(input, output, session, cuestionario = c()){
  ns <- session$ns

  observeEvent(input$GuardarPaso1, {
    shinyjs::disable(input$GuardarPaso1)
    check <- c("nivelClaridad",  "operacionalizacion",  "poblacionObjetivo") %>% mandatory(input = input)
    if(!check){
      shinyalert::shinyalert("Incompleto", "Favor de llenar todas las entradas.")
    }else{
      # Tibble
      cuestionario$paso1 <- tibble::tibble(
        urlArchivo = "adjunto",
        nivelClaridad = input$nivelClaridad,
        obsNivelClaridad = input$obsNivelClaridad,
        operacionalizacion = input$operacionalizacion,
        obsOperacionalizacion = input$obsOperacionalizacion,
        poblacionObjetivo = input$poblacionObjetivo,
        obsPoblacionObjetivo = input$obsPoblacionObjetivo,
        cantidadBloques = 0,
        obsGenerales = "",
        correo = ""
      )
      print(cuestionario$paso1)
    }
    shinyjs::enable(input$GuardarPaso1)
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
