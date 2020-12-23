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
mod_cuestionario_paso_1_server <- function(input, output, session, cuestionario = c(), bd, usuario ,parent_session = NULL, showListadoForm = NULL, idFormGeneral = NULL, readOnly = NULL, idCuestionario = NULL){
  ns <- session$ns

  observeEvent(input$GuardarPaso1, {
    shinyjs::disable(input$GuardarPaso1)
    check <- c("nivelClaridad",  "operacionalizacion",  "poblacionObjetivo") %>% mandatory(input = input)
    if(!check){
      shinyalert::shinyalert("Incompleto", "Favor de llenar todas las entradas.")
    }else{
      # Tibble
      fA <- lubridate::now(tz = "America/Mexico_City") %>% as.character()
      cuestionario$paso1 <- tibble::tibble(
        idFormGeneral = idFormGeneral$val,
        urlArchivo = "adjunto",
        nivelClaridad = input$nivelClaridad,
        obsNivelClaridad = input$obsNivelClaridad,
        operacionalizacion = input$operacionalizacion,
        obsOperacionalizacion = input$obsOperacionalizacion,
        poblacionObjetivo = input$poblacionObjetivo,
        obsPoblacionObjetivo = input$obsPoblacionObjetivo,
        cantidadBloques = 0,
        obsGenerales = "",
        correo = "",
        fechaAlta = fA,
        usuarioCrea = usuario$user,
        activo = 1
      )
      print(cuestionario$paso1)
    }
    shinyjs::enable(input$GuardarPaso1)
  })
  
  observe({
    if(!is.null(cuestionario$paso1$idCuestionario)){
      shinyjs::hide(id = "GuardarPaso1")
      updatePickerInput(inputId = ns("nivelClaridad"), session = parent_session, selected = cuestionario$paso1$nivelClaridad)
      updatePickerInput(inputId = ns("operacionalizacion"), session = parent_session, selected = cuestionario$paso1$operacionalizacion)
      updatePickerInput(inputId = ns("poblacionObjetivo"), session = parent_session, selected = cuestionario$paso1$poblacionObjetivo)
      
      updateTextAreaInput(inputId = ns("obsNivelClaridad"), session = parent_session, value = cuestionario$paso1$obsNivelClaridad)
      updateTextAreaInput(inputId = ns("obsOperacionalizacion"), session = parent_session, value = cuestionario$paso1$obsOperacionalizacion)
      updateTextAreaInput(inputId = ns("obsPoblacionObjetivo"), session = parent_session, value = cuestionario$paso1$obsPoblacionObjetivo)
    }
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
