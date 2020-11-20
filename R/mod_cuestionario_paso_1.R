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
    fluidRow(
      column(width = 12, 
             textInput(inputId = ns("nombreCuestionario"), placeholder = "Ingrese un nombre", label = "Nombre del cuestionario")
      ),
    ),
    fluidRow(
      column(width = 12,
        fileInput(inputId = ns("adjunto"), label = "Adjunte en PDF el archivo sujeto a análisis", accept = "application/pdf",buttonLabel = "Click aqui para buscar")
      ),
      column(width = 6,
             pickerInput(inputId = ns("nivelClaridad"), label = "¿Cuál es el nivel de claridad de los objetivos de investigación?", choices = c("Seleccione" = "" , "Alta", "Intermedio", "Bajo", "No hay"))
      ),
      column(width = 6,
             textAreaInput(inputId = ns("observacionClaridad"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      ),
      column(width = 6,
             pickerInput(inputId = ns("opBloquesCuestionario"), label = "¿Se operacionalizan todos los objetivos de investigación en los bloques del cuestionario?", choices = c("Seleccione" = "" , "Sí, completos", "Sí, parcialmente", "No"))
      ),
      column(width = 6,
             textAreaInput(inputId = ns("observacionOpBloquesCuestionario"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      ),
      column(width = 6,
             pickerInput(inputId = ns("filtrosPoblacionObjetivo"), label = "¿Hay filtros que permitan identificar correctamente a la población objetivo?", choices = c("Seleccione" = "" , "Sí, completos", "Sí, parcialmente", "No"))
      ),
      column(width = 6,
             textAreaInput(inputId = ns("observacionFiltrosPoblacionObjetivo"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      )
    ),
    
    actionButton(inputId = ns("GuardarPaso1"), "Guardar", class = "btn-primary"),
    # End bloques del cuestionario
  )
}
    
#' cuestionario_paso_1 Server Function
#'
#' @noRd 
mod_cuestionario_paso_1_server <- function(input, output, session, cuestionario = c()){
  ns <- session$ns
  # observeEvent(input$ConBasicos,{
  #   removeUI(
  #     selector = ".ColumnEspecifique"
  #   ) 
  #   for (value in input$ConBasicos) {
  #     if(value == "Otro"){
  #       insertUI(
  #         selector = '.RowInvestigacion', 
  #         where = "beforeEnd",
  #         ui = column(width = 6, 
  #                     class="ColumnEspecifique",
  #                     textInput(inputId = "Especifique", placeholder = "...", label = "Especifique"),
  #                     actionButton(inputId = ns("CancelarOtro"), "Cancelar", class = "btn-default"),
  #                     actionButton(inputId = "Guardar", "Guardar concepto", class = "btn-primary"),
  #         )
  #       )
  #       break;
  #     }
  #   }
  # }, ignoreNULL = F)
  # observeEvent(input$CancelarOtro, {
  #   removeUI(
  #     selector = ".ColumnEspecifique"
  #   )
  #   # x <- input$ConBasicos
  #   # remove <- c("Otro")
  #   # setdiff(x, remove)
  #   #updateSelectizeInput(session = session, inputId = ns("ConBasicos"), selected = NULL, server = T)
  # })
  # Guardar cuestionario
  
  observeEvent(input$GuardarPaso1, {
    #shinyjs::disable(input$GuardarPaso1)
    check <- c("nombreCuestionario", "nivelClaridad",  "opBloquesCuestionario",  "filtrosPoblacionObjetivo") %>% mandatory(input = input)
    if(!check){
      shinyalert::shinyalert("Incompleto", "Favor de llenar todas las entradas.")
    }else{
      # Tibble
      cuestionario$paso1 <- tibble::tibble(
        nombreCuestionario = input$nombreCuestionario,
        adjunto = "adjunto",
        nivelClaridad = input$nivelClaridad,
        observacionClaridad = input$observacionClaridad,
        opBloquesCuestionario = input$opBloquesCuestionario,
        observacionOpBloquesCuestionario = input$observacionOpBloquesCuestionario,
        filtrosPoblacionObjetivo = input$filtrosPoblacionObjetivo,
        observacionFiltrosPoblacionObjetivo = input$observacionFiltrosPoblacionObjetivo
      )
    }
    #shinyjs::enable(input$GuardarPaso1)
  })
  
  # Generar bloques
  # observeEvent(input$genBloques, {
  #   value <- input$CantidadBloques
  #   removeUI(
  #     selector = ".NombreBloque",
  #     multiple = T
  #   ) 
  #   if(value > 0){
  #     for (item in 1:value) {
  #       insertUI(
  #         selector = '.NombreBloquesContainer', 
  #         where = "beforeEnd",
  #         ui = column(class="NombreBloque",
  #                     width = 6, 
  #                     textInput(inputId = ns(paste0("NombreBloque-", item)), placeholder = "...", label = paste0("Título de bloque no. ", item))
  #         )
  #       )
  #     }
  #   }
  # })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
