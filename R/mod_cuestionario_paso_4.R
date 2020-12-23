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
                 textInput(inputId = ns("correo"), label = "Correo al que se enviará análisis", placeholder="ejemplo@dominio.com")),
          column(width = 12,
                 textAreaInput(inputId = ns("obsGenerales"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
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
mod_cuestionario_paso_4_server <- function(input, output, session, cuestionario = c(), bd, usuario , showListadoForm = NULL, idFormGeneral = NULL, readOnly = NULL, idCuestionario = NULL){
  ns <- session$ns
  observeEvent(cuestionario$paso1,{
    if(!is.null(cuestionario$paso1$idCuestionario)){
      updateTextInput(inputId = "correo", session = session, value = cuestionario$paso1$correo)
      updateTextAreaInput(inputId = "obsGenerales", session = session, value = cuestionario$paso1$obsGenerales)
      shinyjs::hide(selector = paste0("#", ns("guardarCuestionario")))
    }
  })
  
  observeEvent(input$guardarCuestionario, {
    cuestionario$paso1$correo <- input$correo
    cuestionario$paso1$obsGenerales <- input$obsGenerales
    
    # Se guarda info principal
    insertBd(pool, formCuestionarioBd, bd = cuestionario$paso1, F)
    idCuestionario <- tbl(pool, formCuestionarioBd) %>% filter(fechaAlta == !!cuestionario$paso1$fechaAlta) %>% pull(idCuestionario)
    
    # Se reasigna id guardada a las id's de las preguntas
    for(x in 1:length(cuestionario$paso3$idCuestionario)){
         cuestionario$paso3[x,]$idCuestionario <- idCuestionario
    }
    
    insertBd(pool, formCuestionarioPreguntasXBloqueBd, bd = cuestionario$paso3)
    gargoyle::trigger("cuestionario")
  })
}
    
## To be copied in the UI
# mod_cuestionario_paso_4_ui("cuestionario_paso_4_ui_1")
    
## To be copied in the server
# callModule(mod_cuestionario_paso_4_server, "cuestionario_paso_4_ui_1")
 
