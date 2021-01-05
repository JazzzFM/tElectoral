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
    h3("Creación de preguntas"),
    p("De click en Agregar pregunta para añadir una pregunta a un bloque."),
    uiOutput(ns("outPreguntas")),
    hr(),
    fluidRow(
      uiOutput(ns("outGuardar"), style = "width: 100%")
    )
  )
}
    
#' cuestionario_paso_3 Server Function
#'
#' @noRd 
mod_cuestionario_paso_3_server <- function(input, output, session, cuestionario = NULL, bd, usuario ,parent_session = NULL, showListadoForm = NULL, idFormGeneral = NULL, readOnly = NULL, idCuestionario = NULL){
  ns <- session$ns
  listaPreguntas <- reactiveValues()
  output$outPreguntas <- renderUI({
    if(!is.null(cuestionario$paso1)){
      lapply(seq_along(cuestionario$titulos), function(i) {
        #mod_cuestionario_bloques_ui(ns(glue::glue("cuestionario_bloques_ui_{i}")), bloque = cuestionario$titulos[i], i)
        tagList(
          fluidRow(class="two-columns",
                   p(class="text-center", cuestionario$titulos[i]), # Título bloque
                   HTML(input_btns(inputId = ns("editar"), label = "Ver preguntas", icon = "pencil", users = i, status = "primary"))
          )
        )
      })
    }
  })
  observe({
    if(!is.null(cuestionario$paso1$idCuestionario)){
      # Se añaden tibbles a listaPreguntas
      for(x in 1:cuestionario$paso1$cantidadBloques){
        item <- cuestionario$paso3[x,]
        listaPreguntas[[as.character(x)]] <- item
      }
    }
  })
  output$outGuardar <- renderUI({
    if(readOnly$val == FALSE){
      tagList(
        fluidRow( class ="padding15-25",
                  column(width = 6,
                         actionButton(ns("guardar"), "Guardar")
                  )
        )
      )
    }
  })
  
  actualValue <- reactiveValues(val = 0)
  observeEvent(input$editar,{
    
    showModal(modalDialog(title = glue::glue("Preguntas del bloque {cuestionario$titulos[as.numeric(input$editar)]}"),
                          mod_cuestionario_pregunta_ui(ns(glue::glue("cuestionario_pregunta_ui_{as.numeric(input$editar)}"))),
                          footer = actionButton(ns("editarModal"),"Aceptar"),
                          easyClose = T
    )
    )
    actualValue$val <- input$editar
    callModule(mod_cuestionario_pregunta_server, glue::glue("cuestionario_pregunta_ui_{as.numeric(input$editar)}"), valores = listaPreguntas[[as.character(input$editar)]], parent_session = parent_session, bloque = cuestionario$titulos[as.numeric(input$editar)])
  })
  observeEvent(input$editarModal,{
    
    removeModal()
    evt <- callModule(mod_cuestionario_pregunta_server, glue::glue("cuestionario_pregunta_ui_{as.numeric(actualValue$val)}"), valores = NULL, parent_session = parent_session, bloque = cuestionario$titulos[as.numeric(actualValue$val)])
    listaPreguntas[[as.character(actualValue$val)]] <- evt()
  })
  observeEvent(input$guardar, {
    
    cuestionario$paso3 <- seq_along(listaPreguntas) %>% map(~listaPreguntas[[as.character(.x)]]) %>% do.call(rbind,.)

    #map(seq_along(1:sum(length(listaPreguntas), -1)), function(i){listaPreguntas[[as.character(i)]]$idCuestionario <- 2}) # Para asignar idCuestionario a cada pregunta
  })
}
    
## To be copied in the UI
# mod_cuestionario_paso_3_ui("cuestionario_paso_3_ui_1")
    
## To be copied in the server
# callModule(mod_cuestionario_paso_3_server, "cuestionario_paso_3_ui_1")
 
