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
  # observe({
  #   listaPreguntas$preguntas <- seq_along(cuestionario$titulos) %>% map(~callModule(mod_cuestionario_bloques_server,
  #                                                                                   glue::glue("cuestionario_bloques_ui_{.x}"),
  #                                                                                   bloque = cuestionario$titulos[.x],
  #                                                                                   parent_session = parent_session,
  #                                                                                   cuestionario,
  #                                                                                   .x))
  # })
  actualValue <- reactiveValues(val = 0)
  observeEvent(input$editar,{
    
    showModal(modalDialog(title = glue::glue("Preguntas del bloque {cuestionario$titulos[as.numeric(input$editar)]}"),
                          mod_cuestionario_pregunta_ui(ns(glue::glue("cuestionario_pregunta_ui_{as.numeric(input$editar)}"))),
                          footer = actionButton(ns("editarModal"),"Aceptar"),
                          easyClose = T
    )
    )
    actualValue$val <- input$editar
    callModule(mod_cuestionario_pregunta_server, glue::glue("cuestionario_pregunta_ui_{as.numeric(input$editar)}"), valores = cuestionario$paso3[as.numeric(input$editar)], parent_session = parent_session, bloque = cuestionario$titulos[as.numeric(input$editar)])
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
 
