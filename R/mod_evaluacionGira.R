#' evaluacionGira UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_evaluacionGira_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Evaluación de gira"),
    p("Seleccione una gira y llene los siguientes campos para completar la evaluación"),
    hr(),
    selectizeInput(inputId = ns("gira"), choices = c("Seleccione ..." = "", "Gira 1", "Gira 2"), label = "Gira a evaluar"),
    h4("Eventos"),
    DT::DTOutput(ns("eventos"))
  )
}
    
#' evaluacionGira Server Function
#'
#' @noRd 
mod_evaluacionGira_server <- function(input, output, session, parent_session = NULL){
  ns <- session$ns
  a <- tibble(abc = c(a = "Evento 1", b = "Evento 2", c= "Evento 3"), Acciones = c(a = 1, b = 2, c= 3))
  output$eventos <- DT::renderDT({
    a$Acciones$a <- HTML(input_btns(ns("evaluar"), 1, "Evaluar", icon = "edit", status = "primary"), input_btns(ns("ver"), 1, "Ver", icon = "eye", status = "info"))
    a$Acciones$b <- HTML(input_btns(ns("evaluar"), 2, "Evaluar", icon = "edit", status = "primary"), input_btns(ns("ver"), 2, "Ver", icon = "eye", status = "info"))
    a$Acciones$c <- HTML(input_btns(ns("evaluar"), 3, "Evaluar", icon = "edit", status = "primary"), input_btns(ns("ver"), 3, "Ver", icon = "eye", status = "info"))
    DT::datatable(
      data = a
    )
  }, escape = F)
  
  evaluaciones <- reactiveValues()
  actualEvaluable <- reactiveValues(value = 0)
  observeEvent(input$evaluar, {
    showModal(modalDialog(title = "Evaluación de evento", #Se planea concatenar el título del evento
                          mod_evaluacionGiraPreguntas_ui(glue::glue(ns("evaluacionGiraPreguntas_ui_{input$evaluar}"))),
                          footer = actionButton("Concluir evaluación", class = "btn-primary", inputId = ns("concluirEvaluacion")),
                          easyClose = T
                          ))
    callModule(mod_evaluacionGiraPreguntas_server, glue::glue("evaluacionGiraPreguntas_ui_{input$evaluar}"), valores = NULL, parent_session = parent_session)
    actualEvaluable$value <- input$evaluar
  })
  
  observeEvent(input$concluirEvaluacion, {
    removeModal()
    evaluaciones[[as.character(actualEvaluable$value)]] <- callModule(mod_evaluacionGiraPreguntas_server, glue::glue("evaluacionGiraPreguntas_ui_{actualEvaluable$value}"), valores = NULL, parent_session = parent_session)
    
  })
  
  observeEvent(input$ver, {
    showModal(modalDialog(title = "Evaluación de evento", #Se planea concatenar el título del evento
                          mod_evaluacionGiraPreguntas_ui(glue::glue(ns("evaluacionGiraPreguntas_ui_{input$ver}"))),
                          footer =  modalButton("Cerrar"),
                          easyClose = T
    ))
    callModule(mod_evaluacionGiraPreguntas_server, glue::glue("evaluacionGiraPreguntas_ui_{input$ver}"), valores = evaluaciones[[as.character(input$ver)]](), parent_session = parent_session, readonly = 1)
  })
}
    
## To be copied in the UI
# mod_evaluacionGira_ui("evaluacionGira_ui_1")
    
## To be copied in the server
# callModule(mod_evaluacionGira_server, "evaluacionGira_ui_1")
 
