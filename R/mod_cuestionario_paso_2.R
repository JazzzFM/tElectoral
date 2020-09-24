#' cuestionario_paso_2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyWidgets magrittr
mod_cuestionario_paso_2_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      class = "FlexRow",
      column(width = 5,
             textInput(inputId = ns("PreguntaAnalizar"), placeholder = "Ingrese pregunta a analizar", label = "Pregunta")
      ),
      column(width = 4,
             pickerInput(label = "Bloque al que pertenece", choices = c(), inputId = ns("BloquePerteneciente")
             )
      ),
      column(width = 3,
             actionButton(inputId = ns("GenPregunta"), "Generar pregunta", class = "btn-primary"),
      )
    ),
    fluidRow(
      column(width = 12, class="TabBloques", uiOutput(ns("TabBloques")))
    )
  )
}

#' cuestionario_paso_2 Server Function
#'
#' @noRd 
mod_cuestionario_paso_2_server <- function(input, output, session, tituloBloques = c()){
  ns <- session$ns
  slotPregunta <- reactiveValues(pregunta = "", bloque = "", preguntas = c())
  var <- callModule(mod_cuestionario_pregunta_server, "cuestionario_pregunta_ui_1")
  
  observeEvent(input$GenPregunta, {
    slotPregunta$pregunta <- pregunta <- input$PreguntaAnalizar
    slotPregunta$bloque <- bloque <- input$BloquePerteneciente
    showModal(modalDialog(
      title = paste0(pregunta, " - Bloque: ", bloque),
      mod_cuestionario_pregunta_ui(ns("cuestionario_pregunta_ui_1")),
      easyClose = T,
      footer = list(
        modalButton("Cancelar"),
        actionButton(inputId = ns("GuardarPregunta"), "Guardar pregunta")
      )
    ))
  })
  observeEvent(input$GuardarPregunta,{
    #slotPregunta$preguntas[1] <- slotPregunta$modulo
    browser()
    var()
    insertUI(selector = paste0(".tab-pane[data-value='",slotPregunta$bloque,"']"), where = "beforeEnd", session = session,
             ui = mod_pregunta_guardada_ui(ns(paste0("pregunta_guardada_ui_1-",slotPregunta$pregunta)), slotPregunta$pregunta, slotPregunta$bloque))
  })
  observe({
    callModule(mod_pregunta_guardada_server, "pregunta_guardada_ui_1", slotPregunta)
  })
  observe({
    isolate(
      if(length(tituloBloques$titulos) > 1){
        output$TabBloques <- renderUI({
          do.call(tabsetPanel, c(id=ns("Tabs"),lapply(2:length(tituloBloques$titulos), function(i) {
            titulo <- tituloBloques$titulos[i]
            tabPanel(title = titulo, value = titulo)
          })))
        })
      } 
    )
    updatePickerInput(session = session, inputId = "BloquePerteneciente", choices = tituloBloques$titulos)
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 