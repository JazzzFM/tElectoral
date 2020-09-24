#' pregunta_guardada UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyWidgets magrittr
mod_pregunta_guardada_ui <- function(id,pregunta = "",bloque = ""){
  ns <- NS(id)
  
  tagList(
    div(class="ButtonWDeleteAddon",
        shinyjs::hidden(textInput(value = pregunta, inputId = ns("Pregunta"))),
        actionButton(inputId = ns("delete"), icon = icon("trash"), "", class ="btn-danger"),
        actionButton(inputId = ns("VerPregunta"), pregunta)
        )
  )
}
#' pregunta_guardada Server Function
#'
#' @noRd 
mod_pregunta_guardada_server <- function(input, output, session, slotPregunta = c()){
  ns <- session$ns
  
  observeEvent(input$VerPregunta, {
    pregunta <-input$Pregunta
    bloque <- slotPregunta$bloque
    print(modulo)
    browser()
    showModal(modalDialog(
      title = paste0(pregunta, " - Bloque: ", bloque),
      pregunta,
      easyClose = T,
      footer = list(
        modalButton("Cancelar"),
        actionButton(inputId = ns("GuardarPregunta"), "Guardar pregunta")
      )
    ))
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# callModule(mod_pregunta_guardada_server, "pregunta_guardada_ui_1")
 
