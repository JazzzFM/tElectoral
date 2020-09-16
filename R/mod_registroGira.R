#' registroGira UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_registroGira_ui <- function(id){
  ns <- NS(id)
  showModal(
    modalDialog(
      title = "Ingrese la información de una nueva gira",
      tagList(
        alignCenter(
          pickerInput(inputId = ns("responsable"),label = "Responsable de la gira",choices = "",
                      options = pickerOptions(noneSelectedText = "Seleccione una opción")
          ) 
        ),
        alignCenter(
          textAreaInput(inputId = ns("descripcion"), label = "Descripción de la gira") 
        ),
        h1("Inicio"),
        dateInput(inputId = ns("fechaIni"),label = "Fecha de inicio",value = lubridate::today(),format = "d M yy",language = "es"),
        timeInput(inputId = ns("inicio"),label = "Hora inicial",seconds = F,minute.steps = 15),
        h2("Fin"),
        dateInput(inputId = ns("fechaIni"),label = "Fecha final",value = lubridate::today(),format = "d M yy",language = "es"),
        timeInput(inputId = ns("fin"),label = "Hora final", seconds = F,minute.steps = 15)
      ),
      easyClose = T,
      fade = T, size = "l",
      footer = alignCenter(actionButton(ns("guardar"), "Guardar"))
    )
  )
}
    
#' registroGira Server Function
#'
#' @noRd 
mod_registroGira_server <- function(input, output, session){
  ns <- session$ns
 
  observeEvent(input$guardar,{
    check <- c("responsable","descripcion") %>% mandatory(input = input, .)
    if(check){
      
      removeModal()  
    } else{
      shinyalert::shinyalert(title = "Registro incompleto", text = "Favor de llenar por completo el formulario.")
    }
    
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
