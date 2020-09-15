#' registro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyTime
mod_registro_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(8,
             pickerInput(inputId = ns("gira"), label = "Seleccione una gira", choices = "",
                         options = pickerOptions(noneSelectedText = "Seleccione una opción")
             )
      ),
      column(4,
             actionBttn(inputId = ns("addGira"), label = "",style = "material-circle", 
                        color = "success",
                        icon = icon("plus"))
      )
    ),
    fluidRow(
      column(12,
             dateInput(inputId = ns("fecha"),label = "Fecha",value = lubridate::today(),format = "d M yy",language = "es"),
             timeInput(inputId = ns("inicio"),label = "Hora inicial",seconds = F,minute.steps = 15),
             timeInput(inputId = ns("fin"),label = "Hora final", seconds = F,minute.steps = 15)
      )
    ),
    fluidRow(
      column(8,
             pickerInput(inputId = ns("responsable"), label = "Seleccione un responsable", choices = "",
                         options = pickerOptions(noneSelectedText = "Seleccione una opción")
             )
      ),
      column(4,
             actionBttn(inputId = ns("addResponsable"), label = "",style = "material-circle", 
                        color = "success",
                        icon = icon("plus")
             )
      )
    ),
    fluidRow(
      pickerInput(inputId = ns("lugar"), label = "Lugar",choices = "",
                  options = pickerOptions(noneSelectedText = "Seleccione una opción")
      )
    ),
    fluidRow(
      textAreaInput(inputId = ns("descripcion"),label = "Descripción",rows = 8,resize = "none") 
    ),
    fluidRow(
      actionBttn(inputId = ns("guardarEvento"), label = "Guardar",block = T)
    )
  )
}

#' registro Server Function
#'
#' @noRd 
mod_registro_server <- function(input, output, session){
  ns <- session$ns
  
  observeEvent(input$addGira,{
    mod_registroGira_ui(ns("registroGira_ui_1"))
  })
  
  callModule(mod_registroGira_server, "registroGira_ui_1")
  
  observeEvent(input$guardarEvento,{
    check <- c("gira", "responsable", "lugar", "descripcion") %>% mandatory(input = input, .)
    if(check){
      updatePickerInput(session = ssession,inputId = "gira",selected = "")
      updatePickerInput(session = ssession,inputId = "responsable",selected = "")
      updatePickerInput(session = ssession,inputId = "lugar",selected = "")
      updateTextAreaInput(session = session, inputId = "descripcion", value = "")
      updateDateInput(session = session, inputId = "fecha", value = lubridate::today(),language = "es")
      updateTimeInput(session = session, inputId = "inicio",value = 0)
      updateTimeInput(session = session, inputId = "fin",value = 0)
    } else{
      shinyalert::shinyalert(title = "Registro incompleto", text = "Favor de llenar por completo el formulario.")
    }
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 

