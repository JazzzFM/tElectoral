#' giraPaso1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_giraPaso1_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Instancia de gira"),
    p("A continuación, llene los campos para completar el paso 1"),
    fluidRow(
      column(width = 12,
             textInput(inputId = ns("Responsable"), label = "Responsable" , placeholder = "..."))
    ),
    fluidRow(
      column(width = 12,
             textAreaInput(inputId = ns("Descripcion"), label = "Descripcion" , placeholder = "...", rows = 5))
    ),
    fluidRow(
      column(width = 8,
             selectizeInput(inputId = ns("LugarInicio"), label = "Lugar de inicio", choices = c("Seleccione un lugar" = "", "Lugar 1", "Lugar 2") )
      ),
      column(width = 4,
             timeInput(inputId = ns("HorarioInicio"), label = "Hora de inicio", value =  Sys.time(), seconds = F)
      )
    ),
    fluidRow(
      column(width = 8,
             selectizeInput(inputId = ns("LugarFinal"), label = "Lugar de destino", choices = c("Seleccione un lugar" = "", "Lugar 1", "Lugar 2") )
      ),
      column(width = 4,
             timeInput(inputId = ns("HorarioFinal"), label = "Hora de finalización", value =  Sys.time(), seconds = F)
      )
    ),
    actionButton(inputId = ns("guardar"), "Guardar")
  )
}

#' giraPaso1 Server Function
#'
#' @noRd 
mod_giraPaso1_server <- function(input, output, session){
  ns <- session$ns
  
  gira <- reactiveVal()
  
  observeEvent(input$guardar,{
    browser()
    check <- c("Responsable","Descripcion","LugarInicio","HorarioInicio","LugarFinal","HorarioFinal") %>%
      mandatory(input = input, .)
    if(check){
      gira <- tibble::tibble(
        Responsable = input$Responsable, 
        Descripcion = input$Descripcion, LugarInicio = input$LugarInicio, 
        HorarioInicio = input$HorarioInicio, LugarFinal = input$LugarFinal, 
        HorarioFinal = input$HorarioFinal
      ) 
    } else{
      shinyalert::shinyalert(title = "Formato incompleto")
    }
  })
  
  return(gira)
}

## To be copied in the UI
# 

## To be copied in the server
# 

