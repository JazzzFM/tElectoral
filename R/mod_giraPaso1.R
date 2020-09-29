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
      column(width = 7,
             selectizeInput(inputId = ns("LugarInicio"), label = "Lugar de inicio", choices = c("Seleccione un lugar" = "", "Lugar 1", "Lugar 2") )
      ),
      column(width = 5,
             selectizeInput(inputId = ns("HorarioInicio"), label = "Hora de inicio", choices =  c("Seleccione hora" = "", seq(
               from=as.POSIXct("2012-1-1 0:00", tz="UTC"),
               to=as.POSIXct("2012-1-1 23:00", tz="UTC"),
               by="hour"
             ) %>% format(.,"%R"))
            )
      )
    ),
    fluidRow(
      column(width = 7,
             selectizeInput(inputId = ns("LugarFinal"), label = "Lugar de destino", choices = c("Seleccione un lugar" = "", "Lugar 1", "Lugar 2") )
      ),
      column(width = 5,
             selectizeInput(inputId = ns("HorarioFinal"), label = "Hora de finalización", choices = c("Seleccione hora" = "", seq(
               from=as.POSIXct("2012-1-1 0:00", tz="UTC"),
               to=as.POSIXct("2012-1-1 23:00", tz="UTC"),
               by="hour"
             ) %>% format(.,"%R"))
             )
      )
    ),
    actionButton(inputId = ns("guardar"), "Guardar", class = "btn-primary")
  )
}

#' giraPaso1 Server Function
#'
#' @noRd 
mod_giraPaso1_server <- function(input, output, session){
  ns <- session$ns
  
  gira <- reactiveVal()
  
  observeEvent(input$guardar,{
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

