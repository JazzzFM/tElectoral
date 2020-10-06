#' evento UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_evento_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             textInput(label = "Nombre del evento", placeholder = "...", inputId = ns("nombre"))),
      column(width = 12,
             textAreaInput(label = "Descripción del evento", inputId = ns("descripcion"))),
      column(width = 6,
             textInput(label = "Contacto", placeholder = "Correo, teléfono, etc.", inputId = ns("contacto"))),
      column(width = 6,
             selectizeInput(inputId = ns("duracion"), label = "Duración", choices =  c("Seleccione hora" = "", seq(
               from=as.POSIXct("2012-1-1 0:00", tz="UTC"),
               to=as.POSIXct("2012-1-1 23:00", tz="UTC"),
               by="30 min"
             ) %>% format(.,"%R"))
             ))
    )
  )
}
    
#' evento Server Function
#'
#' @noRd 
mod_evento_server <- function(input, output, session, valores = NULL, parent_session = NULL){
  ns <- session$ns
 out <- reactive({
   tibble(nombre = input$nombre, descripcion = input$descripcion, contacto = input$contacto, duracion = input$duracion)
 })
 
 observeEvent(valores,{
   if(!is.null(valores)){
     updateTextInput(session = parent_session, inputId = ns("nombre"), value = valores$nombre)
     updateTextAreaInput(session = parent_session, inputId = ns("descripcion"), value = valores$descripcion)
     updateTextInput(session = parent_session, inputId = ns("contacto"), value = valores$contacto)
     updateSelectizeInput(session = parent_session, inputId = ns("duracion"), selected = valores$duracion)
   }
 })
 
 return (out)
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
