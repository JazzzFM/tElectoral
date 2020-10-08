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
      column(width = 6,
             textInput(label = "Nombre del evento", placeholder = "...", inputId = ns("nombre"))),
      column(width = 6,
             textInput(label = "Dirección donde será el evento", placeholder = "...", inputId = ns("direccion"))),
      column(width = 12,
             textAreaInput(label = "Descripción del evento", inputId = ns("descripcion"))),
      column(width = 6,
             textInput(label = "Nombre de contacto", placeholder = "...", inputId = ns("contacto"))),
      column(width = 6,
             textInput(label = "Teléfono", placeholder = "...", inputId = ns("telefono"))),
      column(width = 6,
             textInput(label = "Correo electrónico", placeholder = "...", inputId = ns("correo"))),
      column(width = 6,
             dateInput(inputId = ns("fechaEvento"), label = "Fecha de evento", format = "dd/mm/yyyy", language = "es", value = Sys.Date(), min = Sys.Date() )
      ),
      column(width = 6,
             selectizeInput(inputId = ns("inicioEvento"), label = "Inicio de evento", choices =  c("Seleccione hora" = "", seq(
               from=as.POSIXct("2012-1-1 0:00", tz="UTC"),
               to=as.POSIXct("2012-1-1 23:00", tz="UTC"),
               by="30 min"
             ) %>% format(.,"%R"))
             )),
      column(width = 6,
             selectizeInput(inputId = ns("finEvento"), label = "Fin de evento", choices =  c("Seleccione hora" = "", seq(
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
   tibble(
     nombre = input$nombre, 
     direccion = input$direccion,
     descripcion = input$descripcion, 
     contacto = input$contacto,
     telefono = input$telefono,
     correo = input$correo,
     fechaEvento = input$fechaEvento, 
     inicioEvento = input$inicioEvento,
     finEvento = input$finEvento)
 })
 
 observeEvent(valores,{
   if(!is.null(valores)){
     updateTextInput(session = parent_session, inputId = ns("nombre"), value = valores$nombre)
     updateTextInput(session = parent_session, inputId = ns("direccion"), value = valores$direccion)
     updateTextAreaInput(session = parent_session, inputId = ns("descripcion"), value = valores$descripcion)
     updateTextInput(session = parent_session, inputId = ns("contacto"), value = valores$contacto)
     updateTextInput(session = parent_session, inputId = ns("telefono"), value = valores$telefono)
     updateTextInput(session = parent_session, inputId = ns("correo"), value = valores$correo)
     updateDateInput(session = parent_session, inputId = ns("fechaEvento"), value = valores$fechaEvento)
     updateSelectizeInput(session = parent_session, inputId = ns("inicioEvento"), selected = valores$inicioEvento)
     updateSelectizeInput(session = parent_session, inputId = ns("finEvento"), selected = valores$finEvento)
   }
 })
 
 return (out)
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
