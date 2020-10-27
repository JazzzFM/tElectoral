#' evento UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import lubridate
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
             dateInput(inputId = ns("fechaEvento"), label = "Fecha de evento", format = "dd/mm/yyyy", language = "es", value = Sys.Date())
      ),
      column(width = 6,
             selectizeInput(inputId = ns("inicioEvento"), label = "Inicio de evento", choices =  c(seq(
                from=as.POSIXct("2012-1-1 0:00", tz="UTC"),
                to=as.POSIXct("2012-1-1 23:00", tz="UTC"),
                by="30 min"
             ) %>% format(.,"%R")))),
      column(width = 6,
             selectizeInput(inputId = ns("finEvento"), label = "Fin de evento", choices =  c(seq(
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
mod_evento_server <- function(input, output, session, parent_session, valores, paso1, editar, index, lugar){
  ns <- session$ns
 observeEvent(paso1, {
   if(!is.null(paso1)){
     updateDateInput(session = parent_session, inputId = ns("fechaEvento"), min = paso1$FechaInicio, max = paso1$FechaFinal)
   }
 })
 selected <- reactiveValues(inicio = "", fin = "")
 observeEvent(input$inicioEvento, {
    selected$inicio <- input$inicioEvento
 })
 observeEvent(input$finEvento, {
    selected$fin <- input$finEvento
 })
 observeEvent(input$fechaEvento, {
   horas <- c(seq(
     from=as.POSIXct("2012-1-1 0:00", tz="UTC"),
     to=as.POSIXct("2012-1-1 23:00", tz="UTC"),
     by="30 min"
   ) %>% format(.,"%R"))
   temp <- horas
   
   if(!is.null(paso1$FechaInicio)){
     if(input$fechaEvento == paso1$FechaInicio){
       temp <- temp[!(temp < paso1$HorarioInicio)]
     }else if(input$fechaEvento == paso1$FechaFinal){
       temp <- temp[!(temp > paso1$HorarioFinal)]
     } 
   }
   # Se asignan valores
   updateSelectizeInput(session = parent_session, inputId = ns("inicioEvento"), selected = selected$inicio, choices = c("Selecione hora"="", temp))
   updateSelectizeInput(session = parent_session, inputId = ns("finEvento"), selected = selected$fin, choices = c("Seleccione hora" ="",temp))
   if(editar){
      h1 <- format(as.POSIXct(valores$inicioEvento,format="%H:%M:%S"),"%H")
      m1 = format(as.POSIXct(valores$inicioEvento,format="%H:%M:%S"),"%M")
      
      h2 <- format(as.POSIXct(valores$finEvento,format="%H:%M:%S"),"%H")
      m2 = format(as.POSIXct(valores$finEvento,format="%H:%M:%S"),"%M")
      
      updateSelectizeInput(session = parent_session, inputId = ns("inicioEvento"), selected = paste(h1, m1, sep = ":"))
      updateSelectizeInput(session = parent_session, inputId = ns("finEvento"), selected = paste(h2, m2, sep = ":"))
      selected$inicio <- paste(h1, m1, sep = ":")
      selected$fin <- paste(h2, m2, sep = ":")
   }
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
    }
 })
 out <- reactive({
   tibble(
     nombre = input$nombre, 
     lugar = lugar,
     direccion = input$direccion,
     descripcion = input$descripcion, 
     contacto = input$contacto,
     telefono = input$telefono,
     correo = input$correo,
     fechaEvento = glue::glue("{input$fechaEvento} 00:00") %>% ymd_hm(), 
     inicioEvento = glue::glue("{input$fechaEvento} {input$inicioEvento}") %>% ymd_hm(),
     finEvento = glue::glue("{input$fechaEvento} {input$finEvento}") %>% ymd_hm()
   )
 })
 
 
 
 return (out)
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
