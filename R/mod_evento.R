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
             selectizeInput(inputId = ns("inicioEvento"), label = "Inicio de evento", choices =  c())),
      column(width = 6,
             selectizeInput(inputId = ns("finEvento"), label = "Fin de evento", choices =  c()
             )),
      column(width = 12, shinyjs::hidden(checkboxInput(inputId = ns("usarNuevoHorario"), label = "¿Usar horarios no ocupados?"))),
      column(width = 6,
             shinyjs::hidden(selectizeInput(inputId = ns("inicioEventoNuevo"), label = "Inicio de evento", choices =  c()
             ))),
      column(width = 6,
             shinyjs::hidden(selectizeInput(inputId = ns("finEventoNuevo"), label = "Fin de evento", choices =  c()
             )))
    )
  )
}
    
#' evento Server Function
#'
#' @noRd 
mod_evento_server <- function(input, output, session, valores = NULL, parent_session = NULL, horariosOcupados = NULL, horariosPaso1 = NULL, editar = F, index = 0){
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
     inicioEvento = ifelse(input$usarNuevoHorario == T, input$inicioEventoNuevo, input$inicioEvento),
     finEvento = ifelse(input$usarNuevoHorario == T, input$finEventoNuevo, input$finEvento) 
     )
 })
 observeEvent(horariosPaso1, {
   if(!is.null(horariosPaso1)){
     updateDateInput(session = parent_session, inputId = ns("fechaEvento"), min = horariosPaso1$FechaInicio, max = horariosPaso1$FechaFinal)
   }
 })
 observeEvent(editar, {
    if(editar){
       shinyjs::show(selector = paste0("#", ns("usarNuevoHorario")))
       shinyjs::show(selector = paste0("#", ns("inicioEventoNuevo")))
       shinyjs::show(selector = paste0("#", ns("finEventoNuevo")))
    }
 })
 observeEvent(input$usarNuevoHorario, {
   if(input$usarNuevoHorario == T){
     shinyjs::disable(selector = paste0("#", ns("inicioEvento")))
     shinyjs::disable(selector = paste0("#", ns("finEvento")))
     
     shinyjs::enable(selector = paste0("#", ns("inicioEventoNuevo")))
     shinyjs::enable(selector = paste0("#", ns("finEventoNuevo")))
   }else{
     shinyjs::enable(selector = paste0("#", ns("inicioEvento")))
     shinyjs::enable(selector = paste0("#", ns("finEvento")))
     
     shinyjs::disable(selector = paste0("#", ns("inicioEventoNuevo")))
     shinyjs::disable(selector = paste0("#", ns("finEventoNuevo")))
   }
 })

 observeEvent(input$fechaEvento, {
   horas <- c(seq(
     from=as.POSIXct("2012-1-1 0:00", tz="UTC"),
     to=as.POSIXct("2012-1-1 23:00", tz="UTC"),
     by="30 min"
   ) %>% format(.,"%R"))
   temp <- horas
   
   if(!is.null(horariosPaso1$FechaInicio)){
     if(input$fechaEvento == horariosPaso1$FechaInicio){
       temp <- temp[!(temp < horariosPaso1$HorarioInicio)]
     }else if(input$fechaEvento == horariosPaso1$FechaFinal){
       temp <- temp[!(temp > horariosPaso1$HorarioFinal)]
     } 
   }
   
   if(!editar){
     # Se eliminan horarios ocupados
     if(!is.null(horariosOcupados) & length(horariosOcupados$lugar) > 0){
       for(i in 1:length(horariosOcupados$lugar)){
         if(horariosOcupados$fecha[i] == input$fechaEvento)
           temp <- temp[!(temp <= horariosOcupados$horaFinal[i])]
       }
     }
     # Se asignan valores
     updateSelectizeInput(session = parent_session, inputId = ns("inicioEvento"), choices = c("Selecione hora"="", temp))
     updateSelectizeInput(session = parent_session, inputId = ns("finEvento"), choices = c("Seleccione hora" ="",temp))
   }else{
     # Se eliminan horarios ocupados
      for(i in 1:length(horariosOcupados$lugar)){
         # Eliminamos horas para nuevo horario según la fecha escogida  
         if(horariosOcupados$fecha[i] == input$fechaEvento)
            temp <- temp[!(temp <= horariosOcupados$horaFinal[i])]
      }
      
      # Eliminamos horas para viejo horario para todas las fechas
      browser()
      horas <- horas[!(horas < horariosOcupados$horaInicio[as.numeric(index)] | horas > horariosOcupados$horaFinal[as.numeric(index)])]
     
     updateSelectizeInput(session = parent_session, inputId = ns("inicioEvento"), selected = horas[1], choices = c("Seleccione hora" = "", horas))
     updateSelectizeInput(session = parent_session, inputId = ns("finEvento"), selected = horas[length(horas)], choices = c("Seleccione hora" = "", horas))
     
     updateSelectizeInput(session = parent_session, inputId = ns("inicioEventoNuevo"), choices = c("Seleccione hora" = "", temp))
     updateSelectizeInput(session = parent_session, inputId = ns("finEventoNuevo"), choices = c("Seleccione hora" = "", temp))
     
     # Se deshabilita check
     if(input$fechaEvento != valores$fechaEvento){
        updateCheckboxInput(session = parent_session, inputId = ns("usarNuevoHorario"), value = T)
        shinyjs::disable(selector = paste0("#",ns("usarNuevoHorario")))
     }else{
        updateCheckboxInput(session = parent_session, inputId = ns("usarNuevoHorario"), value = F)
        shinyjs::enable(selector = paste0("#",ns("usarNuevoHorario")))
     }
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
 
 return (out)
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
