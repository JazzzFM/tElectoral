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
             textInput(label = "Nombre del evento", placeholder = "...", inputId = ns("Nombre"))),
      column(width = 12,
             textAreaInput(label = "Descripción del evento", inputId = "Descripcion")),
      column(width = 6,
             textInput(label = "Contacto", placeholder = "Correo, teléfono, etc.", inputId = "Contacto")),
      column(width = 6,
             selectizeInput(inputId = ns("Duracion"), label = "Duración", choices =  c("Seleccione hora" = "", seq(
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
mod_evento_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_evento_ui("evento_ui_1")
    
## To be copied in the server
# callModule(mod_evento_server, "evento_ui_1")
 
