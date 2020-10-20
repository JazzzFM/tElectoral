#' representantesGenerales UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_representantesGenerales_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Nuevo representante general"),
    p("Llena los siguientes campos para crear un nuevo representante general"),
    fluidRow(
      column(width = 6, textInput(inputId = ns("nombres"), label = "Nombre (s)", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("apPaterno"), label = "Apellido paterno", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("apMaterno"), label = "Apellido materno", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("telefonoRgFijo"), label = "Teléfono fijo", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("telefonoRgCelular"), label = "Teléfono celular", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("correo"), label = "Correo electrónico", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("claveElector"), label = "Clave de elector", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("direccion"), label = "Direccion", placeholder = "...")),
      column(width = 6, shinyjs::disabled(textInput(inputId = ns("nombramiento"), label = "Nombramiento", value = "Representante General")))
    ),
    h3("Información de casilla"),
    hr(),
    fluidRow(
      column(width = 6, selectizeInput(inputId = ns("estado"), label = "Estado", choices = c("Seleccione" = '',"Chiapas","Quintana Roo","Querétaro"))),
      column(width = 6, selectizeInput(inputId = ns("municipio"), label = "Municipio", choices = c("Seleccione" = '',"Chiapas","Quintana Roo","Querétaro"))),
      column(width = 6, selectizeInput(inputId = ns("seccion"), label = "Sección", choices = c("Seleccione" = '',"Chiapas","Quintana Roo","Querétaro"))),
      column(width = 6, selectizeInput(inputId = ns("tipoCasilla"), label = "Tipo de casilla", c("Seleccione" = '',"Chiapas","Quintana Roo","Querétaro"))),
      column(width = 6, textInput(inputId = ns("calle"), label = "Calle", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("noExterior"), label = "No. Exterior", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("noInterior"), label = "No. Interior", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("colonia"), label = "Colonia", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("cp"), label = "Código postal", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("ubicacion"), label = "Ubicación", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("referencias"), label = "Referencias", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("tipoDomicilio"), label = "Tipo de domicilio", placeholder = "..."))
    ),
    actionButton(inputId = ns("guardarRepresentante"), label = "Guardar representante", class="btn-primary")
  )
}
    
#' representantesGenerales Server Function
#'
#' @noRd 
mod_representantesGenerales_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_representantesGenerales_ui("representantesGenerales_ui_1")
    
## To be copied in the server
# callModule(mod_representantesGenerales_server, "representantesGenerales_ui_1")
 
