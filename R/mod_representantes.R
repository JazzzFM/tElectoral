#' representantes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_representantes_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Nuevo representante"),
    p("Llena los siguientes campos para crear un nuevo representante de casilla"),
    
    fluidRow(
      column(width = 12, selectizeInput(inputId = ns("representanteGeneral"), label = "Representante general", choices = c("Seleccione" = '',"RG 1","RG 2","RG 3"))),
      # column(width = 6, textInput(inputId = ns("rg"), label = "RG", placeholder = "...")),
      # column(width = 6, textInput(inputId = ns("telefonoRg"), label = "Teléfono RG", placeholder = "...")),
      # column(width = 6, selectizeInput(inputId = ns("estado"), label = "Estado", choices = c("Seleccione" = '',"Chiapas","Quintana Roo","Querétaro"))),
      # column(width = 6, selectizeInput(inputId = ns("municipio"), label = "Municipio", choices = c("Seleccione" = '',"Chiapas","Quintana Roo","Querétaro"))),
      # column(width = 6, selectizeInput(inputId = ns("seccion"), label = "Sección", choices = c("Seleccione" = '',"Chiapas","Quintana Roo","Querétaro"))),
      # column(width = 6, textInput(inputId = ns("casilla"), label = "Casilla", placeholder = "...")),
      # column(width = 6, selectizeInput(inputId = ns("nombramiento"), label = "Nombramiento", choices = c("Seleccione" = '',"Propietario 1","Propietario 2","Suplente 1", "Suplente 2")))
    ),
    h3("Información personal"),
    hr(),
    fluidRow(
      column(width = 6, textInput(inputId = ns("nombres"), label = "Nombre (s)", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("apPaterno"), label = "Apellido paterno", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("apMaterno"), label = "Apellido materno", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("telefono"), label = "Teléfono", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("correo"), label = "Correo electrónico", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("claveElector"), label = "Clave de elector", placeholder = "..."))
    ),
    h3("Información domiciliaria"),
    hr(),
    fluidRow(
      column(width = 6, textInput(inputId = ns("direccion"), label = "Direccion", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("calle"), label = "Calle", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("colonia"), label = "Colonia", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("noExterior"), label = "No. Exterior", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("noInterior"), label = "No. Interior", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("cp"), label = "Código postal", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("referencias"), label = "Referencias", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("tipoDomicilio"), label = "Tipo de domicilio", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("fuente"), label = "Fuente", placeholder = "...")),
      column(width = 6, textInput(inputId = ns("ubicacion"), label = "Ubicación", placeholder = "...")),
    ),
    actionButton(inputId = ns("guardar"), label = "Guardar representante", class="btn-primary")
  )
}
    
#' representantes Server Function
#'
#' @noRd 
mod_representantes_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_representantes_ui("representantes_ui_1")
    
## To be copied in the server
# callModule(mod_representantes_server, "representantes_ui_1")
 
