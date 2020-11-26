#' investigacionFormularioGeneral UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_investigacionFormularioGeneral_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Formulario general de encuestas"),
    p("Llene los siguientes campos para ..."),
    fluidRow(
      column(width = 6,
             textInput(inputId = ns("nombre"), label = "Nombre de la encuesta", placeholder = "...")
      ),
      column(width = 6,
             textInput(inputId = ns("casaEncuestadora"), label = "Casa encuestadora", placeholder = "...")
       ),
      column(width = 12,
             textInput(inputId = ns("poblacionObjetivo"), label = "Población objetivo", placeholder = "...")
      ),
      column(width = 6,
             dateInput(inputId = ns("fechaInicio"), label = "Fecha de inicio", value = Sys.Date(), min = Sys.Date())
      ),
      column(width = 6,
             dateInput(inputId = ns("fechaFin"), label = "Fecha de finalización", value = Sys.Date(), min = Sys.Date())
      ),
      actionButton(inputId = ns("guardar"), "Guardar", class = "btn btn-definitive")
    )
  )
}
    
#' investigacionFormularioGeneral Server Function
#'
#' @noRd 
mod_investigacionFormularioGeneral_server <- function(input, output, session, bd, usuario, parent_session, showForm = NULL){
  ns <- session$ns
  observeEvent(input$guardar, {
    if(validarFormularioGeneral(input$nombre, input$casaEncuestadora, input$poblacionObjetivo, input$fechaInicio, input$fechaFin)){
      fA <- lubridate::now(tz = "America/Mexico_City") %>% as.character()
      formGeneral <- tibble::tibble(
        nombre = input$nombre,
        casaEncuestadora = input$casaEncuestadora,
        poblacionObjetivo = input$poblacionObjetivo,
        fechaInicio = input$fechaInicio,
        fechaFin = input$fechaFin,
        fechaAlta = fA,
        fechaEdicion = NULL,
        usuarioCrea = usuario$user,
        usuarioEdicion = NULL,
        activo = 1
      )
      insertBd(pool, formGeneralBd, bd = formGeneral)
      # expr = {
      #   leerBd(pool, formGeneralBd)
      #   shinyalert::shinyalert(title = "Los datos se subieron correctamente.")
      #   }
      # error = function(e){    
      #   shinyalert::shinyalert(title = "Los datos no se subieron, intente más tarde o revise su conexión..")
      # }
      # warning = function(w){    
      #   shinyalert::shinyalert(title = "Revise su conexión.")
      # }
    }
  })
}
    
## To be copied in the UI
# mod_investigacionFormularioGeneral_ui("investigacionFormularioGeneral_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionFormularioGeneral_server, "investigacionFormularioGeneral_ui_1")
 
