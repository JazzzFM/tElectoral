#' investigacionFormularioDisMuestral UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_investigacionFormularioDisMuestral_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Formulario de Diseño Muestral"),
    tags$hr(),
    p("Llene los siguientes campos ..."),
    fluidRow(
      column(width = 6,
             pickerInput(label = "Modo de levantamiento",
                         choices = c("Seleccione" = '',"Vivienda","Telefónica","Internet"),
                         inputId = ns("modoLevantamiento"), selected = 0)
             ),
      column(width = 6,
              textInput(inputId = ns("marcoMuestral"), label = "Marco Muestral",
                        placeholder = "Respuesta libre ...")
            ),
      column(width = 6, 
             numericInput(inputId = ns("numeroEntrevistas"),
                              label = "Sólo ingrese números enteros",
                              value = 0,
                              min = 0)
            ),
      column(width = 6,
        prettyRadioButtons(label = "Aleatoria", choices = c("Sí", "No"),
                           inputId = ns("aleatoria"), selected = 0)
      ),
      column(width = 4,
             prettyRadioButtons(label = "Poloetápica", choices = c("Sí", "No"),
                                inputId = ns("poliEtapa"), selected = 0)
      ),
      column(width = 4,
             prettyRadioButtons(label = "Estratificada", choices = c("Sí", "No"),
                                inputId = ns("estrat"), selected = 0)
      ),
      column(width = 4,
             prettyRadioButtons(label = "Conglomerados", choices = c("Sí", "No"),
                                inputId = ns("conglo"), selected = 0)
      ),
      column(width = 4, 
             numericInput(inputId = ns("nivelPolietap"),
                              label = "¿Cuántos niveles?",
                              value = 0,
                              min = 0)
      ), 
      column(width = 4, 
             numericInput(inputId = ns("nivelEstrat"),
                              label = "¿Cuántos niveles?",
                              value = 0,
                              min = 0)
      ),
      column(width = 4, 
                numericInput(inputId = ns("nivelConglo"),
                                 label = "¿Cuántos niveles?",
                                 value = 0,
                                 min = 0)
      ),
      column(width = 12,
             textInput(inputId = ns("unidadMuestral"), label = "Unidad muestral",
                       placeholder = "Respuesta libre ...")
      ),
      column(width = 12,
             textInput(inputId = ns("nivelConfianza"), label = "Nivel de confianza",
                       placeholder = "Respuesta libre ...")
      ),
      column(width = 12,
             textInput(inputId = ns("margenError"), label = "Margen de error",
                       placeholder = "Respuesta libre ...")
      ),
      actionButton(inputId = ns("guardar"), "Guardar", class = "btn btn-definitive")
    )
  )
}
    
#' investigacionFormularioDisMuestral Server Function
#'
#' @noRd 
mod_investigacionFormularioDisMuestral_server <- function(input, output, session){
  ns <- session$ns
  
  observeEvent(input$guardar, {
  if(validarFormularioDisMuestral(input$modoLevantamiento, input$marcoMuestral, input$numeroEntrevistas,
                               input$aleatoria, input$poliEtapa, input$estrat, input$conglo,
                               input$nivelPolietap, input$nivelEstrat, input$nivelConglo,
                               input$unidadMuestral, input$nivelConfianza, input$margenError)){
    return (
      tibble::tibble(
        modoLevanamiento = input$modoLevantamiento,
        marcoMuestral = input$marcoMuestral,
        numeroEntrevistas = input$numeroEntrevistas,
        aleatoria = input$aleatoria, 
        poliEtap = input$poliEtapa,
        nivelPolietap = input$nivelPolietap, 
        estrat = input$estrat,
        nivelEstrat = input$nivelEstrat, 
        conglo = input$conglo,
        nivelConglo = input$nivelConglo,
        unidadMuestral = input$unidadMuestral,
        nivelConfianza = input$nivelConfianza,
        margenError = input$margenError
        )
      )
    }
  })
     
}
    
## To be copied in the UI
# mod_investigacionFormularioDisMuestral_ui("investigacionFormularioDisMuestral_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionFormularioDisMuestral_server, "investigacionFormularioDisMuestral_ui_1")
