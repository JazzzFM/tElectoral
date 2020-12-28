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
    fluidRow(
      column(width = 6,
             actionButton(inputId = ns("atras"), label = "Regresar al listado de encuestas", class ="btn-default")
      )
    ),
    h3("Formulario general de encuestas"),
    hr(),
    div(class="shadowForm",
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
          )
        ),
        hr(),
        uiOutput(ns("outGuardar"), style = "width: 100%")
      )
  )
}

#' investigacionFormularioGeneral Server Function
#'
#' @noRd 
mod_investigacionFormularioGeneral_server <- function(input, output, session, bd, usuario ,parent_session = NULL, showListadoForm = NULL, idFormGeneral = NULL, readOnly = NULL){
  ns <- session$ns
  
  output$outGuardar <- renderUI({
    if(readOnly$val == FALSE){
      tagList(
        fluidRow( class ="padding15-25",
                  actionButton(inputId = ns("guardar"), "Guardar", class = "btn-primary")
        )
      )
    }else{
      tagList(
        fluidRow( class ="padding15-25",
                  actionButton(inputId = ns("actualizar"), "Actualizar", class = "btn-primary")
        )
      )
    }
  })
  
  observeEvent(input$atras,{
    showListadoForm$val <- 1
    readOnly$val <- FALSE
  })
  
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
      showListadoForm$val <- 1 # Se regresa al listado
      gargoyle::trigger("encuestasGeneral")
    }
  })
  
  observeEvent(input$actualizar, {
    if(validarFormularioGeneral(input$nombre, input$casaEncuestadora, input$poblacionObjetivo, input$fechaInicio, input$fechaFin)){
      fA <- lubridate::now(tz = "America/Mexico_City") %>% as.character()
      formGeneral <- tibble::tibble(
        nombre = input$nombre,
        casaEncuestadora = input$casaEncuestadora,
        poblacionObjetivo = input$poblacionObjetivo,
        fechaInicio = as.character(input$fechaInicio),
        fechaFin = as.character(input$fechaFin),
        fechaEdicion = fA,
        usuarioEdicion = usuario$user,
      )
      condition <- glue::glue("idFormGeneral = {idFormGeneral$val}")
      updateBd(pool, formGeneralBd, formGeneral, condition)
      gargoyle::trigger("encuestasGeneral")
      showListadoForm$val <- 1 # Se regresa al listado
      readOnly$val <- FALSE
    }
  })
  
  infoEncuesta <- reactiveVal(NULL)
  observe({
    if(readOnly$val == TRUE){
      infoEncuesta(bd$encuestas %>% filter(activo == 1 & idFormGeneral == !! idFormGeneral$val) %>% collect())
      updateTextInput(session = parent_session, inputId = ns("nombre"), value = infoEncuesta()$nombre)
      updateTextInput(session = parent_session, inputId = ns("casaEncuestadora"), value = infoEncuesta()$casaEncuestadora)
      updateTextInput(session = parent_session, inputId = ns("poblacionObjetivo"), value = infoEncuesta()$poblacionObjetivo)
      updateDateInput(session = parent_session, inputId = ns("fechaInicio"), value = infoEncuesta()$fechaInicio)
      updateDateInput(session = parent_session, inputId = ns("fechaFin"), value = infoEncuesta()$fechaFin)
    }
  })
}
  
  ## To be copied in the UI
  # mod_investigacionFormularioGeneral_ui("investigacionFormularioGeneral_ui_1")
  
  ## To be copied in the server
  # callModule(mod_investigacionFormularioGeneral_server, "investigacionFormularioGeneral_ui_1")
  
  