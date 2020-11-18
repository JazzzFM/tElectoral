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
      column(width = 4,
             dateInput(inputId = ns("fechaRegistro"), label = "Fecha de registo",
                       format = "dd/mm/yyyy", language = "es", value = Sys.Date(),
                       min = Sys.Date())
      ),
      column(width = 4,
             pickerInput(label = "Modo de levantamiento",
                         choices = c("Seleccione" = '',"Vivienda","Telefónica","Internet"),
                         inputId = ns("modoLevantamiento"), selected = 0)
             ),
      column(width = 4,
              textInput(inputId = ns("marcoMuestral"), label = "Marco Muestral",
                        placeholder = "Respuesta libre ...")
            ),
      column(width = 6, 
             numericInput(inputId = ns("numeroEntrevistas"),
                              label = "Número de entrevistas",
                              value = 0,
                              min = 0)
            ),
      column(width = 6,
        prettyRadioButtons(label = "Aleatoria", choices = c("Sí", "No"),
                           inputId = ns("aleatoria"), selected = 0)
      ),
      column(width = 6,
             prettyRadioButtons(label = "Poloetápica", choices = c("Sí", "No"),
                                inputId = ns("poliEtapa"), selected = 0)
      ),
      column(width = 6,
      shinyjs::hidden(numericInput(inputId = ns("nivelpoliEtapa"),
                   label = "¿Cuántos niveles?",
                   value = 1,
                   min = 1,
                   max = 10))
        ),
      column(width = 6,
             prettyRadioButtons(label = "Estratificada", choices = c("Sí", "No"),
                                inputId = ns("estratificada"), selected = 0)
      ),
      column(width = 6,
             shinyjs::hidden(numericInput(inputId = ns("nivielEstratificada"),
                                          label = "¿Cuántos niveles?",
                                          value = 1,
                                          min = 1,
                                          max = 10))
      ),
      column(width = 6,
             prettyRadioButtons(label = "Conglomerados", choices = c("Sí", "No"),
                                inputId = ns("conglomerados"), selected = 0)
      ),
      column(width = 6,
             shinyjs::hidden(numericInput(inputId = ns("nivielConglomerados"),
                                          label = "¿Cuántos niveles?",
                                          value = 1,
                                          min = 1,
                                          max = 10))
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
      column(width = 12,
             textAreaInput(inputId = ns("observaciones"), label = 'Observaciones', placeholder = "Respuesta libre ..."),
      ),
      actionButton(inputId = ns("guardar"), "Guardar", class = "btn btn-definitive")
    )
  )
}
    
#' investigacionFormularioDisMuestral Server Function
#'
#' @noRd 

mod_investigacionFormularioDisMuestral_server <- function(input, output, session, bd, usuario, parent_session = NULL, showForm = NULL){
  ns <- session$ns
  observeEvent(input$poliEtapa, {
    if(input$poliEtapa == "Sí"){
      shinyjs::show(
        selector = paste0("#", ns("nivelpoliEtapa"))
      )
    }else{
      shinyjs::hide(
        selector = paste0("#", ns("nivelpoliEtapa"))
      )
      updateNumericInput(session = parent_session, inputId = ns("nivelpoliEtapa"), value = 1)
    }
  })
  observeEvent(input$estratificada, {
    if(input$estratificada == "Sí"){
      shinyjs::show(
        selector = paste0("#", ns("nivielEstratificada"))
      )
    }else{
      shinyjs::hide(
        selector = paste0("#", ns("nivielEstratificada"))
      )
      updateNumericInput(session = parent_session, inputId = ns("nivielEstratificada"), value = 1)
    }
  })
  observeEvent(input$conglomerados, {
    if(input$conglomerados == "Sí"){
      shinyjs::show(
        selector = paste0("#", ns("nivielConglomerados"))
      )
    }else{
      shinyjs::hide(
        selector = paste0("#", ns("nivielConglomerados"))
      )
      updateNumericInput(session = parent_session, inputId = ns("nivielConglomerados"), value = 1)
    }
  })
  observeEvent(input$guardar, {
  if(validarFormularioDisMuestral(input$modoLevantamiento, input$marcoMuestral, input$numeroEntrevistas,
                               input$aleatoria, input$poliEtapa, input$estratificada, input$conglomerados,
                               input$nivelpoliEtapa, input$nivielEstratificada, input$nivielConglomerados,
                               input$unidadMuestral, input$nivelConfianza, input$margenError)){
    
    fA <- lubridate::now(tz = "America/Mexico_City") %>% as.character()
    disMuestral <- tibble::tibble(
          idFormGeneral = 1,
          modoLevantamiento = input$modoLevantamiento,
          marcoMuestral = input$marcoMuestral,
          numeroEntrevistas = input$numeroEntrevistas,
          aleatoria = input$aleatoria,
          poliEtapa = input$poliEtapa,
          nivelpoliEtapa = input$nivelpoliEtapa,
          estratificada = input$estratificada,
          nivielEstratificada = input$nivielEstratificada,
          conglomerados = input$conglomerados,
          nivielConglomerados = input$nivielConglomerados,
          unidadMuestral = input$unidadMuestral,
          nivelConfianza = input$nivelConfianza,
          margenError = input$margenError,
          observaciones = input$observaciones,
          fechaAlta = fA,
          fechaEdicion = NULL,
          usuarioCrea = usuario$user,
          usuarioEdicion = NULL,
          activo = 1)
    
    tryCatch(                      
      expr = {
        insertBd(pool, formDisMuestralBd, bd = disMuestral)
        shinyalert::shinyalert(title = "Los datos se subieron correctamente.")
        },
      error = function(e){    
        shinyalert::shinyalert(title = "Los datos no se subieron, intente más tarde o revise su conexión..")
      },
      warning = function(w){    
        shinyalert::shinyalert(title = "Revise su conexión.")
      }
    )
    
    }
  })
     
}
    
## To be copied in the UI
# mod_investigacionFormularioDisMuestral_ui("investigacionFormularioDisMuestral_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionFormularioDisMuestral_server, "investigacionFormularioDisMuestral_ui_1")
