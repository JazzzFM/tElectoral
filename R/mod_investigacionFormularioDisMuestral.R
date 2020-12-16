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
    fluidRow(
      column(width = 6,
             actionButton(inputId = ns("atras"), label = "Regresar al listado de diseño muestral", class ="btn-default")
      )
    ),
    h3("Formulario de diseño muestral "),
    tags$hr(),
    div(class="shadowForm",
        fluidRow(
          column(width = 4,
                 pickerInput(label = "Modo de levantamiento",
                             choices = c("Seleccione" = '',"Vivienda","Telefónica","Internet"),
                             inputId = ns("modoLevantamiento"), selected = 0)
          ),
          column(width = 4,
                 numericInput(inputId = ns("numeroEntrevistas"),
                              label = "Número de entrevistas",
                              value = 0,
                              min = 0)
          ),
          column(width = 4,
                 prettyRadioButtons(label = "Aleatoria", choices = c("Sí", "No"),
                                    inputId = ns("aleatoria"), selected = 0)
          ),
          column(width = 12, 
                 textInput(inputId = ns("marcoMuestral"), label = "Marco Muestral",
                           placeholder = "Respuesta libre ...")
          ),
          
          column(width = 3,
                 prettyRadioButtons(label = "Poloetápica", choices = c("Sí", "No"),
                                    inputId = ns("poliEtapa"), selected = 0)
          ),
          column(width = 3,
                 shinyjs::hidden(numericInput(inputId = ns("nivelpoliEtapa"),
                                              label = "¿Cuántos niveles?",
                                              value = 1,
                                              min = 1,
                                              max = 10))
          ),
          column(width = 3,
                 prettyRadioButtons(label = "Estratificada", choices = c("Sí", "No"),
                                    inputId = ns("estratificada"), selected = 0)
          ),
          column(width = 3,
                 shinyjs::hidden(numericInput(inputId = ns("nivielEstratificada"),
                                              label = "¿Cuántos niveles?",
                                              value = 1,
                                              min = 1,
                                              max = 10))
          ),
          column(width = 3,
                 prettyRadioButtons(label = "Conglomerados", choices = c("Sí", "No"),
                                    inputId = ns("conglomerados"), selected = 0)
          ),
          column(width = 3,
                 shinyjs::hidden(numericInput(inputId = ns("nivielConglomerados"),
                                              label = "¿Cuántos niveles?",
                                              value = 1,
                                              min = 1,
                                              max = 10))
          ),
          column(width = 12,
                 selectizeInput(inputId = ns("unidadMuestral"),
                                choices = c("Seleccione un candidato" = "",
                                            tbl(pool,"tElectoralTest_MarcoMuestral") %>%
                                              collect() %>% pull(nombreMarco)),
                                            label = "Unidad muestral")
          ),
          column(width = 12,
                 numericInput(inputId = ns("nivelConfianza"),
                              label = "Porcentaje de nivel de confianza",
                              value = 0,
                              min = 0,
                              max = 100)
          ),
          column(width = 12,
                 numericInput(inputId = ns("margenError"),
                              label = "Porcentaje de margen de error",
                              value = 0,
                              min = 0,
                              max = 100)
          ),
          column(width = 12,
                 textAreaInput(inputId = ns("observaciones"), label = 'Observaciones', rows = 6,placeholder = "Respuesta libre ..."),
          ),
        ),
        hr(),
        fluidRow(
          uiOutput(ns("outGuardar"), style = "width: 100%")
        )
    ),
  )
}
    
#' investigacionFormularioDisMuestral Server Function
#'
#' @noRd 

mod_investigacionFormularioDisMuestral_server <- function(input, output, session,  bd, usuario ,parent_session = NULL, showListadoForm = NULL, idFormGeneral = NULL, readOnly = NULL, idDMuestral = NULL){
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
    insertBd(pool, formDisMuestralBd, bd = disMuestral)
    showListadoForm$val <- 1 # Se regresa al listado
    gargoyle::trigger("disMuestral")
    }
  })
  observeEvent(input$agregarCareo, {
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
      insertBd(pool, formDisMuestralBd, bd = disMuestral)
      
    }
  })
  observeEvent(input$atras,{
    showListadoForm$val <- 1
  })
  output$outGuardar <- renderUI({
    if(readOnly$val == FALSE){
      tagList(
        fluidRow(class="padding15-25",
          column(width = 6,
                 actionButton(inputId = ns("agregarCareo"), label = "Guardar y agregar otro", class ="btn-default")
          ),
          column(width = 6,
                 actionButton(inputId = ns("guardar"), label = "Guardar y terminar", class ="btn-primary pull-right")
          )
        )
      )
    }
  })
  infoDisMuestral <- reactiveVal(NULL)
  observe({
    if(idDMuestral$val != 0){
      infoDisMuestral(bd$listadoDisMuestral %>% filter(activo == 1 & idFormGeneral == !! idFormGeneral$val & idDMuestral == !! idDMuestral$val) %>% collect())
      updatePickerInput(session = parent_session, inputId = ns("modoLevantamiento"), selected = infoDisMuestral()$modoLevantamiento)
      updateTextInput(session = parent_session, inputId = ns("marcoMuestral"), value = infoDisMuestral()$marcoMuestral)
      updateNumericInput(session = parent_session, inputId = ns("numeroEntrevistas"), value = infoDisMuestral()$numeroEntrevistas)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("aleatoria"), selected = infoDisMuestral()$aleatoria)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("poliEtapa"), selected = infoDisMuestral()$poliEtapa)
      updateNumericInput(session = parent_session, inputId = ns("nivelpoliEtapa"), value = infoDisMuestral()$nivelpoliEtapa)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("estratificada"), selected = infoDisMuestral()$estratificada)
      updateNumericInput(session = parent_session, inputId = ns("nivielEstratificada"), value = infoDisMuestral()$nivielEstratificada)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("conglomerados"), selected = infoDisMuestral()$conglomerados)
      updateNumericInput(session = parent_session, inputId = ns("nivielConglomerados"), value = infoDisMuestral()$nivielConglomerados)
      updatePickerInput(session = parent_session, inputId = ns("unidadMuestral"), selected = infoDisMuestral()$unidadMuestral)
      updateNumericInput(session = parent_session, inputId = ns("nivelConfianza"), value = infoDisMuestral()$nivelConfianza)
      updateNumericInput(session = parent_session, inputId = ns("margenError"), value = infoDisMuestral()$margenError)
      updateTextAreaInput(session = parent_session , inputId = ns("observaciones"), value = infoDisMuestral()$observaciones)
    }
  })
}
    
## To be copied in the UI
# mod_investigacionFormularioDisMuestral_ui("investigacionFormularioDisMuestral_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionFormularioDisMuestral_server, "investigacionFormularioDisMuestral_ui_1")
