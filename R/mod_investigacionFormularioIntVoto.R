#' investigacionFormularioIntVoto UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_investigacionFormularioIntVoto_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
             actionButton(inputId = ns("atras"), label = "Regresar al listado de intención de voto", class ="btn-default")
             )
    ),
    h3("Formulario de intención de voto"),
    useShinyjs(),
    fluidRow(
      column(12, 
             pickerInput(inputId = ns("tipoIntVoto"), label = "Tipo de intención de voto", choices = c("Candidato + Partido", "Candidato", "Partido"))
       ),
      column(12,
             textInput(inputId=ns("pregunta"), label = "Escriba la pregunta tal y como viene en la encuesta", placeholder = "...")
       ),
      column(12,
             textInput(inputId=ns("noSabeNoContesto"), label = "Escriba como se reportó la opción 'No sabe/No contestó'", placeholder = "Explique...")
      )
    ),
    
    h3("Registro"),
    div(class = "flexTable", id = "tablaCandidatos",
        div( class="four-columns", id="tableHeader",
          h4("Candidato"),
          h4("Partido"),
          h4("Resultado"),
          h4("Acciones")
        ),
        div(class="candContainer",
            
        ),
        div(class="footer",
            uiOutput(ns("outAddFila"))
        )
    ),
    hr(),
    fluidRow(
      uiOutput(ns("outGuardar"))
    )
    
    
  )
}
    
#' investigacionFormularioIntVoto Server Function
#'
#' @noRd 
mod_investigacionFormularioIntVoto_server <- function(input, output, session, bd, usuario ,parent_session = NULL, showListadoForm = NULL, idFormGeneral = NULL, readOnly = NULL, idIntencionVoto = NULL){
  ns <- session$ns
  uiCount <- reactiveValues(val = 1)
  observeEvent(input$addFila, {
    clase <- ""
    claseCandidato <- ""
    clasePartido <- ""
    if(input$tipoIntVoto == "Candidato + Partido")
      clase <- "four-columns"
    else if(input$tipoIntVoto == "Candidato"){
      clase <- "three-columns"
      clasePartido="none"
    }else if(input$tipoIntVoto == "Partido"){
      clase <- "three-columns"
      claseCandidato="none"
    }
    insertUI(selector = "#tablaCandidatos .candContainer", where = "beforeEnd",
             ui = div(class=clase, id=glue::glue("row-candidato-{uiCount$val}"),
                      div(class=glue::glue("ctr-1 {claseCandidato}"), selectizeInput(inputId = ns(glue::glue("nombreCandidato-{uiCount$val}")), choices = c("Juan", "Alejandro", "María"), label = "")),
                      div(class=glue::glue("ctr-2 {clasePartido}"), selectizeInput(inputId = ns(glue::glue("partido-{uiCount$val}")), choices = c("PRD", "PRI", "PAN"), label = "")),
                      numericInputIcon(icon = list(icon("percent"),NULL),inputId = ns(glue::glue("resultado-{uiCount$val}")), value = 0, min = 0, max= 100, label = ""),
                      HTML(input_btns(ns("eliminar"), users = uiCount$val, tooltip = "Eliminar", icon ="trash-o", status = "danger"))
                  )
             )
    uiCount$val <- uiCount$val+1
  })
  observeEvent(input$agregarCareo, {
    if(uiCount$val > 1){
      fA <- lubridate::now(tz = "America/Mexico_City") %>% as.character()
      
      if(validarFormularioIntVoto(input$tipoIntVoto, input$pregunta, input$noSabeNoContesto)){
        
        intencionVoto <- tibble::tibble(
          idFormGeneral = idFormGeneral$val,
          tipoIntencionVoto = input$tipoIntVoto,
          pregunta = input$pregunta,
          siNoExplicacion = input$noSabeNoContesto,
          fechaAlta = fA,
          usuarioCrea = usuario$user,
          activo = 1
        )
        insertBd(pool, formIntVotoBd, bd = intencionVoto, F)
        idIntVoto <- tbl(pool, formIntVotoBd) %>% filter(fechaAlta == !!fA) %>% pull(idIntencionVoto)
        
        # Se obtienen candidatos de tabla
        id <- c()
        cand <- c()
        part <- c()
        res <- c()
        fechaAlta <- c()
        usuarioCrea <- c()
        activo <- c()
        for(i in 1:uiCount$val -1){
          if(!is.null(input[[(glue::glue("nombreCandidato-{sum(i,1)}"))]]) & !is.null(input[[(glue::glue("partido-{sum(i,1)}"))]]) & !is.null(input[[(glue::glue("resultado-{sum(i,1)}"))]])){
            if(input[[(glue::glue("nombreCandidato-{sum(i,1)}"))]] != "" & input[[(glue::glue("partido-{sum(i,1)}"))]] != "" & input[[(glue::glue("resultado-{sum(i,1)}"))]] >= 0){
              id <- append(id, idIntVoto)
              cand <- append(cand, input[[(glue::glue("nombreCandidato-{sum(i,1)}"))]])
              part <- append(part, input[[(glue::glue("partido-{sum(i,1)}"))]])
              res <- append(res, input[[(glue::glue("resultado-{sum(i,1)}"))]])
              fechaAlta <- append(fechaAlta, fA)
              usuarioCrea <- append(usuarioCrea, usuario$user)
              activo <- append(activo, 1)
            }
          }
        }
        candidatos <- tibble::tibble(
          idIntencionVoto = id,
          candidato = cand,
          partido = part,
          resultado = res,
          fechaAlta = fechaAlta,
          usuarioCrea = usuarioCrea,
          activo = activo
        )
        insertBd(pool, formIntVotoRegistroBd, bd = candidatos)
        updatePickerInput(session = parent_session, inputId = ns("tipoIntVoto"), selected = "")
        updateTextInput(session = parent_session, inputId = ns("pregunta"), value = "")
        updateTextInput(session = parent_session, inputId = ns("noSabeNoContesto"), value = "")
        removeUI(multiple = T, selector = ".candContainer > *")
        uiCount$val <- 1
      }
    }else{
      shinyalert::shinyalert(title = "¡No hay candidatos!", 
                             text = "Debe especificar al menos un candidato.")
    }
  })
  observeEvent(input$guardar, {
    if(uiCount$val > 1){
      fA <- lubridate::now(tz = "America/Mexico_City") %>% as.character()
      
      if(validarFormularioIntVoto(input$tipoIntVoto, input$pregunta, input$noSabeNoContesto)){
        
        intencionVoto <- tibble::tibble(
          idFormGeneral = idFormGeneral$val,
          tipoIntencionVoto = input$tipoIntVoto,
          pregunta = input$pregunta,
          siNoExplicacion = input$noSabeNoContesto,
          fechaAlta = fA,
          usuarioCrea = usuario$user,
          activo = 1
        )
        insertBd(pool, formIntVotoBd, bd = intencionVoto)
        idIntVoto <- tbl(pool, formIntVotoBd) %>% filter(fechaAlta == !!fA) %>% pull(idIntencionVoto)
        
        # Se obtienen candidatos de tabla
        id <- c()
        cand <- c()
        part <- c()
        res <- c()
        fechaAlta <- c()
        usuarioCrea <- c()
        activo <- c()
        for(i in 1:uiCount$val -1){
          if(!is.null(input[[(glue::glue("nombreCandidato-{sum(i,1)}"))]]) & !is.null(input[[(glue::glue("partido-{sum(i,1)}"))]]) & !is.null(input[[(glue::glue("resultado-{sum(i,1)}"))]])){
            if(input[[(glue::glue("nombreCandidato-{sum(i,1)}"))]] != "" & input[[(glue::glue("partido-{sum(i,1)}"))]] != "" & input[[(glue::glue("resultado-{sum(i,1)}"))]] >= 0){
              id <- append(id, idIntVoto)
              cand <- append(cand, input[[(glue::glue("nombreCandidato-{sum(i,1)}"))]])
              part <- append(part, input[[(glue::glue("partido-{sum(i,1)}"))]])
              res <- append(res, input[[(glue::glue("resultado-{sum(i,1)}"))]])
              fechaAlta <- append(fechaAlta, fA)
              usuarioCrea <- append(usuarioCrea, usuario$user)
              activo <- append(activo, 1)
            }
          }
        }
        candidatos <- tibble::tibble(
          idIntencionVoto = id,
          candidato = cand,
          partido = part,
          resultado = res,
          fechaAlta = fechaAlta,
          usuarioCrea = usuarioCrea,
          activo = activo
        )
        insertBd(pool, formIntVotoRegistroBd, bd = candidatos)
        showListadoForm$val <- 1 # Se regresa al listado
        gargoyle::trigger("intencionVoto")
      }
    }else{
      shinyalert::shinyalert(title = "¡No hay candidatos!", 
                             text = "Debe especificar al menos un candidato.")
    }
  })
  
  observeEvent(input$eliminar, {
    removeUI(selector = glue::glue("#row-candidato-{input$eliminar}"))
    updateSelectizeInput(session = parent_session, inputId = ns(glue::glue("nombreCandidato-{input$eliminar}")), selected = "")
    updateSelectizeInput(session = parent_session, inputId = ns(glue::glue("partido-{input$eliminar}")), selected = "")
    updateSelectizeInput(session = parent_session, inputId = ns(glue::glue("resultado-{input$eliminar}")), selected = -1)
  })
  
  observeEvent(input$atras,{
    showListadoForm$val <- 1
  })
  output$outAddFila <- renderUI({
    if(readOnly$val == FALSE){
      actionButton(inputId = ns("addFila"), icon = icon("plus"), label = "Añadir fila", width = "150px")
    }
  })
  output$outGuardar <- renderUI({
    if(readOnly$val == FALSE){
      tagList(
        column(width = 6,
               actionButton(inputId = ns("agregarCareo"), label = "Guardar y agregar otro careo", class ="btn-default")
        ),
        column(width = 6,
               actionButton(inputId = ns("guardar"), label = "Guardar y terminar", class ="btn-primary pull-right")
        )
      )
    }
  })
  infoIntencion <- reactiveVal(NULL)
  infoCandidatos <- reactiveVal(NULL)
  observe({
    if(idIntencionVoto$val != 0){
      infoIntencion(bd$listadoIntVoto %>% filter(activo == 1 & idFormGeneral == !! idFormGeneral$val & idIntencionVoto == !! idIntencionVoto$val) %>% collect())
      infoCandidatos(bd$intVotoRegistro %>% filter(activo == 1 & idIntencionVoto == !! idIntencionVoto$val) %>% collect())
      updatePickerInput(session = parent_session, inputId = ns("tipoIntVoto"), selected = infoIntencion()$tipoIntencionVoto)
      updateTextInput(session = parent_session, inputId = ns("pregunta"), value = infoIntencion()$pregunta)
      updateTextInput(session = parent_session, inputId = ns("noSabeNoContesto"), value = infoIntencion()$siNoExplicacion)
      disable(id = ns("tipoIntVoto"), selector = paste0("#",ns("tipoIntVoto")))
      
      # Inserción de filas
      clase <- ""
      claseCandidato <- ""
      clasePartido <- ""
      if(infoIntencion()$tipoIntencionVoto == "Candidato + Partido")
        clase <- "four-columns"
      else if(infoIntencion()$tipoIntencionVoto == "Candidato"){
        clase <- "three-columns"
        clasePartido="none"
        hide("#tableHeader h4:nth-of-type(2)")
      }else if(infoIntencion()$tipoIntencionVoto == "Partido"){
        clase <- "three-columns"
        claseCandidato="none"
        hide("#tableHeader h4:first-child")
      }
      for(x in 1:length(infoCandidatos()$idIntencionVoto)){
        item <- infoCandidatos()[x,]
        insertUI(selector = "#tablaCandidatos .candContainer", where = "beforeEnd",
                 ui = div(class=clase, id=glue::glue("row-candidato-{x}"),
                          div(class=glue::glue("ctr-1 {claseCandidato}"), selectizeInput(inputId = ns(glue::glue("nombreCandidato-{x}")), choices = c(item$candidato), label = "")),
                          div(class=glue::glue("ctr-2 {clasePartido}"), selectizeInput(inputId = ns(glue::glue("partido-{x}")), choices = c(item$partido), label = "")),
                          numericInputIcon(icon = list(icon("percent"),NULL),inputId = ns(glue::glue("resultado-{x}")), value = item$resultado, min = 0, max= 100, label = ""),
                          HTML(input_btns(ns("eliminar"), users = x, tooltip = "Eliminar", icon ="trash-o", status = "danger"))
                 )
        )
      }
    }
  })
}
    
## To be copied in the UI
# mod_investigacionFormularioIntVoto_ui("investigacionFormularioIntVoto_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionFormularioIntVoto_server, "investigacionFormularioIntVoto_ui_1")
 
