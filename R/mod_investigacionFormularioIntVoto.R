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
    h3("Formulario de intención de voto"),
    p("A continuacion, ..."),
    useShinyjs(),
    fluidRow(
      column(12, 
             pickerInput(inputId = ns("tipoIntVoto"), label = "Tipo de intención de voto", choices = c("Candidato + Partido", "Candidato", "Partido"))
       ),
      column(12,
             textInput(inputId=ns("pregunta"), label = "Pregunta", placeholder = "...")
       ),
      column(12,
             textInput(inputId=ns("noSabeNoContesto"), label = "¿Cómo se registró la pregunta 'No sabe/No contestó'?", placeholder = "Explique...")
      ),
      column(6,
             numericInput(inputId=ns("careos"), label = "Cantidad de careos", value = 0, min = 0)
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
            actionButton(inputId = ns("addFila"), icon = icon("plus"), label = "Añadir fila", width = "150px")
        )
    ),
    hr(),
    actionButton(inputId = ns("guardar"), label = "Guardar", class ="btn-definitive")
    
  )
}
    
#' investigacionFormularioIntVoto Server Function
#'
#' @noRd 
mod_investigacionFormularioIntVoto_server <- function(input, output, session, usuario ,parent_session = NULL, showForm = NULL){
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
                      numericInput(inputId = ns(glue::glue("resultado-{uiCount$val}")), value = 0, min = 0, max= 10, label = ""),
                      HTML(input_btns(ns("eliminar"), users = uiCount$val, tooltip = "Eliminar", icon ="trash-o", status = "danger"))
                  )
             )
    uiCount$val <- uiCount$val+1
  })
  
  observeEvent(input$guardar, {
    if(uiCount$val > 1){
      fA <- lubridate::now(tz = "America/Mexico_City") %>% as.character()
      
      if(validarFormularioIntVoto(input$tipoIntVoto, input$pregunta, input$noSabeNoContesto, input$careos)){
        
        intencionVoto <- tibble::tibble(
          idFormGeneral = 1,
          tipoIntencionVoto = input$tipoIntVoto,
          pregunta = input$pregunta,
          siNoExplicacion = input$noSabeNoContesto,
          careos = input$careos,
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
}
    
## To be copied in the UI
# mod_investigacionFormularioIntVoto_ui("investigacionFormularioIntVoto_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionFormularioIntVoto_server, "investigacionFormularioIntVoto_ui_1")
 
