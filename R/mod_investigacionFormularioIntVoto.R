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
             numericInput(inputId=ns("resultado"), label = "Resultado", value = 0, min = 0)
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
mod_investigacionFormularioIntVoto_server <- function(input, output, session, parent_session = NULL, showForm = NULL){
  ns <- session$ns
  uiCount <- reactiveValues(val = 1)
  observeEvent(input$addFila, {
    clase <- ""
    if(input$tipoIntVoto == "Candidato + Partido")
      clase <- "four-columns"
    else {
      clase <- "three-columns"
    }
    insertUI(selector = "#tablaCandidatos .candContainer", where = "beforeEnd",
             ui = div(class=clase, id=glue::glue("row-candidato-{uiCount$val}"),
                      selectizeInput(inputId = ns(glue::glue("nombreCandidato-{uiCount$val}")), choices = c("Juan", "Alejandro", "María"), label = ""),
                      selectizeInput(inputId = ns(glue::glue("partido-{uiCount$val}")), choices = c("PRD", "PRI", "PAN"), label = ""),
                      numericInput(inputId = ns(glue::glue("resultado-{uiCount$val}")), value = 0, min = 0, max= 10, label = ""),
                      HTML(input_btns(ns("eliminar"), users = uiCount$val, tooltip = "Eliminar", icon ="trash-o", status = "danger"))
                  )
             )
    uiCount$val <- uiCount$val+1
  })
  
  observeEvent(input$guardar, {
    if(uiCount$val > 1){
      # Se obtienen candidatos de tabla
      cand <- c()
      part <- c()
      res <- c()
      for(i in 1:uiCount$val -1){
        if(!is.null(input[[(glue::glue("nombreCandidato-{sum(i,1)}"))]]) & !is.null(input[[(glue::glue("partido-{sum(i,1)}"))]]) & !is.null(input[[(glue::glue("resultado-{sum(i,1)}"))]])){
          if(input[[(glue::glue("nombreCandidato-{sum(i,1)}"))]] != "" & input[[(glue::glue("partido-{sum(i,1)}"))]] != "" & input[[(glue::glue("resultado-{sum(i,1)}"))]] >= 0){
            cand <- append(cand, input[[(glue::glue("nombreCandidato-{sum(i,1)}"))]])
            part <- append(part, input[[(glue::glue("partido-{sum(i,1)}"))]])
            res <- append(res, input[[(glue::glue("resultado-{sum(i,1)}"))]])  
          }
        }
      }
      
      if(validarFormularioIntVoto(input$tipoIntVoto, input$pregunta, input$noSabeNoContesto, input$resultado)){
        candidatos <- tibble::tibble(
          candidato = cand,
          partido = part,
          resultado = res
        )
        print(tibble::tibble(
          tipoIntencionVoto = input$tipoIntVoto,
          pregunta = input$pregunta,
          noSabeNoContesto = input$noSabeNoContesto,
          resultado = input$resultado
        )) 
        print(candidatos)
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
 
