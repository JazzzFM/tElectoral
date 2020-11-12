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
    fluidRow(
      column(12, 
             pickerInput(inputId = ns("tipoIntVoto"), label = "Tipo de intención de voto", choices = c("Seleccione", "Candidato + Partido", "Candidato", "Partido"))
       ),
      column(12,
             textInput(inputId=ns("pregunta"), label = "Pregunta", placeholder = "...")
       ),
      column(12,
             textInput(inputId=ns("noSabeNoContesto"), label = "¿Cómo se registró la pregunta 'No sabe/No contestó'?", placeholder = "Explique...")
      ),
      column(6,
             numericInput(inputId=ns("resultado"), label = "Resultado", value = 0, min = 0)
      ),
      column(6,
             numericInput(inputId=ns("numCandidatos"), label = "Número de candidatos", value = 0, min = 0)
      ),
    ),
    
    h3("Registro"),
    div(class = "flexTable", id = "tablaCandidatos",
        div( class="four-columns",
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
mod_investigacionFormularioIntVoto_server <- function(input, output, session, parent_session){
  ns <- session$ns
  uiCount <- reactiveValues(val = 1)
  observeEvent(input$addFila, {
    insertUI(selector = "#tablaCandidatos .candContainer", where = "beforeEnd",
             ui = div(class="four-columns", id=glue::glue("candidato-{uiCount$val}"),
                      selectizeInput(inputId = ns("cand1"), choices = c("Algo", "algo2", "Algo3"), label = ""),
                      selectizeInput(inputId = ns("cand1"), choices = c("Algo", "algo2", "Algo3"), label = ""),
                      numericInput(inputId = ns("resultado"), value = 0, min = 0, max= 10, label = ""),
                      HTML(input_btns(ns("eliminar"), users = uiCount$val, tooltip = "Eliminar", icon ="trash-o", status = "danger"))
                  )
             )
    uiCount$val <- uiCount$val+1
  })
  
  observeEvent(input$eliminar, {
    removeUI(selector = glue::glue("#candidato-{input$eliminar}"))
  })
}
    
## To be copied in the UI
# mod_investigacionFormularioIntVoto_ui("investigacionFormularioIntVoto_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionFormularioIntVoto_server, "investigacionFormularioIntVoto_ui_1")
 
