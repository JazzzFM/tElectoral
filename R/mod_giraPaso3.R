#' giraPaso3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import shinyjs
mod_giraPaso3_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    h3("Asignación de eventos"),
    p("Asigne eventos a los lugares que ha escogido en el paso 2"),
    uiOutput(ns("outEventos")),
    actionButton(ns("guardar"), "Guardar", class = "btn-primary")
  )
}

#' giraPaso3 Server Function
#'
#' @noRd 

mod_giraPaso3_server <- function(input, output, session, gira = NULL){
  ns <- session$ns
  
  listaEventos <- reactiveValues(eventos = NULL)
  
  
  output$outEventos <- renderUI({
    if(!is.null(gira$paso2)){
      lapply(seq_along(gira$paso2$lugares), function(i) {
        mod_lugaresPaso3_ui(ns(glue::glue("lugaresPaso3_ui_{i}")), titulo = gira$paso2$lugares[i])
      }) 
    }
  })
  
  observe({
    listaEventos$eventos <- seq_along(gira$paso2$lugares) %>% map(~callModule(mod_lugaresPaso3_server,
                                                                      glue::glue("lugaresPaso3_ui_{.x}"),
                                                                      lugar = gira$paso2$lugares[.x]))
  })

  observeEvent(input$guardar, {
    seq_along(listaEventos$eventos) %>% map(~listaEventos$eventos[[.x]]()) %>% do.call(rbind,.)
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 

