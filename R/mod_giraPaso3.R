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
    actionButton(ns("guardar"), "Guardar", class = "btn-definitive")
  )
}

#' giraPaso3 Server Function
#'
#' @noRd 

mod_giraPaso3_server <- function(input, output, session, gira = NULL, parent_session = NULL){
  ns <- session$ns
  
  listaEventos <- reactiveValues(eventos = c())
  horariosOcupados <- reactiveValues()
  eventos <- reactiveValues()
  uiCount <- reactiveValues(val = 1)
  output$outEventos <- renderUI({
    if(!is.null(gira$paso2)){
      lapply(seq_along(gira$paso2$lugares), function(i) {
        mod_lugaresPaso3_ui(ns(glue::glue("lugaresPaso3_ui_{i}")), titulo = gira$paso2$lugares[i])
      }) 
    }
  })
  
  observe({
    if(length(gira$paso2$lugares) > 0){
      listaEventos$eventos <- seq_along(gira$paso2$lugares) %>% map(~callModule(mod_lugaresPaso3_server,
                                                                                glue::glue("lugaresPaso3_ui_{.x}"),
                                                                                lugar = gira$paso2$lugares[.x],
                                                                                lugarAntes = ifelse(.x != 1, gira$paso2$lugares[sum(.x, -1)], ""),
                                                                                parent_session = parent_session,
                                                                                eventos = eventos,
                                                                                uiCount = uiCount,
                                                                                paso1 = gira$paso1,
                                                                                tiempoActual = ifelse(.x != length(gira$paso2$lugares), gira$paso2Tiempos[.x], 0),
                                                                                tiempoAntes = ifelse(.x != 1, gira$paso2Tiempos[sum(.x, -1)], 0),
                                                                                index = .x
      ))
    }
  })

  observeEvent(input$guardar, {
    browser()
    # if(is.null(seq_along(listaEventos$eventos) %>% detect(~is.null(listaEventos$eventos[[.x]]())))){
    #   if(is.null(seq_along(listaEventos$eventos) %>% detect(~nrow(listaEventos$eventos[[.x]]()) == 0))){
    #     seq_along(listaEventos$eventos) %>% map(~listaEventos$eventos[[.x]]()) %>% do.call(rbind,.)  
    #   }else{
    #     shinyalert::shinyalert(title = "Debe añadir al menos un evento por lugar")  
    #   } 
    # }else{
    #   shinyalert::shinyalert(title = "Debe añadir al menos un evento por lugar")  
    # }
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 

