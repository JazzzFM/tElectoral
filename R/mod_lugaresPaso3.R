#' lugaresPaso3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_lugaresPaso3_ui <- function(id, titulo){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             shinydashboardPlus::boxPlus(
               title = titulo,
               collapsible = T,
               width = 12,
               actionButton(
                 inputId = ns("addEvento"),
                 "Agregar evento",
                 class = "btn btn-primary"
               )
             )
      )
    )
  )
}
    
#' lugaresPaso3 Server Function
#'
#' @noRd 
mod_lugaresPaso3_server <- function(input, output, session, lugar){
  ns <- session$ns
  
  observeEvent(input$addEvento,{

    showModal(modalDialog(title = "Evento",
                          mod_evento_ui(ns(glue::glue("evento_ui_{uiCount()}"))),
                          footer = actionButton(ns("agregar"),"Agregar")
                          )
              )
  })
 
  eventos <- reactiveValues()
  uiCount <- reactiveVal(1)
  observeEvent(input$agregar,{
    removeModal()
    eventos[[as.character(uiCount())]] <- callModule(mod_evento_server, glue::glue("evento_ui_{uiCount()}"))
    insertUI(selector = glue::glue("#{ns('addEvento')}"),where = "beforeBegin",
             ui = p(eventos[[as.character(uiCount())]]()$nombre))
    uiCount(uiCount()+1)
  })
  
  # observe({
  #   req(eventos[["1"]]())
  #   
  # })
  ev <- reactive({
    seq_len(uiCount()-1) %>% map(~eventos[[as.character(.x)]]() %>% mutate(lugar = lugar)) %>% do.call(rbind,.)
  })
  return(ev)
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
