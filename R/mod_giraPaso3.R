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
    actionButton(ns("GuardarPaso3"), "Guardar", class = "btn-primary")
  )
}

#' giraPaso3 Server Function
#'
#' @noRd 

mod_giraPaso3_server <- function(input, output, session, gira = NULL){
  ns <- session$ns
  eventos <- tibble()
  observe({
    output$outEventos <- renderUI({
      if(!is.null(gira$paso2)){
        lapply(1:length(gira$paso2$lugares), function(i) {
          titulo <- gira$paso2$lugares[i]
          fluidRow(
            column(width = 12,
                   shinydashboardPlus::boxPlus(
                     title = titulo,
                     collapsible = T,
                     width = 12,
                     tags$button(
                       id = ns(paste0("add-evento-",i)),
                       "Agregar evento",
                       class = "btn btn-primary"
                     )
                   )
            )
          )
        }) 
      }
    })
  #   if(!is.null(gira$paso2)){
  #     
  #     
  #     # for(i in 1:length(gira$paso2$lugares)){
  #     #   local({
  #     #     idName <- ns(paste0("add-evento-", i))
  #     #     print(idName)
  #     #     shinyjs::onclick(id = idName, {
  #     #       print(paste0("button 1: ", "add-evento-", i))
  #     #     })
  #     #   })
  #     # }
  #   }
 })
  # observeEvent(input$GuardarPaso3, {
  #   
  # })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
