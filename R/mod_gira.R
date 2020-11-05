#' gira UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyjs V8
mod_gira_ui <- function(id){
  ns <- NS(id)
  jscode <- "
  shinyjs.disableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    tab.bind('click.tab', function(e) {
      e.preventDefault();
      return false;
    });
    tab.addClass('disabled');
  }
  
  shinyjs.enableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    tab.unbind('click.tab');
    tab.removeClass('disabled');
  }
  "
  tagList(
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("disableTab", "enableTab")),
    shinyjs::hidden(actionButton(inputId = ns("volverEmpezar"), "Volver a empezar", class = "btn-primary pull-right")),
    tabsetPanel(
      id = "TabsGira",
      tabPanel(title = "Paso 1", value = "paso1", id="paso1",
               mod_giraPaso1_ui(ns("giraPaso1_ui_1"))
      ),
      tabPanel(title = "Paso 2", value = "paso2", id="paso2",
               shinyjs::hidden(div(id = ns("Paso2"),
                   mod_lugaresGira_ui(ns("lugaresGira_ui_1")
                   )))
      ),
      tabPanel(title = "Paso 3", value = "paso3", id="paso3",
               shinyjs::hidden(div(id = ns("Paso3"),
                                   mod_giraPaso3_ui(ns("giraPaso3_ui_1"))
                                   ))
      )
    )
  )
}

#' gira Server Function
#'
#' @noRd 
mod_gira_server <- function(input, output, session, parent_session){
  ns <- session$ns
  
  hideTab(inputId = "TabsGira", target = "paso2", session = parent_session)
  hideTab(inputId = "TabsGira", target = "paso3", session = parent_session)
  
  gira <- reactiveValues(paso1 = NULL, paso2 = NULL, paso2Tiempos = NULL, paso3 = NULL)
  reseted <- reactiveValues(value = F, resPaso1 = F, resPaso2 = F, resPaso3 = F)
  # Paso 1
  callModule(mod_giraPaso1_server, "giraPaso1_ui_1", gira, parent_session, reseted)
  # Lugares gira
  callModule(mod_lugaresGira_server, "lugaresGira_ui_1", gira, parent_session, reseted)
  # Paso 3
  callModule(mod_giraPaso3_server, "giraPaso3_ui_1", gira, parent_session, reseted)
  observe({
    if(!is.null(gira$paso1)){
      js$disableTab("paso1")
      shinyjs::show(selector = paste0("#", ns("Paso2")))
      shinyjs::show(selector = paste0("#", ns("volverEmpezar")))
      updateTabsetPanel(inputId = "TabsGira", selected = "paso2", parent_session)
    }
    if(!is.null(gira$paso2)){
      js$disableTab("paso2")
      shinyjs::show(selector = paste0("#", ns("Paso3")))
      updateTabsetPanel(inputId = "TabsGira", selected = "paso3", parent_session)
    }
    if(reseted$resPaso1 & reseted$resPaso2 & reseted$resPaso3){
      reseted$value <- F
      reseted$resPaso1 <- F
      reseted$resPaso2 <- F
      reseted$resPaso3 <- F
    }
  })
  
  observeEvent(input$volverEmpezar, {
    shinyalert::shinyalert(title = "¿Está seguro?", 
                           text = glue::glue("Los cambios realizados hasta ahora se perderán y tendrá que volver a ingresar toda su información."),
                           showCancelButton = T,showConfirmButton = T,cancelButtonText = "No",
                           confirmButtonText = "Sí", 
                           callbackR = function(x) if(x) {
                             reseted$value <- T
                             js$enableTab("paso1")
                             js$enableTab("paso2")
                             js$enableTab("paso3")
                             shinyjs::hide(selector = paste0("#", ns("Paso3")))
                             shinyjs::hide(selector = paste0("#", ns("Paso2")))
                             shinyjs::hide(selector = paste0("#", ns("volverEmpezar")))
                             gira$paso1 <- NULL
                             gira$paso2 <- NULL
                             gira$paso2Tiempos <- NULL
                             gira$paso3 <- NULL
                             updateTabsetPanel(inputId = "TabsGira", selected = "paso1", parent_session)
                             #reseted$value <- F
                           })
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 

