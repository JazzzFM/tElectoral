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
    extendShinyjs(text = jscode, functions = "disableTab"),
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
  
  gira <- reactiveValues(paso1 = NULL, paso2 = NULL, paso2Tiempos = NULL, paso3 = NULL)
  # Paso 1
  callModule(mod_giraPaso1_server, "giraPaso1_ui_1", gira)
  # Lugares gira
  callModule(mod_lugaresGira_server, "lugaresGira_ui_1", gira)
  # Paso 3
  callModule(mod_giraPaso3_server, "giraPaso3_ui_1", gira, parent_session)
  observe({
    if(!is.null(gira$paso1)){
      js$disableTab("paso1")
      shinyjs::show(selector = paste0("#", ns("Paso2")))
      updateTabsetPanel(inputId = "TabsGira", selected = "paso2", parent_session)
    }
    if(!is.null(gira$paso2)){
      js$disableTab("paso2")
      shinyjs::show(selector = paste0("#", ns("Paso3")))
      updateTabsetPanel(inputId = "TabsGira", selected = "paso3", parent_session)
    }
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 

