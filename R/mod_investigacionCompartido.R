#' investigacionCompartido UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyjs
mod_investigacionCompartido_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("investigacionContainer"))
  )
}
    
#' investigacionCompartido Server Function
#'
#' @noRd 
mod_investigacionCompartido_server <- function(input, output, session, bd, usuario, parent_session = NULL){
  ns <- session$ns
  showForm <- reactiveValues(val = 1)
  idFormGeneral <- reactiveValues(val = 0)
  
  callModule(mod_investigacionEncuestas_server, "investigacionEncuestas_ui_1", bd, usuario, parent_session, showForm, idFormGeneral)
  callModule(mod_investigacionListadoDisMuestral_server, "investigacionListadoDisMuestral_ui_1", bd, usuario, parent_session, showForm, idFormGeneral)
  callModule(mod_investigacionListadoIntVoto_server, "investigacionListadoIntVoto_ui_1", bd, usuario, parent_session, showForm, idFormGeneral)
  callModule(mod_investigacionListadoCuestionario_server, "investigacionListadoCuestionario_ui_1", bd, usuario, parent_session, showForm, idFormGeneral)
  
  output$investigacionContainer <- renderUI({ # Encuestas
    if(showForm$val == 1){
      mod_investigacionEncuestas_ui(ns("investigacionEncuestas_ui_1"))
    }
    else if(showForm$val == 2){ # Listado Diseño muestral
      mod_investigacionListadoDisMuestral_ui(ns("investigacionListadoDisMuestral_ui_1"))
    }
    else if(showForm$val == 3){
      mod_investigacionListadoIntVoto_ui(ns("investigacionListadoIntVoto_ui_1"))
    }
    else if(showForm$val == 4){
      mod_investigacionListadoCuestionario_ui(ns("investigacionListadoCuestionario_ui_1"))
    }
  })
}

## To be copied in the UI
# mod_investigacionCompartido_ui("investigacionCompartido_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionCompartido_server, "investigacionCompartido_ui_1")
 
