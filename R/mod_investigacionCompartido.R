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
    useShinyjs(),
    # hidden(
    #   div(id = ns("formEncuestas"), mod_investigacionEncuestas_ui("investigacionEncuestas_ui_1"))
    #   ),
    # hidden(
    #   div(id = ns("formIntVoto"), mod_investigacionFormularioIntVoto_ui("investigacionFormularioIntVoto_ui_1"))
    #   )
  )
}
    
#' investigacionCompartido Server Function
#'
#' @noRd 
mod_investigacionCompartido_server <- function(input, output, session, parent_session = NULL){
  ns <- session$ns
  showForm <- reactiveValues(val = 1)
  
  #Encuestas
  #callModule(mod_investigacionEncuestas_server, "investigacionEncuestas_ui_1", parent_session, showForm)
  #Intención de voto
  #callModule(mod_investigacionFormularioIntVoto_server, "investigacionFormularioIntVoto_ui_1", parent_session, showForm)
  
  observe({
    print(showForm$val)
    if(showForm$val == 1){
      hideElementsInvestigacion()
      show(selector = paste0("#",ns("formEncuestas")))
    }
    else if(showForm$val == 2){
      hideElementsInvestigacion()
      show(selector = "#formDisMuestral") 
    }
    else if(showForm$val == 3){
      hideElementsInvestigacion()
      show(selector = paste0("#", ns("formIntVoto")))
      
    }
    else if(showForm$val == 4){
      hideElementsInvestigacion()
      show(selector = "#formCuestionario")
    }
  })
  
}
 
hideElementsInvestigacion <- function(){
  hide(selector = "#formEncuestas")
  hide(selector = "#formDisMuestral")
  hide(selector = "#formIntVoto")
  hide(selector = "#formCuestionario")
}   
## To be copied in the UI
# mod_investigacionCompartido_ui("investigacionCompartido_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionCompartido_server, "investigacionCompartido_ui_1")
 
