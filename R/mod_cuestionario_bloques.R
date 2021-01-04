#' cuestionario_bloques UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cuestionario_bloques_ui <- function(id, bloque, index){
  ns <- NS(id)
  tagList(
    fluidRow(class="two-columns",
      p(class="text-center",bloque),
      HTML(input_btns(inputId = ns("editar"), label = "", icon = "pencil", users = index, status = "primary"))
    )
  )
}
    
#' cuestionario_bloques Server Function
#'
#' @noRd 

mod_cuestionario_bloques_server <- function(input, output, session, bloque = NULL, parent_session = NULL, cuestionario, index){
  ns <- session$ns
  preguntas <- reactiveValues()
  observeEvent(input$editar,{
    showModal(modalDialog(title = glue::glue("Preguntas del bloque {bloque}"),
                          mod_cuestionario_pregunta_ui(ns(glue::glue("cuestionario_pregunta_ui_{input$editar}"))),
                          footer = actionButton(ns("editarModal"),"Editar"),
                          easyClose = T
    )
    )
    callModule(mod_cuestionario_pregunta_server, glue::glue("cuestionario_pregunta_ui_{input$editar}"), valores = cuestionario$paso3[index], parent_session = parent_session)
  })
  observeEvent(input$editarModal,{
    removeModal()
    evt <- callModule(mod_cuestionario_pregunta_server, glue::glue("cuestionario_pregunta_ui_{index}"), valores = NULL, parent_session = parent_session)
    preguntas[[as.character(index)]] <- evt()
  })
  ev <- reactive({
    seq_len(index-1) %>% map(~preguntas[[as.character(.x)]] %>% mutate(bloque = bloque)) %>% do.call(rbind,.) %>% na.omit()
  })

  return(ev)
}
    
## To be copied in the UI
# mod_cuestionario_bloques_ui("cuestionario_bloques_ui_1")
    
## To be copied in the server
# callModule(mod_cuestionario_bloques_server, "cuestionario_bloques_ui_1")
 
