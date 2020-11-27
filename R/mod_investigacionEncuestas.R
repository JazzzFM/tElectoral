#' investigacionEncuestas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_investigacionEncuestas_ui <- function(id){
  ns <- NS(id)
  tagList(
      h3("Registro de encuestas"),
      p("Se muestra el listado de encuestas registradas hasta el momento."),
      tags$hr(),
      DT::DTOutput(ns("encuestas"))
    )
}
    
#' investigacionEncuestas Server Function
#'
#' @noRd 
mod_investigacionEncuestas_server <- function(input, output, session, bd, parent_session = NULL, showForm = NULL, idFormGeneral){
  ns <- session$ns
  
  seleccion <- reactive(
    bd$encuestas %>% filter(activo == 1)
  )
  
  output$encuestas <- DT::renderDT({
    seleccion() %>% select(idFormGeneral,nombre,casaEncuestadora) %>%
      mutate(FD = input_btns(ns("disMuestral"), idFormGeneral, "Diseño muestral", icon = "pie-chart", status = "primary"),
             FV = input_btns(ns("intVoto"), idFormGeneral, "Intención de voto", icon = "hand-pointer-o", status = "primary"),
             FC = input_btns(ns("cuestionario"), idFormGeneral, "Cuestionario", icon = "question-circle", status = "primary"),
             Editar = input_btns(ns("editar"), idFormGeneral, "Ver", icon = "edit", status = "info"),
             Eliminar = input_btns(ns("eliminar"), idFormGeneral, "Eliminar", icon = "trash-o", status = "danger"),
      ) %>% select(-idFormGeneral)
  }, selection = 'none',rownames = FALSE,
  options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json',searchPlaceholder = "Buscar..."),
                 lengthMenu = c(5, 10, 25, 50, 100), pageLength = 5
  ),
  escape = F)
  
  observeEvent(input$disMuestral, {
    showForm$val <- 2
    idFormGeneral$val <- input$disMuestral
    gargoyle::trigger("disMuestral")
  })
  observeEvent(input$intVoto, {
    showForm$val <- 3
    idFormGeneral$val <- input$intVoto
    gargoyle::trigger("intencionVoto")
  })
  observeEvent(input$cuestionario, {
    showForm$val <- 4
    idFormGeneral$val <- input$cuestionario
  })
  
  
  
}

    
## To be copied in the UI
# mod_investigacionEncuestas_ui("investigacionEncuestas_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionEncuestas_server, "investigacionEncuestas_ui_1")
 
