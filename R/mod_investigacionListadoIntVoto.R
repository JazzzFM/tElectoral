#' investigacionListadoIntVoto UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_investigacionListadoIntVoto_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      actionButton(ns("atras"), "Regresar a encuestas", class="btn-secondary"),
      actionButton(ns("crearIntVoto"), "Nueva intención de voto", class="btn-primary pull-right")  
    ),
    h3("Listado de intención de voto"),
    hr(),
    DT::DTOutput(ns("listadoIntVoto"))
  )
}
    
#' investigacionListadoIntVoto Server Function
#'
#' @noRd 
mod_investigacionListadoIntVoto_server <- function(input, output, session, bd, usuario, parent_session, showForm, idFormGeneral){
  ns <- session$ns
 
  seleccion <- reactive(
    bd$listadoIntVoto %>% filter(activo == 1 & idFormGeneral == !! idFormGeneral$val)
  )
  
  output$listadoIntVoto <- DT::renderDT({
    seleccion() %>% select(idIntencionVoto,tipoIntencionVoto,pregunta, fechaAlta) %>%
      mutate(Ver = input_btns(ns("ver"), idIntencionVoto, "Ver", icon = "eye", status = "info"),
             Eliminar = input_btns(ns("eliminar"), idIntencionVoto, "Eliminar", icon = "trash-o", status = "danger"),
      ) %>% select(-idIntencionVoto)
  }, selection = 'none',rownames = FALSE,
  options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json',searchPlaceholder = "Buscar..."),
                 lengthMenu = c(5, 10, 25, 50, 100), pageLength = 5
  ),
  escape = F)
  observeEvent(input$ver,{
    print(input$ver)
    print(idFormGeneral$val)
  })
  observeEvent(input$atras,{
    showForm$val <- 1
  })
}
    
## To be copied in the UI
# mod_investigacionListadoIntVoto_ui("investigacionListadoIntVoto_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionListadoIntVoto_server, "investigacionListadoIntVoto_ui_1")
 
