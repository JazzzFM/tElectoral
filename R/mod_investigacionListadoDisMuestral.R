#' investigacionListadoDisMuestral UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_investigacionListadoDisMuestral_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("listadoForm"))
  )
}
    
#' investigacionListadoDisMuestral Server Function
#'
#' @noRd 
mod_investigacionListadoDisMuestral_server <- function(input, output, session, bd, usuario, parent_session, showForm, idFormGeneral){
  ns <- session$ns
  
  showListadoForm <- reactiveValues(val = 1)
  readOnly <- reactiveValues(val = FALSE)
  idDMuestral <- reactiveValues(val = 0)
  
  output$listadoForm <- renderUI({
    if(showListadoForm$val == 1){
      tagList(
        fluidRow(
          column(width = 6,actionButton(ns("atras"), "Regresar a encuestas", class="btn-secondary")),
          column(width = 6,actionButton(ns("crearMuestral"), "Nuevo diseño muestral", class="btn-primary pull-right"))
        ),
        h3("Listado de diseño muestral"),
        hr(),
        DT::DTOutput(ns("listadoDisMuestral"))
      )
    }
    else if(showListadoForm$val == 2){
      mod_investigacionFormularioDisMuestral_ui(ns("investigacionFormularioDisMuestral_ui_1"))
    }
  })
  callModule(mod_investigacionFormularioDisMuestral_server, "investigacionFormularioDisMuestral_ui_1", bd, usuario, parent_session, showListadoForm, idFormGeneral, readOnly, idDMuestral)
  
  
  seleccion <- reactiveVal(NULL)
  observeEvent(c(gargoyle::watch("disMuestral")), {
    seleccion(bd$listadoDisMuestral %>% filter(activo == 1 & idFormGeneral == !! idFormGeneral$val) %>% collect()) 
  })
  output$listadoDisMuestral <- DT::renderDT({
    validate(need(!is.null(seleccion()), message = "Cargando datos ..."))
    seleccion() %>% select(idDMuestral,modoLevantamiento,marcoMuestral, numeroEntrevistas, fechaAlta) %>%
      mutate(FechaAlta= as_date(fechaAlta),
             Ver = input_btns(ns("ver"), idDMuestral, "Ver", icon = "eye", status = "info"),
             Eliminar = input_btns(ns("eliminar"), idDMuestral, "Eliminar", icon = "trash-o", status = "danger"),
      ) %>% select(-idDMuestral, -fechaAlta)
  }, selection = 'none',rownames = FALSE,
  options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json',searchPlaceholder = "Buscar..."),
                 lengthMenu = c(5, 10, 25, 50, 100), pageLength = 5
  ),
  escape = F)
  
  observeEvent(input$ver,{
    showListadoForm$val <- 2
    readOnly$val <- T
    idDMuestral$val <- input$ver
  })
  observeEvent(input$atras,{
    showForm$val <- 1
  })
  observeEvent(input$crearMuestral,{
    showListadoForm$val <- 2
    readOnly$val <- F
  })
  
  observeEvent(input$eliminar, {
    shinyalert::shinyalert(title = "Advertencia", 
                           text = glue::glue("¿Está seguro que desea eliminar esta encuesta? No podrá recuperarla."),
                           showCancelButton = T,showConfirmButton = T,cancelButtonText = "No",
                           confirmButtonText = "Sí", 
                           callbackR = function(x) if(x) {
                             c1 <- glue::glue("idDMuestral = {input$eliminar}")
                             disableBd(pool = pool, nombre = formDisMuestralBd, condition = c1)
                             gargoyle::trigger("disMuestral")
                           })
  })
}
    
## To be copied in the UI
# mod_investigacionListadoDisMuestral_ui("investigacionListadoDisMuestral_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionListadoDisMuestral_server, "investigacionListadoDisMuestral_ui_1")
 
