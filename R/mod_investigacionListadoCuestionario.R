#' investigacionListadoCuestionario UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_investigacionListadoCuestionario_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("listadoForm"))
  )
}
    
#' investigacionListadoCuestionario Server Function
#'
#' @noRd 
mod_investigacionListadoCuestionario_server <- function(input, output, session, bd, usuario, parent_session, showForm, idFormGeneral){
  ns <- session$ns
  
  showListadoForm <- reactiveValues(val = 1)
  readOnly <- reactiveValues(val = FALSE)
  idCuestionario <- reactiveValues(val = 0)
  
  output$listadoForm <- renderUI({
    if(showListadoForm$val == 1){
      tagList(
        fluidRow(
          column(width = 6,
                 actionButton(ns("atras"), "Regresar a encuestas", class="btn-secondary")
          ),
          column(width = 6,
                 actionButton(ns("crearCuestionario"), "Nuevo cuestionario", class="btn-primary pull-right")
          )
        ),
        h3("Listado de cuestionarios"),
        hr(),
        DT::DTOutput(ns("listadoCuestionario"))
      )
    }
    else if(showListadoForm$val == 2){
      mod_comunicacion_ui(ns("comunicacion_ui_1"))
    }
  })
  callModule(mod_comunicacion_server, "comunicacion_ui_1", bd, usuario, parent_session, showListadoForm, idFormGeneral, readOnly, idCuestionario)
  
  seleccion <- reactiveVal(NULL)
  observeEvent(c(gargoyle::watch("cuestionario")), {
    seleccion(bd$listadoCuestionario %>% filter(activo == 1 & idFormGeneral == !! idFormGeneral$val) %>% collect()) 
  })
  output$listadoCuestionario <- DT::renderDT({
    validate(need(!is.null(seleccion()), message = "Cargando datos ..."))
    seleccion() %>% select(idCuestionario,nivelClaridad,cantidadBloques, correo, obsGenerales, fechaAlta) %>%
      mutate(FechaAlta= as_date(fechaAlta),
             Ver = input_btns(ns("ver"), idCuestionario, "Ver", icon = "eye", status = "info"),
             Eliminar = input_btns(ns("eliminar"), idCuestionario, "Eliminar", icon = "trash-o", status = "danger"),
      ) %>% select(-idCuestionario, -fechaAlta)
  }, selection = 'none',rownames = FALSE,
  options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json',searchPlaceholder = "Buscar..."),
                 lengthMenu = c(5, 10, 25, 50, 100), pageLength = 5
  ),
  escape = F)
  
  observeEvent(input$ver,{
    showListadoForm$val <- 2
    readOnly$val <- T
    idCuestionario$val <- input$ver
  })
  observeEvent(input$atras,{
    showForm$val <- 1
  })
  observeEvent(input$crearCuestionario,{
    showListadoForm$val <- 2
    readOnly$val <- F
  })
  observeEvent(input$eliminar, {
    shinyalert::shinyalert(title = "Advertencia", 
                           text = glue::glue("¿Está seguro que desea eliminar esta encuesta? No podrá recuperarla."),
                           showCancelButton = T,showConfirmButton = T,cancelButtonText = "No",
                           confirmButtonText = "Sí", 
                           callbackR = function(x) if(x) {
                             c1 <- glue::glue("idCuestionario = {input$eliminar}")
                             disableBd(pool = pool, nombre = formCuestionarioBd, condition = c1)
                             gargoyle::trigger("cuestionario")
                           })
  })
}
    
## To be copied in the UI
# mod_investigacionListadoCuestionario_ui("investigacionListadoCuestionario_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionListadoCuestionario_server, "investigacionListadoCuestionario_ui_1")
 
