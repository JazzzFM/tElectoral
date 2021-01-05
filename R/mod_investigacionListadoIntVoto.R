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
    uiOutput(ns("listadoForm"))
  )
}
    
#' investigacionListadoIntVoto Server Function
#'
#' @noRd 
mod_investigacionListadoIntVoto_server <- function(input, output, session, bd, usuario, parent_session, showForm, idFormGeneral){
  ns <- session$ns
  showListadoForm <- reactiveValues(val = 1)
  readOnly <- reactiveValues(val = FALSE)
  idIntencionVoto <- reactiveValues(val = 0)
  
  output$listadoForm <- renderUI({
    if(showListadoForm$val == 1){
      tagList(
        fluidRow(
          column(width = 6,
                 actionButton(ns("atras"), "Regresar a encuestas", class="btn-secondary"),
          ),
          column(width = 6,
                 actionButton(ns("crearIntVoto"), "Nueva intención de voto", class="btn-primary pull-right")  
          )
        ),
        h3("Listado de intención de voto"),
        hr(),
        DT::DTOutput(ns("listadoIntVoto"))
      )
    }
    else if(showListadoForm$val == 2){
      mod_investigacionFormularioIntVoto_ui(ns("investigacionFormularioIntVoto_ui_1"))
    }
  })
  
  callModule(mod_investigacionFormularioIntVoto_server, "investigacionFormularioIntVoto_ui_1", bd, usuario, parent_session, showListadoForm, idFormGeneral, readOnly, idIntencionVoto)
 
  seleccion <- reactiveVal(NULL)
  observeEvent(c(gargoyle::watch("intencionVoto")), {
    seleccion(bd$listadoIntVoto %>% filter(activo == 1 & idFormGeneral == !! idFormGeneral$val) %>% collect()) 
  })
  output$listadoIntVoto <- DT::renderDT({
    validate(need(!is.null(seleccion()), message = "Cargando datos ..."))
    seleccion() %>% select(idIntencionVoto,tipoIntencionVoto,pregunta, fechaAlta) %>%
      mutate(FechaAlta= as_date(fechaAlta),
             Ver = input_btns(ns("ver"), idIntencionVoto, "Ver", icon = "eye", status = "info"),
             Eliminar = input_btns(ns("eliminar"), idIntencionVoto, "Eliminar", icon = "trash-o", status = "danger"),
      ) %>% select(-idIntencionVoto, -fechaAlta)
  }, selection = 'none',rownames = FALSE, extensions = 'Responsive',
  options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json',searchPlaceholder = "Buscar..."),
                 lengthMenu = c(5, 10, 25, 50, 100), pageLength = 5
  ),
  escape = F)
  observeEvent(input$ver,{
    showListadoForm$val <- 2
    readOnly$val <- T
    idIntencionVoto$val <- input$ver
  })
  observeEvent(input$atras,{
    showForm$val <- 1
  })
  observeEvent(input$crearIntVoto,{
    showListadoForm$val <- 2
    readOnly$val <- F
  })
  observeEvent(input$eliminar, {
    shinyalert::shinyalert(title = "Advertencia", 
                           text = glue::glue("¿Está seguro que desea eliminar esta encuesta? No podrá recuperarla."),
                           showCancelButton = T,showConfirmButton = T,cancelButtonText = "No",
                           confirmButtonText = "Sí", 
                           callbackR = function(x) if(x) {
                             c1 <- glue::glue("idIntencionVoto = {input$eliminar}")
                             disableBd(pool = pool, nombre = formIntVotoRegistroBd, condition = c1, showMessage = F)
                             disableBd(pool = pool, nombre = formIntVotoBd, condition = c1)
                             gargoyle::trigger("intencionVoto")
                           })
  })
}
    
## To be copied in the UI
# mod_investigacionListadoIntVoto_ui("investigacionListadoIntVoto_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionListadoIntVoto_server, "investigacionListadoIntVoto_ui_1")
 
