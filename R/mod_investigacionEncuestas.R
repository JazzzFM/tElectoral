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
    uiOutput(ns("listadoForm"))
    )
}
    
#' investigacionEncuestas Server Function
#'
#' @noRd 
mod_investigacionEncuestas_server <- function(input, output, session, bd, usuario, parent_session = NULL, showForm = NULL, idFormGeneral){
  ns <- session$ns
  showListadoForm <- reactiveValues(val = 1)
  readOnly <- reactiveValues(val = FALSE)
  
  output$listadoForm <- renderUI({
    if(showListadoForm$val == 1){
      tagList(
        fluidRow(
          column(width = 6,
                 actionButton(ns("crearEncuesta"), "Nueva encuesta", class="btn-primary")  
          )
        ),
        h3("Registro de encuestas"),
        p("Se muestra el listado de encuestas registradas hasta el momento."),
        tags$hr(),
        DT::DTOutput(ns("encuestas"))
      )
    }
    else if(showListadoForm$val == 2){
      mod_investigacionFormularioGeneral_ui(ns("investigacionFormularioGeneral_ui_1"))
    }
  })
  callModule(mod_investigacionFormularioGeneral_server, "investigacionFormularioGeneral_ui_1", bd, usuario, parent_session, showListadoForm, idFormGeneral, readOnly)

  seleccion <- reactiveVal(NULL)
  observeEvent(c(gargoyle::watch("encuestasGeneral")), {
    seleccion(bd$encuestas %>% filter(activo == 1) %>% collect()) 
  })
  output$encuestas <- DT::renderDT({
    validate(need(!is.null(seleccion()), message = "Cargando datos ..."))
    seleccion() %>% select(idFormGeneral,nombre,casaEncuestadora) %>%
      mutate(FD = input_btns(ns("disMuestral"), idFormGeneral, "Diseño muestral", icon = "pie-chart", status = "primary"),
             FV = input_btns(ns("intVoto"), idFormGeneral, "Intención de voto", icon = "hand-pointer-o", status = "primary"),
             FC = input_btns(ns("cuestionario"), idFormGeneral, "Cuestionario", icon = "question-circle", status = "primary"),
             Editar = input_btns(ns("editar"), idFormGeneral, "Ver", icon = "edit", status = "info"),
             Eliminar = input_btns(ns("eliminar"), idFormGeneral, "Eliminar", icon = "trash-o", status = "danger"),
      ) %>% select(-idFormGeneral)
  }, selection = 'none',rownames = FALSE, extensions = 'Responsive',
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
    gargoyle::trigger("cuestionario")
  })
  
  #Editar
  observeEvent(input$editar,{
    showListadoForm$val <- 2
    readOnly$val <- T
    idFormGeneral$val <- input$editar
  })
  observeEvent(input$crearEncuesta,{
    showListadoForm$val <- 2
    readOnly$val <- F
  })
  observeEvent(input$eliminar, {
    shinyalert::shinyalert(title = "Advertencia", 
                           text = glue::glue("¿Está seguro que desea eliminar esta encuesta? No podrá recuperarla."),
                           showCancelButton = T,showConfirmButton = T,cancelButtonText = "No",
                           confirmButtonText = "Sí", 
                           callbackR = function(x) if(x) {
                             c1 <- glue::glue("idFormGeneral = {input$eliminar}")
                             disableBd(pool = pool, nombre = formGeneralBd, condition = c1)
                             disableBd(pool = pool, nombre = formDisMuestralBd, condition = c1)
                             disableBd(pool = pool, nombre = formIntVotoBd, condition = c1)
                             disableBd(pool = pool, nombre = formIntVotoRegistroBd, condition = c1)
                             disableBd(pool = pool, nombre = formCuestionarioBd, condition = c1)
                             gargoyle::trigger("encuestasGeneral")
                           })
  })
  
}

    
## To be copied in the UI
# mod_investigacionEncuestas_ui("investigacionEncuestas_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionEncuestas_server, "investigacionEncuestas_ui_1")
 
