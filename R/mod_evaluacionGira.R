#' evaluacionGira UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_evaluacionGira_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Evaluación de gira"),
    p("Seleccione una gira y llene los siguientes campos para completar la evaluación"),
    hr(),
    selectizeInput(inputId = ns("gira"), choices = c("Seleccione ..." = ""), label = "Gira a evaluar"),
    h4("Eventos"),
    DT::DTOutput(ns("eventos"))
  )
}

#' evaluacionGira Server Function
#'
#' @noRd 
mod_evaluacionGira_server <- function(input, output, session, parent_session = NULL, bd, res_auth){
  ns <- session$ns
  observeEvent(bd$giras,{
    aux <- bd$giras %>% filter(activo == 1) %>%
      mutate(nombre = glue::glue("{LugarInicio} {FechaInicio} - {LugarFinal} {FechaFinal}"))
    updateSelectizeInput(session,inputId = "gira", choices = aux$idGira %>% set_names(aux$nombre))
  })
  
  seleccion <- reactive(
    bd$eventos %>% filter(activo == 1, idGira == !! input$gira)
  )
  
  output$eventos <- DT::renderDT({
    seleccion() %>% select(idEvento,nombre, lugar) %>%
      mutate(Evaluar = input_btns(ns("evaluar"), idEvento, "Evaluar", icon = "edit", status = "primary"),
             Ver  = input_btns(ns("ver"), idEvento, "Ver", icon = "eye", status = "info")
      ) %>% select(-idEvento)
  }, selection = 'none',rownames = FALSE,
  options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json',searchPlaceholder = "Buscar..."),
                 lengthMenu = c(5, 10, 25, 50, 100), pageLength = 5
  ),
  escape = F)
  
  
  observeEvent(input$evaluar, {
    showModal(modalDialog(title = "Evaluación de evento", #Se planea concatenar el título del evento
                          mod_evaluacionGiraPreguntas_ui(glue::glue(ns("evaluacionGiraPreguntas_ui_{input$evaluar}"))),
                          footer = actionButton("Concluir evaluación", class = "btn-primary", inputId = ns("concluirEvaluacion")),
                          easyClose = T
    ))
  })
  
  observeEvent(input$concluirEvaluacion, {
    removeModal()
    fA <- lubridate::now(tz = "America/Mexico_City") %>% as.character
    callModule(mod_evaluacionGiraPreguntas_server, glue::glue("evaluacionGiraPreguntas_ui_{input$evaluar}"), valores = NULL, parent_session = parent_session)() %>% 
      mutate(fechaAlta = fA,
             usuarioCrea = res_auth$user,
             activo = 1,
             idEvento = input$evaluar) %>% 
      insertBd(pool, evaluacionEventosBd, bd = .)
    
  })
  
  observeEvent(input$ver, {
    showModal(modalDialog(title = "Evaluación de evento", #Se planea concatenar el título del evento
                          mod_evaluacionGiraPreguntas_ui(glue::glue(ns("evaluacionGiraPreguntas_ui_{input$ver}")),
                                                         valores = tbl(pool,evaluacionEventosBd) %>% 
                                                           filter(idEvento == !! input$ver) %>% collect()),
                          # footer =  modalButton("Cerrar"),
                          easyClose = T
    ))
    # callModule(mod_evaluacionGiraPreguntas_server, glue::glue("evaluacionGiraPreguntas_ui_{input$ver}"), valores = evaluaciones[[as.character(input$ver)]](), parent_session = parent_session, readonly = 1)
  })
}

## To be copied in the UI
# mod_evaluacionGira_ui("evaluacionGira_ui_1")

## To be copied in the server
# callModule(mod_evaluacionGira_server, "evaluacionGira_ui_1")

