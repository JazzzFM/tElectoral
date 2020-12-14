#' giraPaso3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import shinyjs
mod_giraPaso3_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    h3("Asignación de eventos"),
    p("Asigne eventos a los lugares que ha escogido en el paso 2"),
    uiOutput(ns("outEventos")),
    actionButton(ns("guardar"), "Guardar", class = "btn-primary")
  )
}

#' giraPaso3 Server Function
#'
#' @noRd 

mod_giraPaso3_server <- function(input, output, session, gira = NULL, parent_session = NULL, reseted, usuario){
  ns <- session$ns
  
  listaEventos <- reactiveValues(eventos = c())
  horariosOcupados <- reactiveValues()
  output$outEventos <- renderUI({
    if(!is.null(gira$paso2)){
      lapply(seq_along(gira$paso2$lugares), function(i) {
        mod_lugaresPaso3_ui(ns(glue::glue("lugaresPaso3_ui_{i}")), titulo = gira$paso2$lugares[i])
      })
    }
  })
  
  observe({
    if(length(gira$paso2$lugares) > 0){
      listaEventos$eventos <- seq_along(gira$paso2$lugares) %>% map(~callModule(mod_lugaresPaso3_server,
                                                                                glue::glue("lugaresPaso3_ui_{.x}"),
                                                                                lugar = gira$paso2$lugares[.x],
                                                                                lugarAntes = ifelse(.x != 1, gira$paso2$lugares[sum(.x, -1)], ""),
                                                                                parent_session = parent_session,
                                                                                todosEventos = listaEventos$eventos,
                                                                                paso1 = gira$paso1,
                                                                                tiempoActual = ifelse(.x != length(gira$paso2$lugares), gira$paso2Tiempos[.x], 0),
                                                                                tiempoAntes = ifelse(.x != 1, gira$paso2Tiempos[sum(.x, -1)], 0),
                                                                                index = .x
      ))
    }
  })
  observe({
    if(reseted$value){
      # Aquí no se realiza un reseteo, pero se mantiene la estructura para validar reseteo
      reseted$resPaso3 <- T
    }
  })
  observeEvent(input$guardar, {
    if(is.null(seq_along(listaEventos$eventos) %>% detect(~is.null(listaEventos$eventos[[.x]]())))){
      if(is.null(seq_along(listaEventos$eventos) %>% detect(~nrow(listaEventos$eventos[[.x]]()) == 0))){
        fA <- lubridate::now(tz = "America/Mexico_City") %>% as.character()
        gira$paso1 %>% 
          mutate(FechaInicio = lubridate::ymd_hm(glue::glue("{FechaInicio} {HorarioInicio}"),tz = "America/Mexico_City"),
                 FechaFinal = lubridate::ymd_hm(glue::glue("{FechaFinal} {HorarioFinal}"),tz = "America/Mexico_City"),
                 fechaAlta = fA,
                 usuarioCrea = usuario$user,
                 activo = 1) %>%
          select(-HorarioInicio,-HorarioFinal) %>% 
          insertBd(pool, girasBd, bd = .)
        
        idgiraRec <- tbl(pool, girasBd) %>% filter(fechaAlta == !!fA) %>% pull(idGira)
        
        seq_along(listaEventos$eventos) %>% map(~listaEventos$eventos[[.x]]()) %>% do.call(rbind,.) %>% 
          mutate(fechaAlta = fA,
                 usuarioCrea = usuario$user,
                 idGira = idgiraRec,
                 activo = 1) %>% 
          insertBd(pool, eventosBd, bd = .)
        
        
      }else{
        shinyalert::shinyalert(title = "Debe añadir al menos un evento por lugar")
      }
    }else{
      shinyalert::shinyalert(title = "Debe añadir al menos un evento por lugar")
    }
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 

