#' investigacionAnalisis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import dplyr ggplot2 highcharter tidyr shinycssloaders

mod_investigacionAnalisis_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Letreros
    fluidRow(class="analisisValueBoxes",
      valueBoxOutput(ns("caja1")),
      valueBoxOutput(ns("caja2")),
      valueBoxOutput(ns("caja3"))
     ),
    # Gráficos
    fluidRow(
      column(width = 12, class="shadowBox",
             shinycssloaders::withSpinner(highchartOutput(ns("intervalos"))
                                          ))
    ),
    fluidRow(
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(
               plotOutput(ns("intencion"))
               )),
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(
             plotOutput(ns("gPdt"))
             ))
    ),
    h3("Resultados Diseño Muestral"),
    tags$hr(),
    fluidRow(
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(
               plotOutput(ns("levantamiento"))
               ))
    )
  )
}

#' investigacionAnalisis Server Function
#'
#' @noRd
mod_investigacionAnalisis_server <- function(input, output, session, bd){
  ns <- session$ns
  #Letreros
  output$caja1 <- renderValueBox({
    #DB_MichEncuesta %>% nrow() %>% 
    leerBd(pool, formGeneralBd) %>% collect() %>% nrow() %>% 
      valueBox(subtitle = "Encuestas Realizadas", color = "light-blue", width = 12)
  })
  output$caja2 <- renderValueBox({
    start <- datetime <- ymd_hms(now("GMT"))
    end <- ymd_hms("2021-06-06 5:21:00", tz = "GMT")
    d <- as.numeric(round(end - start)) 
    d %>% valueBox(subtitle = "Días para la Elección", color = "light-blue", width = 12)
  })
  output$caja3 <- renderValueBox({
    Sys.setlocale(locale = "es_MX.utf8")
    #DB_MichEncuesta %>% select(fecha_final) %>% tail(1) 
    leerBd(pool, formIntVotoBd) %>%
    collect() %>%
    pull(fechaEncuesta) %>%
    tail(1) %>% 
    as.Date() %>% format("%d de %b, %Y") %>% 
    valueBox(subtitle = "Fecha de Última Encuesta",
               icon = icon("calendar-o"), color = "light-blue")
  })
  # Probabilidad de triunfo
  # Pendiente a donde quedar
  output$caja4 <- renderPlot({
    BB <- tibble(x = rnorm(n = 30, sd = .06, mean = .3), y = rnorm(n = 30, sd = .06, mean = .10))
    cajaResume(BB, 4)
  })
  # Prueba
  output$intervalos <- renderHighchart({
    # real data
     # bd <- procesamiento_graph(DB_MichEncuesta) %>%
     #   filter(!candidato %in% c("PVEM", "PES", "PT", "MC", "INDEPENDIENTE"))
    
    DBI::dbGetQuery(pool,"SELECT fechaEncuesta, partido, resultado
                          FROM tElectoralTest_investigacion_intencionVoto as i
                          JOIN tElectoralTest_investigacion_intencionVotoRegistro as r
                          ON i.idIntencionVoto = r.idIntencionVoto;") %>%
    procesamientoFormularios() %>%
    select(fecha, candidato, voto, Tot_voto, prom_r_voto,
           votacion, sigma, var, min, max, colores) %>%
          filter(!votacion %in% c(100.000000)) %>% 
    hPollofPolls3()
  })
  # Probabilidad de triunfo
  output$intencion <- renderPlot({
    # Real Data
    #bd <- procesamiento_graph(DB_MichEncuesta)
    # Forms Data
    leerBd(pool, formIntVotoRegistroBd) %>%
    collect() %>% 
    select(partido, resultado) %>% 
    mutate(candidato = partido,
           votacion = as.double(resultado)/100) %>%
    select(candidato, votacion) %>% 
    iVotoBarras()
    
    })
  #Probabilidad de triunfo
  
  output$gPdt <- renderPlot({
    cand <- procesamiento_graph(DB_MichEncuesta) %>%
            group_by(candidato, colores) %>% summarise() %>%
            mutate(prob = runif(1, min = 0, max = 26))  %>%
            arrange(desc(prob)) %>% ungroup() %>% 
            mutate(cand = candidato, rw = seq(1:9), prob = round(prob)) 
    # Función
    probGanarOld(cand, candidato = "MORENA", 5)
    })
  
  output$levantamiento <- renderPlot({
    # Función
    gglevantamiento(bd$listadoDisMuestral)
  })
  
}
## To be copied in the UI
# 

## To be copied in the server
# callModule(mod_investigacionAnalisis_server, "investigacionAnalisis_ui_1")

