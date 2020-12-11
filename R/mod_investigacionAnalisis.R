#' investigacionAnalisis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import dplyr ggplot2 highcharter tidyr shinycssloaders tidytext ggwordcloud 

mod_investigacionAnalisis_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Letreros
    fluidRow(class = "analisisValueBoxes",
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
    h3("Resultados diseño muestral"),
    tags$hr(),
    fluidRow(
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(
               plotOutput(ns("levantamiento"))
               )),
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(
               plotOutput(ns("marcoMuestral"))
             )),
      column(width = 6,
             div(class="topBoxInfo bordered-white ft-sm",
                shinycssloaders::withSpinner( 
                  plotOutput(outputId = ns("aleatoria"))
                )
             )),
      column(width = 6,
             div(class="topBoxInfo bordered-white ft-sm",
                 shinycssloaders::withSpinner( 
                   plotOutput(outputId = ns("noAleatoria"))
                 )
             ))
    ),
    fluidRow(class="analisisValueBoxes",
             valueBoxOutput(ns("maxEntrevistas")),
             valueBoxOutput(ns("modaEntrevistas")),
             valueBoxOutput(ns("minEntrevistas"))
    ),
    fluidRow(
    column(width = 4,
           div(class="topBoxInfo bordered-white ft-sm",
               shinycssloaders::withSpinner(
                 plotOutput(outputId = ns("poliEtapa"))
               )
           )),
    column(width = 4,
           div(class="topBoxInfo bordered-white ft-sm",
               shinycssloaders::withSpinner(
                 plotOutput(outputId = ns("Estratificada"))
               )
           )),
    column(width = 4,
           div(class="topBoxInfo bordered-white ft-sm",
               shinycssloaders::withSpinner(
                 plotOutput(outputId = ns("Conglomerados"))
               )
           )),
    column(width = 12,
           div(class="topBoxInfo bordered-white ft-sm",
               shinycssloaders::withSpinner(
                 plotOutput(outputId = ns("unidadMuestral"))
               )
           ))
    ),
    fluidRow(class="analisisValueBoxes",
             valueBoxOutput(ns("maxMargenError")),
             valueBoxOutput(ns("modaMargenError")),
             valueBoxOutput(ns("minMargenError"))
    ),
    fluidRow(
      column(width = 4,
             div(class="topBoxInfo bordered-white ft-sm",
                 shinycssloaders::withSpinner(
                   plotOutput(outputId = ns("minNivelConfianza"))
                 )
             )),
      column(width = 4,
             div(class="topBoxInfo bordered-white ft-sm",
                 shinycssloaders::withSpinner(
                   plotOutput(outputId = ns("modaNivelConfianza"))
                 )
             )),
      column(width = 4,
             div(class="topBoxInfo bordered-white ft-sm",
                 shinycssloaders::withSpinner(
                   plotOutput(outputId = ns("maxNivelConfianza"))
                 )
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
      bd$encuestas %>% collect() %>% nrow() %>% 
      valueBox(subtitle = "Encuestas realizadas", color = "light-blue", width = 12)
  })
  output$caja2 <- renderValueBox({
    start <- datetime <- ymd_hms(now("GMT"))
    end <- ymd_hms("2021-06-06 5:21:00", tz = "GMT")
    d <- as.numeric(round(end - start)) 
    d %>% valueBox(subtitle = "Días para la elección", color = "light-blue", width = 12)
  })
  output$caja3 <- renderValueBox({
    Sys.setlocale(locale = "es_MX.utf8")
    #DB_MichEncuesta %>% select(fecha_final) %>% tail(1) 
    bd$listadoIntVoto %>%
    collect() %>%
    pull(fechaEncuesta) %>%
    tail(1) %>% 
    as.Date() %>% format("%d de %b, %Y") %>% 
    valueBox(subtitle = "Fecha de última encuesta",
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
    procesamientoPollofPolls() %>% hPollofPolls3()
    })
  # Probabilidad de triunfo
  output$intencion <- renderPlot({
    # Real Data
    #bd <- procesamiento_graph(DB_MichEncuesta)
    # Forms Data
    procesamientoBrras()%>% 
    iVotoBarras()
    })
  #Probabilidad de triunfo
  
  output$gPdt <- renderPlot({
    # cand <- procesamiento_graph(DB_MichEncuesta) %>%
    #         group_by(candidato, colores) %>% summarise() %>%
    #         mutate(prob = runif(1, min = 0, max = 26))  %>%
    #         arrange(desc(prob)) %>% ungroup() %>% 
    #         mutate(cand = candidato, rw = seq(1:9), prob = round(prob)) 
    
    # Función
    cand <- procesamientoBrras() %>% 
    mutate(prob = votacion) %>%
    tibble(rw = 1:5) 
    
    probGanarOld(cand, candidato = "MORENA", 5)
    })
  
  output$levantamiento <- renderPlot({
    # Función
    #gglevantamiento(bd$listadoDisMuestral)
    ggbarrasLevantamiento(bd$listadoDisMuestral %>% collect())
  })
  
  output$marcoMuestral <- renderPlot({
    # Función
    WordCldTV(bd$listadoDisMuestral %>% collect())
  })

  output$aleatoria <- renderPlot({
    ggAleatoria(bd$listadoDisMuestral %>%
                  collect())
  })
  
  output$noAleatoria <- renderPlot({
    ggNoaleatoria(bd$listadoDisMuestral %>% 
                    collect())
  })
  
  output$maxEntrevistas <- renderValueBox({
    valueBox(
      bd$listadoDisMuestral %>% 
      collect() %>%
      pull(numeroEntrevistas) %>%
      max(),
      "Máximo de encuestas",
      icon = icon("line-chart")
    )
  })
  
  output$modaEntrevistas <- renderValueBox({
    valueBox(
        bd$listadoDisMuestral%>% 
        collect() %>%
        pull(numeroEntrevistas) %>%
        getmoda(),
      "Moda de encuestas",
      icon = icon("bar-chart")
    )
  })
  
  output$minEntrevistas <- renderValueBox({
    valueBox(
        bd$listadoDisMuestral%>% 
        collect() %>%
        pull(numeroEntrevistas) %>%
        min(),
      "Mínimo de encuestas",
      icon = icon("sort-amount-desc")
    )
  })
  
  output$poliEtapa <- renderPlot({
    ggPoliEtapa(bd$listadoDisMuestral %>% collect())
  })
  
  output$Estratificada <- renderPlot({
    ggEstratificada(bd$listadoDisMuestral %>% collect())
  })
  
  output$Conglomerados <- renderPlot({
    ggConglomerados(bd$listadoDisMuestral %>% collect())
  })

  output$unidadMuestral <- renderPlot({
    ggUnidadMuestral(bd$listadoDisMuestral %>% collect())
  })

  output$maxMargenError <- renderValueBox({
    valueBox(
      paste0(paste0("±", 
               bd$listadoDisMuestral %>% 
               collect() %>%
               pull(margenError) %>%
               max()), "%"), 
      "Margen de error máximo",
      icon = icon("line-chart")
    )
  })
  
  output$modaMargenError <- renderValueBox({
    valueBox(
      paste0(paste0("±",
        bd$listadoDisMuestral %>% 
        collect() %>%
        pull(margenError) %>%
        getmoda()), "%"),
      "Margen de error frecuente",
      icon = icon("bar-chart")
    )
  })
  
  output$minMargenError <- renderValueBox({
    valueBox(
      paste0(paste0("±",
        bd$listadoDisMuestral %>% 
        collect() %>%
        pull(margenError) %>%
        min()), "%"),
      "Margen de error mínimo",
      icon = icon("sort-amount-desc")
    )
  })
  
  output$minNivelConfianza <- renderPlot({
    ggMinNivelConfianza(bd$listadoDisMuestral %>%
                  collect())
  })
  
  output$modaNivelConfianza <- renderPlot({
    ggModaNivelConfianza(bd$listadoDisMuestral %>%
                  collect())
  })
  
  output$maxNivelConfianza <- renderPlot({
    ggMaxNivelConfianza(bd$listadoDisMuestral %>%
                  collect())
  })
  
}
  ## To be copied in the UI
# 

## To be copied in the server
# callModule(mod_investigacionAnalisis_server, "investigacionAnalisis_ui_1")

