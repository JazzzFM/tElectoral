#' cuestionario_paso_2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyWidgets magrittr
mod_cuestionario_paso_2_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Bloques del cuestionario"),
    p("Introduzca una cantidad de bloques, de click en el botón que sigue para generar los títulos y llenarlos."),
    tags$hr(),
    div(class="shadowForm",
        fluidRow(
          class = "NombreBloquesContainer",
          column(
            width = 6,
            class = "FlexColumn",
            numericInputIcon(inputId = ns("CantidadBloques"),
                             label = "Cantidad de bloques",
                             value = 0,
                             min = 0,
                             icon = list(NULL, 
                                         actionButton(inputId = ns("genBloques"), icon = icon("cog") ,"", class="btn btn-primary")
                             )
            )
          )
        ),
        hr(),
        actionButton(inputId = ns("guardarPaso2"), "Guardar paso 2", class="btn btn-primary")
    )
  )
}

#' cuestionario_paso_2 Server Function
#'
#' @noRd 
mod_cuestionario_paso_2_server <- function(input, output, session, cuestionario = NULL, parent_session){
  ns <- session$ns
   observeEvent(input$guardarPaso2,{
     # Guardar títulos
     temp <- c()
     value <- input$CantidadBloques
     for (i in 1:value) {
       temp[i] <- input[[(paste0("NombreBloque-", i))]]
     }
     cuestionario$titulos <- temp
     cuestionario$paso1$cantidadBloques <- input$CantidadBloques
     print(cuestionario$titulos)
     print(cuestionario$paso1)
     # End guardar títulos
   })
  #Generar bloques
  observeEvent(input$genBloques, {
    value <- input$CantidadBloques
    removeUI(
      selector = ".NombreBloque",
      multiple = T
    )
    if(value > 0){
      for (item in 1:value) {
        insertUI(
          selector = '.NombreBloquesContainer',
          where = "beforeEnd",
          ui = column(class="NombreBloque",
                      width = 6,
                      textInput(inputId = ns(paste0("NombreBloque-", item)), placeholder = "...", label = paste0("Título de bloque no. ", item))
          )
        )
      }
    }
  })
  
}

## To be copied in the UI
# 

## To be copied in the server
# 