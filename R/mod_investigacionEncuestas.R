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
      h3("Encuestas realizadas y por completar"),
      tags$hr(),
      p("Se muestran las encuestas realizadas hasta el momento y por completar los formularios restantes."),
      h4("Encuestas"),
      DT::DTOutput(ns("encuestas"))
    )
}
    
#' investigacionEncuestas Server Function
#'
#' @noRd 
mod_investigacionEncuestas_server <- function(input, output, session, parent_session = NULL, showForm = NULL){
  ns <- session$ns

  t <- tibble(Nombre = c(a = "Encuesta 1", b = "Encuesta 2", c= "Encuesta 3"),
              Fecha_inicio = c(a = today(), b = today(), c = today()),
              Fecha_fin = c(a = today(), b = today(), c = today()), 
              FM = c(a = 1, b = 2, c= 3), 
              FC = c(a = 1, b = 2, c= 3), 
              FV = c(a = 1, b = 2, c= 3),
              Editar =  c(a = 1, b = 2, c= 3),
              Eliminar = c(a = 1, b = 2, c= 3))
  
  output$encuestas <- DT::renderDT({
    
    t$FM$a <- HTML(input_btns(ns("disMuestral"), 1, "Editar", icon = "pie-chart", status = "primary"))
    t$FM$b <- HTML(input_btns(ns("disMuestral"), 2, "Editar", icon = "pie-chart", status = "primary"))
    t$FM$c <- HTML(input_btns(ns("disMuestral"), 3, "Editar", icon = "pie-chart", status = "primary"))
    
    t$FC$a <- HTML(input_btns(ns("cuestionario"), 1, "Editar", icon = "question-circle", status = "primary"))
    t$FC$b <- HTML(input_btns(ns("cuestionario"), 2, "Editar", icon = "question-circle", status = "primary"))
    t$FC$c <- HTML(input_btns(ns("cuestionario"), 3, "Editar", icon = "question-circle", status = "primary"))
    
    t$FV$a <- HTML(input_btns(ns("intVoto"), 1, "Editar", icon = "hand-pointer-o", status = "primary"))
    t$FV$b <- HTML(input_btns(ns("intVoto"), 2, "Editar", icon = "hand-pointer-o", status = "primary"))
    t$FV$c <- HTML(input_btns(ns("intVoto"), 3, "Editar", icon = "hand-pointer-o", status = "primary"))
    
    
    t$Editar$a <- HTML(input_btns(ns("editar"), 1, "Editar", icon = "edit", status = "primary"))
    t$Editar$b <- HTML(input_btns(ns("editar"), 2, "Editar", icon = "edit", status = "primary"))
    t$Editar$c <- HTML(input_btns(ns("editar"), 3, "Editar", icon = "edit", status = "primary"))
    
    t$Eliminar$a <- HTML(input_btns(ns("eliminar"), 1, "Eliminar", icon = "trash-o", status = "primary"))
    t$Eliminar$b <- HTML(input_btns(ns("eliminar"), 2, "Eliminar", icon = "trash-o", status = "primary"))
    t$Eliminar$c <- HTML(input_btns(ns("eliminar"), 3, "Eliminarr", icon = "trash-o", status = "primary"))
    
    DT::datatable(data = t)
    }, escape = F)
  
  observeEvent(input$intVoto, {
    print("hola")
    showForm$val <- 3
  })
  
}

    
## To be copied in the UI
# mod_investigacionEncuestas_ui("investigacionEncuestas_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionEncuestas_server, "investigacionEncuestas_ui_1")
 
