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
      h3("Encuestas realizadasy por completar"),
      tags$hr(),
      p("Se muestran las encuestas realizadas hasta el momento y por completar los formularios restantes."),
      h4("Encuestas"),
      #DT::dataTableOutput("encuestas")
      DT::DTOutput(ns("encuestas"))
    )
}
    
#' investigacionEncuestas Server Function
#'
#' @noRd 
mod_investigacionEncuestas_server <- function(input, output, session, parent_session = NULL){
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
    
    t$FM$a <- HTML(input_btns(ns("encuestas"), 1, "Editar", icon = "edit", status = "primary"))
    t$FM$b <- HTML(input_btns(ns("encuestas"), 2, "Editar", icon = "edit", status = "primary"))
    t$FM$c <- HTML(input_btns(ns("encuestas"), 3, "Editar", icon = "edit", status = "primary"))
    
    t$FC$a <- HTML(input_btns(ns("encuestas"), 1, "Editar", icon = "edit", status = "primary"))
    t$FC$b <- HTML(input_btns(ns("encuestas"), 2, "Editar", icon = "edit", status = "primary"))
    t$FC$c <- HTML(input_btns(ns("encuestas"), 3, "Editar", icon = "edit", status = "primary"))
    
    t$FV$a <- HTML(input_btns(ns("encuestas"), 1, "Editar", icon = "edit", status = "primary"))
    t$FV$b <- HTML(input_btns(ns("encuestas"), 2, "Editar", icon = "edit", status = "primary"))
    t$FV$c <- HTML(input_btns(ns("encuestas"), 3, "Editar", icon = "edit", status = "primary"))
    
    t$Editar$a <- HTML(input_btns(ns("encuestas"), 1, "Editar", icon = "pencil", status = "primary"))
    t$Editar$b <- HTML(input_btns(ns("encuestas"), 2, "Editar", icon = "pencil", status = "primary"))
    t$Editar$c <- HTML(input_btns(ns("encuestas"), 3, "Editar", icon = "pencil", status = "primary"))
    
    t$Eliminar$a <- HTML(input_btns(ns("encuestas"), 1, "Eliminar", icon = "trash-o", status = "primary"))
    t$Eliminar$b <- HTML(input_btns(ns("encuestas"), 2, "Eliminar", icon = "trash-o", status = "primary"))
    t$Eliminar$c <- HTML(input_btns(ns("encuestas"), 3, "Eliminarr", icon = "trash-o", status = "primary"))
    
    DT::datatable(data = t)
    }, escape = F)
  
 }
    
## To be copied in the UI
# mod_investigacionEncuestas_ui("investigacionEncuestas_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionEncuestas_server, "investigacionEncuestas_ui_1")
 
