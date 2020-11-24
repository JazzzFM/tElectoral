#' investigacionListadoDisMuestral UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_investigacionListadoDisMuestral_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      actionButton(ns("atras"), "Regresar a encuestas", class="btn-secondary"),
      actionButton(ns("crearMuestral"), "Nuevo diseño muestral", class="btn-primary pull-right"),
    ),
    h3("Listado de diseño muestral"),
    hr(),
    DT::DTOutput(ns("listadoDisMuestral"))
  )
}
    
#' investigacionListadoDisMuestral Server Function
#'
#' @noRd 
mod_investigacionListadoDisMuestral_server <- function(input, output, session, bd, usuario, parent_session, showForm, idFormGeneral){
  ns <- session$ns
  t <- tibble(Nombre = c(a = "Encuesta 1", b = "Encuesta 2", c= "Encuesta 3"),
              Fecha_inicio = c(a = today(), b = today(), c = today()),
              Fecha_fin = c(a = today(), b = today(), c = today()), 
              FM = c(a = 1, b = 2, c= 3), 
              FC = c(a = 1, b = 2, c= 3), 
              FV = c(a = 1, b = 2, c= 3),
              Editar =  c(a = 1, b = 2, c= 3),
              Eliminar = c(a = 1, b = 2, c= 3))
  
  output$listadoDisMuestral <- DT::renderDT({
    
    t$FM$a <- input_btns(ns("disMuestral"), 1, "Editar", icon = "pie-chart", status = "primary")
    t$FM$b <- input_btns(ns("disMuestral"), 2, "Editar", icon = "pie-chart", status = "primary")
    t$FM$c <- input_btns(ns("disMuestral"), 3, "Editar", icon = "pie-chart", status = "primary")
    
    t$FC$a <- input_btns(ns("cuestionario"), 1, "Editar", icon = "question-circle", status = "primary")
    t$FC$b <- input_btns(ns("cuestionario"), 2, "Editar", icon = "question-circle", status = "primary")
    t$FC$c <- input_btns(ns("cuestionario"), 3, "Editar", icon = "question-circle", status = "primary")
    
    t$FV$a <- input_btns(ns("intVoto"), 1, "Editar", icon = "hand-pointer-o", status = "primary")
    t$FV$b <- input_btns(ns("intVoto"), 2, "Editar", icon = "hand-pointer-o", status = "primary")
    t$FV$c <- input_btns(ns("intVoto"), 3, "Editar", icon = "hand-pointer-o", status = "primary")
    
  
    t$Editar$a <- input_btns(ns("editar"), 1, "Editar", icon = "edit", status = "primary")
    t$Editar$b <- input_btns(ns("editar"), 2, "Editar", icon = "edit", status = "primary")
    t$Editar$c <- input_btns(ns("editar"), 3, "Editar", icon = "edit", status = "primary")
    
    t$Eliminar$a <- input_btns(ns("eliminar"), 1, "Eliminar", icon = "trash-o", status = "primary")
    t$Eliminar$b <- input_btns(ns("eliminar"), 2, "Eliminar", icon = "trash-o", status = "primary")
    t$Eliminar$c <- input_btns(ns("eliminar"), 3, "Eliminarr", icon = "trash-o", status = "primary")
    
    DT::datatable(data = t)
  }, escape = F)
  observeEvent(input$atras,{
    print(idFormGeneral$val)
    showForm$val <- 1
  })
}
    
## To be copied in the UI
# mod_investigacionListadoDisMuestral_ui("investigacionListadoDisMuestral_ui_1")
    
## To be copied in the server
# callModule(mod_investigacionListadoDisMuestral_server, "investigacionListadoDisMuestral_ui_1")
 
