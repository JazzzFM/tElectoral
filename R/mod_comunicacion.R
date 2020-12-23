#' comunicacion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyWidgets magrittr
mod_comunicacion_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
             actionButton(inputId = ns("atras"), label = "Regresar al listado de cuestionarios", class ="btn-default")
      )
    ),
    tabsetPanel(id = "TabsCuestionario",
    tabPanel(title = "Paso 1", value = "paso1",
             mod_cuestionario_paso_1_ui(ns("cuestionario_paso_1_ui_1"))
    ),
    tabPanel(title = "Paso 2", value = "paso2",
             mod_cuestionario_paso_2_ui(ns("cuestionario_paso_2_ui_1")),
       ),
    tabPanel(title = "Paso 3", value ="paso3",
             mod_cuestionario_paso_3_ui(ns("cuestionario_paso_3_ui_1"))
           ),
    tabPanel(title = "Paso 4", value = "paso4",
             mod_cuestionario_paso_4_ui(ns("cuestionario_paso_4_ui_1"))
          ),
    tabPanel(title = "Paso 5", value = "paso5",
             mod_cuestionario_paso_5_ui(ns("cuestionario_paso_5_ui_1"))
    )
    )
  )
}

#' comunicacion Server Function
#'
#' @noRd 
mod_comunicacion_server <- function(input, output, session, bd, usuario ,parent_session = NULL, showListadoForm = NULL, idFormGeneral = NULL, readOnly = NULL, idCuestionario = NULL){
  ns <- session$ns
  
  cuestionario <- reactiveValues(titulos = c(), paso1 = NULL, paso3 = c())
  deleteFile <- reactiveValues(val = F)
  observeEvent(input$atras,{
    deleteFile$val <- T
    showListadoForm$val <- 1
  })
  
  hideTab(inputId = "TabsCuestionario", target = "paso2", session = parent_session)
  hideTab(inputId = "TabsCuestionario", target = "paso3", session = parent_session)
  hideTab(inputId = "TabsCuestionario", target = "paso4", session = parent_session)
  hideTab(inputId = "TabsCuestionario", target = "paso5", session = parent_session)
  
  infoCuestionario <- reactiveVal(NULL)
  infoPreguntas <- reactiveVal(NULL)
  observe({
    
    if(as.numeric(idCuestionario$val) != 0){
      
      # Se obtiene info del cuestionario y sus preguntas
      infoCuestionario(bd$listadoCuestionario %>% filter(activo == 1 & idFormGeneral == !! idFormGeneral$val & idCuestionario == !! idCuestionario$val) %>% collect())
      infoPreguntas(bd$cuestionarioPreguntasXBloque %>% filter(idCuestionario == !! idCuestionario$val) %>% collect())
      
      # Se añaden títulos al reactiveValues
      for(x in 1:infoCuestionario()$cantidadBloques){
        item <- infoPreguntas()[x,]
        cuestionario$titulos[x] = item$nombre
      }
      cuestionario$paso1 <- infoCuestionario()
      cuestionario$paso3 <- infoPreguntas()
      
    }
    
    if(!is.null(cuestionario$paso1)){
      updateTabsetPanel(inputId = "tabsCuestionario", selected = "paso2", parent_session)
    }
    if(!is.null(cuestionario$paso1)){
      if(cuestionario$paso1$cantidadBloques != 0)
      updateTabsetPanel(inputId = "tabsCuestionario", selected = "paso3", parent_session)
    }
    if(!is.null(cuestionario$paso3)){
      updateTabsetPanel(inputId = "tabsCuestionario", selected = "paso4", parent_session)
    }
    if(!is.null(cuestionario$paso1)){
      if(cuestionario$paso1$obsGenerales != "")
        updateTabsetPanel(inputId = "tabsCuestionario", selected = "paso5", parent_session)
    }
  })
  callModule(mod_cuestionario_paso_1_server, "cuestionario_paso_1_ui_1", cuestionario, bd, usuario, parent_session, showListadoForm, idFormGeneral, readOnly, idCuestionario, deleteFile)
  callModule(mod_cuestionario_paso_2_server, "cuestionario_paso_2_ui_1", cuestionario, bd, usuario, parent_session, showListadoForm, idFormGeneral, readOnly, idCuestionario)
  callModule(mod_cuestionario_paso_3_server, "cuestionario_paso_3_ui_1", cuestionario, bd, usuario, parent_session, showListadoForm, idFormGeneral, readOnly, idCuestionario)
  callModule(mod_cuestionario_paso_4_server, "cuestionario_paso_4_ui_1", cuestionario, bd, usuario, showListadoForm, idFormGeneral, readOnly, idCuestionario)
  callModule(mod_cuestionario_paso_5_server, "cuestionario_paso_5_ui_1", cuestionario, bd, usuario, parent_session, showListadoForm, idFormGeneral, readOnly, idCuestionario)
 
}

## To be copied in the UI
# 

## To be copied in the server
# 

