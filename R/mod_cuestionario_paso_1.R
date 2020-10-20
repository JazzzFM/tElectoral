#' cuestionario_paso_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyWidgets magrittr
mod_cuestionario_paso_1_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, 
             textInput(inputId = ns("NombreCuestionario"), placeholder = "Ingrese un nombre", label = "Nombre del cuestionario")
      ),
    ),
    #Objetivo de investigación
    h3("Objetivo de investigación"),
    tags$hr(),
    fluidRow(
      class="RowInvestigacion",
      column(
        width = 6,
        prettyRadioButtons(label = "Nivel de generalidad y especifidad", choices = c("Presente", "No presente"), inputId = ns("NvlGeneralidadEspecifidad"), selected = 0)
      ),
      column(
        width = 6,
        prettyRadioButtons(label = "Tipo de investigación", choices = c("Descriptiva", "Explicativa", "Ambas"), inputId = ns("TipoInvestigacion"), selected = 0)
      ),
      column(
        width = 6,
        prettyRadioButtons(  label = "Uso de verbos en infinitivo", choices = c("Presente", "No presente"), inputId = ns("UsoVerbosInf"), selected = 0)
      ),
      column(
        width = 6,
        pickerInput(label = "Nivel de claridad", choices = c("Seleccione" = '',"No hay","Bajo","Intermedio","Alto"), inputId = ns("NvlClaridad"), selected = 0),
      ),
      column(
        width = 6,
        selectizeInput(inputId = ns("ConBasicos"), label = "Conceptos básicos", choices = c("Evaluación de autoridades", "Percepción de imagen", "Intención de voto", "Servicios públicos", "Políticas públicas", "Conocimiento de candidatos", "Simpatía electoral
", "Identidad partidista", "Otro"), multiple = TRUE, options = list(placeholder = "Seleccione conceptos"))
      )
    ),
    #End objetivo de investigación
    
    #Población objetivo
    h3("Población objetivo"),
    tags$hr(),
    fluidRow(
      column(
        width = 6,
        prettyRadioButtons(  label = "Población objetivo", choices = c("Presente", "No presente"), inputId = ns("PoblacionObjetivo"), selected = 0)
      ),
      column(
        width = 6,
        prettyRadioButtons(  label = "Filtro edad", choices = c("Presente", "No presente"), inputId = ns("FiltroEdad"), selected = 0)
      ),
      column(
        width = 6,
        prettyRadioButtons(  label = "Filtro lugar de residencia", choices = c("Presente", "No presente"), inputId = ns("FiltroLugarResidencia"), selected = 0)
      ),
      column(
        width = 6,
        prettyRadioButtons(  label = "Ubicación con GPS", choices = c("Presente", "No presente"), inputId = ns("GPS"), selected = 0)
      ),
      column(
        width = 6,
        prettyRadioButtons(  label = "Filtro de sección", choices = c("Presente", "No presente"), inputId = ns("FiltroSeccion"), selected = 0)
      )
    ),
    #End Población objetivo
    
    # Bloques del cuestionario / Conceptos básicos
    h3("Bloques del cuestionario / Conceptos básicos"),
    tags$hr(),
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
    tags$hr(),
    fluidRow(
      column(
        width = 6,
        pickerInput(label = "Orden de los bloques", choices = c("Seleccione un orden" = '', "Apropiado","Poco apropiado","Poco desapropiado","Desapropiado"), inputId = ns("OrdenBloques"), selected = 0),
      ),
      column(
        width = 6,
        prettyRadioButtons(  label = "Claridad en la operacionalización", choices = c("No hay", "Ligera", "Mucha"), inputId = ns("Operacionalizacion"), selected = 0)
      ),
      column(
        width = 6,
        prettyRadioButtons(  label = "Relación entre los conceptos y los objetivos", choices = c("No hay", "Ligera", "Mucha"), inputId = ns("ConceptosObjetivos"), selected = 0)
      )
    ),
    actionButton(inputId = ns("GuardarPaso1"), "Guardar", class = "btn-primary"),
    # End bloques del cuestionario
  )
}
    
#' cuestionario_paso_1 Server Function
#'
#' @noRd 
mod_cuestionario_paso_1_server <- function(input, output, session, cuestionario = c()){
  ns <- session$ns
  observeEvent(input$ConBasicos,{
    removeUI(
      selector = ".ColumnEspecifique"
    ) 
    for (value in input$ConBasicos) {
      if(value == "Otro"){
        insertUI(
          selector = '.RowInvestigacion', 
          where = "beforeEnd",
          ui = column(width = 6, 
                      class="ColumnEspecifique",
                      textInput(inputId = "Especifique", placeholder = "...", label = "Especifique"),
                      actionButton(inputId = ns("CancelarOtro"), "Cancelar", class = "btn-default"),
                      actionButton(inputId = "Guardar", "Guardar concepto", class = "btn-primary"),
          )
        )
        break;
      }
    }
  }, ignoreNULL = F)
  observeEvent(input$CancelarOtro, {
    removeUI(
      selector = ".ColumnEspecifique"
    )
    # x <- input$ConBasicos
    # remove <- c("Otro")
    # setdiff(x, remove)
    #updateSelectizeInput(session = session, inputId = ns("ConBasicos"), selected = NULL, server = T)
  })
  # Guardar cuestionario
  
  observeEvent(input$GuardarPaso1, {
    shinyjs::disable(input$GuardarPaso1)
    check <- c("NombreCuestionario", "NvlGeneralidadEspecifidad",  "TipoInvestigacion",  "UsoVerbosInf",  "NvlClaridad",  "ConBasicos",  "PoblacionObjetivo",  "FiltroEdad",  "FiltroLugarResidencia",  "GPS",  "FiltroSeccion",  "CantidadBloques",  "OrdenBloques",  "Operacionalizacion", "ConceptosObjetivos") %>% mandatory(input = input)
    if(!check){
      shinyalert::shinyalert("Incompleto", "Favor de llenar todas las entradas.")
    }else{
      
      # Guardar títulos
      temp <- c()
      value <- input$CantidadBloques
      for (i in 1:value) {
        temp[i] <- input[[(paste0("NombreBloque-", i))]]
      }
      cuestionario$titulos <- temp
      # End guardar títulos
      
      # Tibble
      
      cuestionario$paso1 <- tibble::tibble(
        Nombre = input$NombreCuestionario,
        NvlGeneralidadEspecifidad = input$NvlGeneralidadEspecifidad,
        TipoInvestigacion = input$TipoInvestigacion,
        UsoVerbosInf = input$UsoVerbosInf,
        NvlClaridad = input$NvlClaridad,
        ConBasicos = input$ConBasicos,
        PoblacionObjetivo = input$PoblacionObjetivo,
        FiltroEdad = input$FiltroEdad,
        FiltroLugarResidencia = input$FiltroLugarResidencia,
        GPS = input$GPS,
        FiltroSeccion = input$FiltroSeccion,
        CantidadBloques = input$CantidadBloques,
        OrdenBloques = input$OrdenBloques,
        Operacionalizacion = input$Operacionalizacion,
        ConceptosObjetivos = input$ConceptosObjetivos
      )
    }
    shinyjs::enable(input$GuardarPaso1)
  })
  
  # Generar bloques
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
 
