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
             textInput(inputId = "NombreCuestionario", placeholder = "Ingrese un nombre", label = "Nombre del cuestionario")
      ),
    ),
    #Objetivo de investigación
    h3("Objetivo de investigación"),
    tags$hr(),
    fluidRow(
      class="RowInvestigacion",
      column(
        width = 6,
        prettyRadioButtons(label = "Nivel de generalidad y especifidad", selected = 0, choices = c("Presente", "No presente"), inputId = ns("NvlGeneralidadEspecifidad"))
      ),
      column(
        width = 6,
        prettyRadioButtons(selected = 0, label = "Tipo de investigación", choices = c("Descriptiva", "Explicativa", "Ambas"), inputId = ns("TipoInvestigacion"))
      ),
      column(
        width = 6,
        prettyRadioButtons(selected = 0, label = "Uso de verbos en infinitivo", choices = c("Presente", "No presente"), inputId = ns("UsoVerbosInf"))
      ),
      column(
        width = 6,
        pickerInput(label = "Nivel de claridad", choices = c("Seleccione" = '',"No hay","Bajo","Intermedio","Alto"), inputId = ns("NvlClaridad")),
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
        prettyRadioButtons(selected = 0, label = "Población objetivo", choices = c("Presente", "No presente"), inputId = "PoblacionObjetivo")
      ),
      column(
        width = 6,
        prettyRadioButtons(selected = 0, label = "Filtro edad", choices = c("Presente", "No presente"), inputId = "FiltroEdad")
      ),
      column(
        width = 6,
        prettyRadioButtons(selected = 0, label = "Filtro lugar de residencia", choices = c("Presente", "No presente"), inputId = "FiltroLugarResidencia")
      ),
      column(
        width = 6,
        prettyRadioButtons(selected = 0, label = "Ubicación con GPS", choices = c("Presente", "No presente"), inputId = "GPS")
      ),
      column(
        width = 6,
        prettyRadioButtons(selected = 0, label = "Filtro de sección", choices = c("Presente", "No presente"), inputId = "FiltroSección")
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
        pickerInput(label = "Orden de los bloques", choices = c("Seleccione un orden" = '', "Apropiado","Poco apropiado","Poco desapropiado","Desapropiado"), inputId = "OrdenBloques"),
      ),
      column(
        width = 6,
        prettyRadioButtons(selected = 0, label = "Claridad en la operacionalización", choices = c("No hay", "Ligera", "Mucha"), inputId = "Operacionalizacion")
      ),
      column(
        width = 6,
        prettyRadioButtons(selected = 0, label = "Relación entre los conceptos y los objetivos", choices = c("No hay", "Ligera", "Mucha"), inputId = "ConceptosObjetivos")
      )
    ),
    actionButton(inputId = ns("GuardarPaso1"), "Guardar", class = "btn-primary"),
    # End bloques del cuestionario
  )
}
    
#' cuestionario_paso_1 Server Function
#'
#' @noRd 
mod_cuestionario_paso_1_server <- function(input, output, session, tituloBloques = c()){
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
    check <- c("UsoVerbosInf", "NvlClaridad") %>% mandatory(input = input)
    if(!check){
      shinyalert::shinyalert("Incompleto", "Favor de llenar todas las entradas.")
    }else{
      temp <- c("Seleccione...")
      value <- input$CantidadBloques
      for (i in 1:value) {
        temp[sum(i,1)] <- input[[(paste0("NombreBloque-", i))]]
      }
      tituloBloques$titulos <- temp
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
 
