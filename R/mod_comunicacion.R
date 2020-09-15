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
          prettyRadioButtons(label = "Nivel de generalidad y especifidad", choices = c("Presente", "No presente"), inputId = "NvlGeneralidadEspecifidad")
        ),
        column(
          width = 6,
          prettyRadioButtons(label = "Tipo de investigación", choices = c("Descriptiva", "Explicativa", "Ambas"), inputId = "TipoInvestigacion")
        ),
        column(
          width = 6,
          prettyRadioButtons(label = "Uso de verbos en infinitivo", choices = c("Presente", "No presente"), inputId = "UsoVerbosInf")
        ),
        column(
          width = 6,
          pickerInput(label = "Nivel de claridad", choices = c("No hay","Bajo","Intermedio","Alto"), inputId = "NvlClaridad"),
        ),
        column(
          width = 6,
          selectizeInput(inputId = ns("ConBasicos"), label = "Conceptos básicos", choices = c("Seleccione conceptos" = '',"Evaluación de autoridades", "Percepción de imagen", "Intención de voto", "Servicios públicos", "Políticas públicas", "Conocimiento de candidatos", "Simpatía electoral
", "Identidad partidista", "Otro"), selected = NULL, multiple = TRUE)
        )
      ),
    #End objetivo de investigación
    
    #Población objetivo
    h3("Población objetivo"),
    tags$hr(),
    fluidRow(
      column(
        width = 6,
        prettyRadioButtons(label = "Población objetivo", choices = c("Presente", "No presente"), inputId = "PoblacionObjetivo")
      ),
      column(
        width = 6,
        prettyRadioButtons(label = "Filtro edad", choices = c("Presente", "No presente"), inputId = "FiltroEdad")
      ),
      column(
        width = 6,
        prettyRadioButtons(label = "Filtro lugar de residencia", choices = c("Presente", "No presente"), inputId = "FiltroLugarResidencia")
      ),
      column(
        width = 6,
        prettyRadioButtons(label = "Ubicación con GPS", choices = c("Presente", "No presente"), inputId = "GPS")
      ),
      column(
        width = 6,
        prettyRadioButtons(label = "Filtro de sección", choices = c("Presente", "No presente"), inputId = "FiltroSección")
      )
    ),
    #End Población objetivo
    
    # Bloques del cuestionario / Conceptos básicos
    h3("Bloques del cuestionario / Conceptos básicos"),
    tags$hr(),
    fluidRow(
      
      column(
        width = 6,
        pickerInput(label = "Orden de los bloques", choices = c("Seleccione un orden" = '', "Apropiado","Poco apropiado","Poco desapropiado","Desapropiado"), inputId = "OrdenBloques"),
      ),
      column(
        width = 6,
        prettyRadioButtons(label = "Claridad en la operacionalización", choices = c("No hay", "Ligera", "Mucha"), inputId = "Operacionalizacion")
      ),
      column(
        width = 6,
        prettyRadioButtons(label = "Relación entre los conceptos y los objetivos", choices = c("No hay", "Ligera", "Mucha"), inputId = "ConceptosObjetivos")
      )
    ),
    # End bloques del cuestionario
    
    # Solicitud de respuesta
    h3("Solicitud de respuesta"),
    tags$hr(),
    fluidRow(
      column(
        width = 6,
        prettyRadioButtons(label = "Deseabilidad social", choices = c("No hay", "Ligera", "Mucha"), inputId = "DeseabilidadSocial")
      ),
      column(
        width = 6,
        pickerInput(label = "Centralidad", choices = c("Seleccione una" = '', "No es central","Poco central","Algo centrales","Central", "Muy central"), inputId = "Centralidad"),
      ),
      column(
        width = 6,
        prettyRadioButtons(label = "Énfasis en opiniones subjetivas en la solicitud de respuesta", choices = c("Presente", "No presente"), inputId = "EnfasisSRespuesta")
      ),
      column(
        width = 6,
        prettyRadioButtons(label = "Balance en la solicitud de respuesta", choices = c("Balanceada", "No balanceada", "No aplica"), inputId = "BalanceSRespuesta")
      ),
      column(
        width = 6,
        prettyRadioButtons(label = "Solicitudes con supuestos implícitos", choices = c("Presente", "No presente"), inputId = "SolicitudesImplicitos")
      ),
      column(
        width = 6,
        prettyRadioButtons(label = "Presencia de doble barril", choices = c("Presente", "No presente"), inputId = "DobleBarril")
      ),
      column(
        width = 6,
        numericInput(label = "Número de enunciados en la solicitud de respuesta", min = 1, value = 1, inputId = "NEnunciadosRespuesta")
      )
    ),
    # End solicitud de respuesta
    
    #Categoría de respuesta
    h3("Categoría de respuesta"),
    tags$hr(),
    fluidRow(
      class="RowCategoriaRespuesta",
      column(
        width = 12,
        pickerInput(label = "Categorías de respuesta", choices = c("Seleccione una" = '', "Libre", "Nominal","Ordinal","Dicotómicas","Numéricas", "Cuantificadores vagos","Dibujo en línea", "Orden de importancia"), inputId = ns("CategoriaRespuesta")),
      )
    ),
    #End categoría de respuesta
    
    #Extras
    h3("Extras"),
    tags$hr(),
    fluidRow(
      class="RowExtras",
      column(
        width = 6,
        prettyRadioButtons(label = "Información extra", choices = c("Presente", "No presente"), inputId = "InfExtra")
      ),
      column(
        width = 6,
        prettyRadioButtons(label = "Material de apoyo visual", choices = c("Presente", "No presente"), inputId = ns("material"))
      ),
    ),
    #End extras
    actionButton(inputId = "Guardar", "Guardar", class = "btn-primary"),
    # Ayudas
    div(
      class = "AyudasContainer",
      tags$p("Suele haber deseabilidad social en temas como caridad, finanzas, información y cultura de la persona, gobierno e instituciones. 
      Sobre todo en temas que se refieren al racismo, violencia, religión, comportamiento de voto, situación política, crímenes, drogas, relaciones sexuales.", `ref-id`="DeseabilidadSocial"),
      
      tags$p("Esto se refiere a si el tema es de cercanía para los entrevistados, si le es familiar o no. Por ejemplo:
- No es central: Responden algo sobre lo que podrían no conocer,ejemplo coches solares.
- Poco central: Los entrevistadores pueden conocer algo sobre el tema, ejemplo, condiciones laborales.
- Algo centrales: No es algo que tengan que haber pensado antes pero que fácilmente pueden responder con base en su experiencia, si confían en su sistema de justicia.
- Central: Sobre actividades comunes de una persona, pero aún así tienen que pensar o recordar para medir su respuesta. Ej. ¿Cuánto tiempo a la semana ve la tele?
- Muy Central: El entrevistado puede dar una respuesta casi inmediata sobre sus actividades comunes, ejemplo, ¿Cuál es grado de escolaridad?", `ref-id` = "Centralidad"),
      
      tags$p('Una solicitud puede hacer hincapié en la opinión subjetiva del encuestado o no. Está presente cuando, por ejemplo, se usan frases
como "Por favor, danos tu opinión sobre ...", "¿Qué piensas sobre ...?", "Según usted cuál es el …"', `ref-id` = "EnfasisSRespuesta"),
      
      tags$p('Cuando se le pide una escala al entrevistado, esta escala debe estar balanceada.
Balanceada: conceptos bipolares, ejemplo, ¿a usted le gustan o le disgustan los perritos?
No balanceada: Solo se menciona uno de los conceptos bipolares, ¿A usted le disgustan los perritos?
No aplica: Cuando las respuestas son unipolares.', `ref-id` = "BalanceSRespuesta"),
      
      tags$p('Hay solicitudes de respuestas que asumen un primer componente que no se pregunta literalmente pero que es implícitamente verdadero para responder al segundo componente. Ejemplo: ¿Cuál es el mejor libro que leíste el año pasado? Aquí, la suposición oculta es que los encuestados realmente leen libros. 
    Si el componente oculto se hace explícito en una solicitud separada, el problema se resuelve.', `ref-id` = "SolicitudesImplicitos"),
      
      tags$p('Doble barril se refiere a cuando se está haciendo una solicitud doble al entrevistado en un mismo ítem.',`ref-id` = "DobleBarril"),
      
      tags$p('Este es el número de oraciones que componen la solicitud, sin contar los de la introducción, instrucción, definición motivación u opciones de respuesta, etcétera.',`ref-id` = "NEnunciadosRespuesta")
    ),
    # End ayudas
  )
}
    
#' comunicacion Server Function
#'
#' @noRd 
mod_comunicacion_server <- function(input, output, session){
  ns <- session$ns
  
  observeEvent(input$material,{
    if(input$material == "Presente"){
      insertUI(
        selector = '.RowExtras', 
        where = "afterEnd",
        ui = fluidRow(
          class="RowExtrasImplemented",
          column(
            width = 6,
            pickerInput(label = "Superposición de categorías y etiquetas de escala", choices = c("Superposición presente", "Texto conectado correctamente con su categoría"), inputId = "SuperposicionCategoriaEtiquetaEscala")
          ),
          column(
            width = 6,
            prettyRadioButtons(label = "Inicio de la frase de respuesta en la ayuda visual", choices = c("Presente", "No presente"), inputId = "InicioFraseRespuesta")
          ),
          column(
            width = 6,
            prettyRadioButtons(label = "Imagen dentro del apoyo visual", choices = c("Presente", "No presente"), inputId = "ImagenApoyo")
          )
        )
      )
    }
    else{
      removeUI(
        selector = ".RowExtrasImplemented"
      )
    }
  })
  observeEvent(input$ConBasicos,{
    for (value in input$ConBasicos) {
      if(value == "Otro"){
        insertUI(
          selector = '.RowInvestigacion', 
          where = "beforeEnd",
          ui = column(width = 6, 
                      class="ColumnEspecifique",
                      textInput(inputId = "Especifique", placeholder = "...", label = "Especifique")
          )
        )
        break;
      }
      else{
        removeUI(
          selector = ".ColumnEspecifique"
        )
      }
    }
  })
  observeEvent(input$CategoriaRespuesta,{
    removeUI(
       selector = ".RowCategoriaImplemented"
    )
    insertUI(
      selector = '.RowCategoriaRespuesta', 
      where = "afterEnd",
      ui = fluidRow(
        class = "RowCategoriaImplemented",
        if(input$CategoriaRespuesta == "Nominal" || input$CategoriaRespuesta == "Ordinal" || input$CategoriaRespuesta == "Cuantificadores vagos" || input$CategoriaRespuesta == "Orden de importancia"){
          column(
            width = 6,
            numericInput(label = "Número de categorías", min = 1, value = 1, inputId = "NCategorias")
          )
        },
        if(input$CategoriaRespuesta == "Nominal" || input$CategoriaRespuesta == "Ordinal" || input$CategoriaRespuesta == "Cuantificadores vagos" || input$CategoriaRespuesta == "Orden de importancia"){
          column(
            width = 6,
            prettyRadioButtons(label = "Categorías mutuamente excluyente", choices = c("Presente", "No presente"), inputId = ns("CatExcluyentes"))
          )
        },
        if(input$CategoriaRespuesta == "Numéricas" || input$CategoriaRespuesta == "Ordinal" || input$CategoriaRespuesta == "Cuantificadores vagos"){
          column(
            width = 6,
            prettyRadioButtons(label = "Categoría neutral", choices = c("Presente", "No presente"), inputId = ns("CatNeutral"))
          )
        },
        if(input$CategoriaRespuesta == "Numéricas" || input$CategoriaRespuesta == "Ordinal" || input$CategoriaRespuesta == "Cuantificadores vagos"){
          column(
            width = 6,
            prettyRadioButtons(label = "Equilibrio entre las categorías", choices = c("Presente", "No presente"), inputId = ns("EquilibrioCat"))
          )
        },
        if(input$CategoriaRespuesta == "Nominal" || input$CategoriaRespuesta == "Ordinal" || input$CategoriaRespuesta == "Cuantificadores vagos" || input$CategoriaRespuesta == "Orden de importancia" || input$CategoriaRespuesta == "Numéricas"){
          column(
            width = 6,
            prettyRadioButtons(label = "No sabe/No contesta", choices = c("Presente", "No presente"), inputId = ns("NoSabeNoContest"))
          )
        },
        if(input$CategoriaRespuesta == "Nominal"){
          column(
            width = 6,
            prettyRadioButtons(label = "Otro", choices = c("Presente", "No presente"), inputId = ns("CatOtro"))
          )
        }
      )
    )
  })
 
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
