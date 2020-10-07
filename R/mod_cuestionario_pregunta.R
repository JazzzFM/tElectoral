#' cuestionario_pregunta UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyWidgets magrittr
mod_cuestionario_pregunta_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        textInput(inputId = ns("Pregunta"), label = "Pregunta", placeholder = "...")
      )
    ),
    h3("Solicitud de respuesta"),
    tags$hr(),
    fluidRow(
      column(
        width = 6,
        prettyRadioButtons(selected = 0, label = "Deseabilidad social", choices = c("No hay", "Ligera", "Mucha"), inputId = ns("DeseabilidadSocial"))
      ),
      column(
        width = 6,
        pickerInput(label = "Centralidad", choices = c("Seleccione una" = '', "No es central","Poco central","Algo centrales","Central", "Muy central"), inputId = ns("Centralidad")),
      ),
      column(
        width = 6,
        prettyRadioButtons(selected = 0, label = "Énfasis en opiniones subjetivas en la solicitud de respuesta", choices = c("Presente", "No presente"), inputId = ns("EnfasisSRespuesta"))
      ),
      column(
        width = 6,
        prettyRadioButtons(selected = 0, label = "Balance en la solicitud de respuesta", choices = c("Balanceada", "No balanceada", "No aplica"), inputId = ns("BalanceSRespuesta"))
      ),
      column(
        width = 6,
        prettyRadioButtons(selected = 0, label = "Solicitudes con supuestos implícitos", choices = c("Presente", "No presente"), inputId = ns("SolicitudesImplicitos"))
      ),
      column(
        width = 6,
        prettyRadioButtons(selected = 0, label = "Presencia de doble barril", choices = c("Presente", "No presente"), inputId = ns("DobleBarril"))
      ),
      column(
        width = 6,
        numericInput(label = "Número de enunciados en la solicitud de respuesta", min = 1, value = 1, inputId = ns("NEnunciadosRespuesta"))
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
        pickerInput(label = "Categorías de respuesta", choices = c("Seleccione una" = '', "Libre", "Nominal","Ordinal","Dicotómicas","Numéricas", "Cuantificadores vagos","Dibujo de línea", "Orden de importancia"), inputId = ns("CategoriaRespuesta")),
      )
    ),
    #End categoría de respuesta
    
    #Extras
    h3("Extras"),
    tags$hr(),
    fluidRow(
      column(
        width = 6,
        prettyRadioButtons(selected = 0, label = "Información extra", choices = c("Presente", "No presente"), inputId = ns("InfExtra"))
      )
    ),
    fluidRow(
      class="RowExtras",
      column(
        width = 6,
        prettyRadioButtons(selected = 0, label = "Material de apoyo visual", choices = c("Presente", "No presente"), inputId = ns("material"))
      )
    ),
    #End extras 
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
    
#' cuestionario_pregunta Server Function
#'
#' @noRd 
mod_cuestionario_pregunta_server <- function(input, output, session, valores = NULL, parent_session = NULL){
  ns <- session$ns
  out <- reactive({
    tibble(
      Pregunta = input$Pregunta,
      DeseabilidadSocial = input$DeseabilidadSocial,
      Centralidad = input$Centralidad,
      EnfasisSRespuesta = input$EnfasisSRespuesta,
      BalanceSRespuesta = input$BalanceSRespuesta,
      SolicitudesImplicitos = input$SolicitudesImplicitos,
      DobleBarril = input$DobleBarril,
      NEnunciadosRespuesta = input$NEnunciadosRespuesta,
      CategoriaRespuesta = input$CategoriaRespuesta,
      NCategorias = input$NCategorias,
      CatExcluyentes = input$CatExcluyentes,
      CatNeutral = input$CatNeutral,
      EquilibrioCat = input$EquilibrioCat,
      NoSabeNoContest = input$NoSabeNoContest,
      CatOtro = input$CatOtro,
      InfExtra = input$InfExtra,
      material = input$material,
      SuperposicionCategoriaEtiquetaEscala = input$SuperposicionCategoriaEtiquetaEscala,
      InicioFraseRespuesta = input$InicioFraseRespuesta,
      ImagenApoyo = input$ImagenApoyo
      )
  })
  observeEvent(input$material,{
    if(input$material == "Presente"){
      insertUI(
        selector = '.RowExtras', 
        where = "afterEnd",
        ui = fluidRow(
          class="RowExtrasImplemented",
          column(
            width = 6,
            pickerInput(label = "Superposición de categorías y etiquetas de escala", choices = c("Superposición presente", "Texto conectado correctamente con su categoría"), inputId = ns("SuperposicionCategoriaEtiquetaEscala"))
          ),
          column(
            width = 6,
            prettyRadioButtons(selected = 0, label = "Inicio de la frase de respuesta en la ayuda visual", choices = c("Presente", "No presente"), inputId = ns("InicioFraseRespuesta"))
          ),
          column(
            width = 6,
            prettyRadioButtons(selected = 0, label = "Imagen dentro del apoyo visual", choices = c("Presente", "No presente"), inputId = ns("ImagenApoyo"))
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
            numericInput(label = "Número de categorías", min = 1, value = 1, inputId = ns("NCategorias"))
          )
        },
        if(input$CategoriaRespuesta == "Nominal" || input$CategoriaRespuesta == "Ordinal" || input$CategoriaRespuesta == "Cuantificadores vagos" || input$CategoriaRespuesta == "Orden de importancia"){
          column(
            width = 6,
            prettyRadioButtons(selected = 0, label = "Categorías mutuamente excluyente", choices = c("Presente", "No presente"), inputId = ns("CatExcluyentes"))
          )
        },
        if(input$CategoriaRespuesta == "Numéricas" || input$CategoriaRespuesta == "Ordinal" || input$CategoriaRespuesta == "Cuantificadores vagos"){
          column(
            width = 6,
            prettyRadioButtons(selected = 0, label = "Categoría neutral", choices = c("Presente", "No presente"), inputId = ns("CatNeutral"))
          )
        },
        if(input$CategoriaRespuesta == "Numéricas" || input$CategoriaRespuesta == "Ordinal" || input$CategoriaRespuesta == "Cuantificadores vagos"){
          column(
            width = 6,
            prettyRadioButtons(selected = 0, label = "Equilibrio entre las categorías", choices = c("Presente", "No presente"), inputId = ns("EquilibrioCat"))
          )
        },
        if(input$CategoriaRespuesta == "Nominal" || input$CategoriaRespuesta == "Ordinal" || input$CategoriaRespuesta == "Dicotómicas" || input$CategoriaRespuesta == "Cuantificadores vagos" || input$CategoriaRespuesta == "Dibujo de línea" ||input$CategoriaRespuesta == "Numéricas"){
          column(
            width = 6,
            prettyRadioButtons(selected = 0, label = "No sabe/No contesta", choices = c("Presente", "No presente"), inputId = ns("NoSabeNoContest"))
          )
        },
        if(input$CategoriaRespuesta == "Nominal"){
          column(
            width = 6,
            prettyRadioButtons(selected = 0, label = "Otro", choices = c("Presente", "No presente"), inputId = ns("CatOtro"))
          )
        }
      )
    )
  })
  
  observeEvent(valores,{
    if(!is.null(valores)){
      updateTextInput(session = parent_session, inputId = ns("Pregunta"), value = valores$Pregunta)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("DeseabilidadSocial"), selected = valores$DeseabilidadSocial)
      updatePickerInput(session = parent_session, inputId = ns("Centralidad"), selected = valores$Centralidad)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("EnfasisSRespuesta"), selected = valores$EnfasisSRespuesta)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("BalanceSRespuesta"), selected = valores$BalanceSRespuesta)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("SolicitudesImplicitos"), selected = valores$SolicitudesImplicitos)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("DobleBarril"), selected = valores$DobleBarril)
      updateNumericInput(session = parent_session, inputId = ns("NEnunciadosRespuesta"), value = valores$NEnunciadosRespuesta)
      
      updatePickerInput(session = parent_session, inputId = ns("CategoriaRespuesta"), selected = valores$CategoriaRespuesta)
      updateNumericInput(session = parent_session, inputId = ns("NCategorias"), value = valores$NCategorias)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("CatExcluyentes"), selected = valores$CatExcluyentes)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("CatNeutral"), selected = valores$CatNeutral)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("EquilibrioCat"), selected = valores$EquilibrioCat)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("NoSabeNoContest"), selected = valores$NoSabeNoContest)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("CatOtro"), selected = valores$CatOtro)
      
      updatePrettyRadioButtons(session = parent_session, inputId = ns("InfExtra"), selected = valores$InfExtra)
      updatePickerInput(session = parent_session, inputId = ns("SuperposicionCategoriaEtiquetaEscala"), selected = valores$SuperposicionCategoriaEtiquetaEscala)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("InicioFraseRespuesta"), selected = valores$InicioFraseRespuesta)
      updatePrettyRadioButtons(session = parent_session, inputId = ns("ImagenApoyo"), selected = valores$ImagenApoyo)
      
      updatePrettyRadioButtons(session = parent_session, inputId = ns("material"), selected = valores$material)
    }
  })
  return (out)
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
