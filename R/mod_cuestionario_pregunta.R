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
        numericInput(inputId = ns("numTotalPreguntasBloque"), label = "Indique el total de preguntas que constituyen al bloque", value = 1, min = 1, max = 100)
      )
    ),
    h3("Análisis de solicitudes de respuesta"),
    tags$hr(),
    fluidRow(
      column(width = 6,
             numericInput(inputId = ns("numDeseabilidadSocial"), label = "Número de preguntas con deseabilidad social", value= 0, min = 0, max = 100)
      ),
      column(width = 6,
             textAreaInput(inputId = ns("obsDeseabilidadSocial"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      ),
      column(width = 6,
             numericInput(inputId = ns("numSupuestosImplicitos"), label = "Número de preguntas con supuestos implícitos",value= 0, min = 0, max = 100)
      ),
      column(width = 6,
             textAreaInput(inputId = ns("obsSupuestosImplicitos"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      ),
      column(width = 6,
             numericInput(inputId = ns("numDobleBarril"), label = "Número de preguntas con doble barril", min = 0,value= 0,max = 100)
      ),
      column(width = 6,
             textAreaInput(inputId = ns("obsDobleBarril"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      ),
      column(width = 6,
             numericInput(inputId = ns("numSinBalanceRedaccion"), label = "Número de preguntas sin balance en su redacción", value= 0,min = 0, max = 100)
      ),
      column(width = 6,
             textAreaInput(inputId = ns("obsSinBalanceRedaccion"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      )
    ),
    # End solicitud de respuesta
    
    
    h3("Análisis de opciones de repsuesta"),
    tags$hr(),
    fluidRow(
      column(width = 6,
             numericInput(inputId = ns("numNoMutuamenteExcluyentes"), label = "Número de preguntas con opciones de respuesta que no son mutuamente excluyentes", value= 0,min = 0, max = 100)
      ),
      column(width = 6,
             textAreaInput(inputId = ns("obsNoMutuamenteExcluyentes"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      ),
      column(width = 6,
             numericInput(inputId = ns("numRespuestaSinEquilibrio"), label = "Número de preguntas con opciones de respuesta sin equilibrio entre ellas",value= 0, min = 0, max = 100)
      ),
      column(width = 6,
             textAreaInput(inputId = ns("obsRespuestaSinEquilibrio"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      ),
      column(width = 6,
             numericInput(inputId = ns("numDiezOpciones"), label = "Número de preguntas que tienen más de 10 opciones de respuesta",value= 0, min = 0, max = 100)
      ),
      column(width = 6,
             textAreaInput(inputId = ns("obsDiezOpciones"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      ),
      column(width = 6,
             numericInput(inputId = ns("numCategoriaNeutral"), label = "Número de preguntas que necesitan una categoría de respuesta “neutral” o la opción de “otro” y no cuentan con ella", min = 0, value= 0,max = 100)
      ),
      column(width = 6,
             textAreaInput(inputId = ns("obsCategoriaNeutral"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      ),
    ),
    
    #End extras 
    # Ayudas
    div(
      class = "AyudasContainer",
      tags$p("Suele haber deseabilidad social en temas como caridad, finanzas, información y cultura de la persona, gobierno e instituciones. 
      Sobre todo en temas que se refieren al racismo, violencia, religión, comportamiento de voto, situación política, crímenes, drogas, relaciones sexuales.", `ref-id`=ns("DeseabilidadSocial")),
      
      tags$p("Esto se refiere a si el tema es de cercanía para los entrevistados, si le es familiar o no. Por ejemplo:
- No es central: Responden algo sobre lo que podrían no conocer,ejemplo coches solares.
- Poco central: Los entrevistadores pueden conocer algo sobre el tema, ejemplo, condiciones laborales.
- Algo centrales: No es algo que tengan que haber pensado antes pero que fácilmente pueden responder con base en su experiencia, si confían en su sistema de justicia.
- Central: Sobre actividades comunes de una persona, pero aún así tienen que pensar o recordar para medir su respuesta. Ej. ¿Cuánto tiempo a la semana ve la tele?
- Muy Central: El entrevistado puede dar una respuesta casi inmediata sobre sus actividades comunes, ejemplo, ¿Cuál es grado de escolaridad?", `ref-id` = ns("Centralidad")),
      
      tags$p('Una solicitud puede hacer hincapié en la opinión subjetiva del encuestado o no. Está presente cuando, por ejemplo, se usan frases
como "Por favor, danos tu opinión sobre ...", "¿Qué piensas sobre ...?", "Según usted cuál es el …"', `ref-id` = ns("EnfasisSRespuesta")),
      
      tags$p('Cuando se le pide una escala al entrevistado, esta escala debe estar balanceada.
Balanceada: conceptos bipolares, ejemplo, ¿a usted le gustan o le disgustan los perritos?
No balanceada: Solo se menciona uno de los conceptos bipolares, ¿A usted le disgustan los perritos?
No aplica: Cuando las respuestas son unipolares.', `ref-id` = ns("BalanceSRespuesta")),
      
      tags$p('Hay solicitudes de respuestas que asumen un primer componente que no se pregunta literalmente pero que es implícitamente verdadero para responder al segundo componente. Ejemplo: ¿Cuál es el mejor libro que leíste el año pasado? Aquí, la suposición oculta es que los encuestados realmente leen libros. 
    Si el componente oculto se hace explícito en una solicitud separada, el problema se resuelve.', `ref-id` = ns("SolicitudesImplicitos")),
      
      tags$p('Doble barril se refiere a cuando se está haciendo una solicitud doble al entrevistado en un mismo ítem.',`ref-id` = ns("DobleBarril")),
      
      tags$p('Este es el número de oraciones que componen la solicitud, sin contar los de la introducción, instrucción, definición motivación u opciones de respuesta, etcétera.',`ref-id` = ns("NEnunciadosRespuesta"))
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
      idCuestionario = 0,
      nombre = "",
      numTotalPreguntasBloque = input$numTotalPreguntasBloque,
      numDeseabilidadSocial = input$numDeseabilidadSocial,
      obsDeseabilidadSocial = input$obsDeseabilidadSocial,
      numSupuestosImplicitos = input$numSupuestosImplicitos,
      obsSupuestosImplicitos = input$obsSupuestosImplicitos,
      numDobleBarril = input$numDobleBarril,
      obsDobleBarril = input$obsDobleBarril,
      numSinBalanceRedaccion = input$numSinBalanceRedaccion,
      obsSinBalanceRedaccion = input$obsSinBalanceRedaccion,
      numNoMutuamenteExcluyentes = input$numNoMutuamenteExcluyentes,
      obsNoMutuamenteExcluyentes = input$obsNoMutuamenteExcluyentes,
      numRespuestaSinEquilibrio = input$numRespuestaSinEquilibrio,
      obsRespuestaSinEquilibrio = input$obsRespuestaSinEquilibrio,
      numDiezOpciones = input$numDiezOpciones,
      obsDiezOpciones = input$obsDiezOpciones,
      numCategoriaNeutral = input$numCategoriaNeutral,
      obsCategoriaNeutral = input$obsCategoriaNeutral,
      fechaAlta = "",
      usuarioCrea = "",
      activo = 1
      )
  })
  
  
  observeEvent(valores,{
    if(!is.null(valores)){
      isolate({
        updateNumericInput(session = parent_session, inputId = ns('numTotalPreguntasBloque'), value = valores$numTotalPreguntasBloque)
        updateNumericInput(session = parent_session, inputId = ns("numDeseabilidadSocial"), value = valores$numDeseabilidadSocial)
        updateTextAreaInput(session = parent_session, inputId = ns("obsDeseabilidadSocial"), value = valores$obsDeseabilidadSocial)
        updateNumericInput(session = parent_session, inputId = ns("numSupuestosImplicitos"), value = valores$numSupuestosImplicitos)
        updateTextAreaInput(session = parent_session, inputId = ns("obsSupuestosImplicitos"), value = valores$obsSupuestosImplicitos)
        updateNumericInput(session = parent_session, inputId = ns("numDobleBarril"), value = valores$numDobleBarril)
        updateTextAreaInput(session = parent_session, inputId = ns("obsDobleBarril"), value = valores$obsDobleBarril)
        updateNumericInput(session = parent_sssion, inputId = ns("numSinBalanceRedaccion"), value = valores$numSinBalanceRedaccion)
        updateTextAreaInput(session = parent_session, inputId = ns("obsSinBalanceRedaccion"), value = valores$obsSinBalanceRedaccion)
        updateNumericInput(session = parent_session, inputId = ns("numNoMutuamenteExcluyentes"), value = valores$numNoMutuamenteExcluyentes)
        updateTextAreaInput(session = parent_session, inputId = ns("obsNoMutuamenteExcluyentes"), value = valores$obsNoMutuamenteExcluyentes)
        updateNumericInput(session = parent_session, inputId = ns("numRespuestaSinEquilibrio"), value = valores$numRespuestaSinEquilibrio)
        updateTextAreaInput(session = parent_session, inputId = ns("obsRespuestaSinEquilibrio"), value = valores$obsRespuestaSinEquilibrio)
        updateNumericInput(session = parent_session, inputId = ns("numDiezOpciones"), value = valores$numDiezOpciones)
        updateTextAreaInput(session = parent_session, inputId = ns("obsDiezOpciones"), value = valores$obsDiezOpciones)
        updateNumericInput(session = parent_session, inputId = ns("numCategoriaNeutral"), value = valores$numCategoriaNeutral)
        updateTextAreaInput(session = parent_session, inputId = ns("obsCategoriaNeutral"), value = valores$obsCategoriaNeutral)
      })
    }
  })
  return (out)
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
