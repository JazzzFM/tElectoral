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
        numericInput(inputId = ns("numPreguntas"), label = "Indique el total de preguntas que constituyen al bloque", value = 1, min = 1, max = 100)
      )
    ),
    h3("Análisis de solicitudes de respuesta"),
    tags$hr(),
    fluidRow(
      column(width = 6,
             numericInput(inputId = ns("deseabilidadSocial"), label = "Número de preguntas con deseabilidad social", value= 0, min = 0, max = 100)
      ),
      column(width = 6,
             textAreaInput(inputId = ns("observacionDeseabilidadSocial"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      ),
      column(width = 6,
             numericInput(inputId = ns("supuestosImplicitos"), label = "Número de preguntas con supuestos implícitos",value= 0, min = 0, max = 100)
      ),
      column(width = 6,
             textAreaInput(inputId = ns("observacionSupuestosImplicitos"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      ),
      column(width = 6,
             numericInput(inputId = ns("dobleBarril"), label = "Número de preguntas con doble barril", min = 0,value= 0,max = 100)
      ),
      column(width = 6,
             textAreaInput(inputId = ns("observacionDobleBarril"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      ),
      column(width = 6,
             numericInput(inputId = ns("balanceRedaccion"), label = "Número de preguntas sin balance en su redacción", value= 0,min = 0, max = 100)
      ),
      column(width = 6,
             textAreaInput(inputId = ns("observacionBalanceRedaccion"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      )
    ),
    # End solicitud de respuesta
    
    
    h3("Análisis de opciones de repsuesta"),
    tags$hr(),
    fluidRow(
      column(width = 6,
             numericInput(inputId = ns("opcionesNoMutuamenteExcluyentes"), label = "Número de preguntas con opciones de respuesta que no son mutuamente excluyentes", value= 0,min = 0, max = 100)
      ),
      column(width = 6,
             textAreaInput(inputId = ns("observacionOpcionesNoMutuamenteExcluyentes"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      ),
      column(width = 6,
             numericInput(inputId = ns("opcionesSinEquilibrio"), label = "Número de preguntas con opciones de respuesta sin equilibrio entre ellas",value= 0, min = 0, max = 100)
      ),
      column(width = 6,
             textAreaInput(inputId = ns("observacionOpcionesSinEquilibrio"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      ),
      column(width = 6,
             numericInput(inputId = ns("masDiezRespuesta"), label = "Número de preguntas que tienen más de 10 opciones de respuesta",value= 0, min = 0, max = 100)
      ),
      column(width = 6,
             textAreaInput(inputId = ns("observacionMasDiezRespuesta"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
      ),
      column(width = 6,
             numericInput(inputId = ns("respuestaNeutral"), label = "Número de preguntas que necesitan una categoría de respuesta “neutral” o la opción de “otro” y no cuentan con ella", min = 0, value= 0,max = 100)
      ),
      column(width = 6,
             textAreaInput(inputId = ns("observacionRespuestaNeutral"), label = "Observaciones", value = "", rows = 5, placeholder = "(Opcional)")
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
  
  
  observeEvent(valores,{
    if(!is.null(valores)){
      isolate({
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
      })
    }
  })
  return (out)
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
