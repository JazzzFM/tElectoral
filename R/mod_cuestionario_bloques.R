#' cuestionario_bloques UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cuestionario_bloques_ui <- function(id, bloque){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             shinydashboardPlus::boxPlus(
               title = paste0("Bloque: ", bloque),
               collapsible = T,
               width = 12,
               actionButton(
                 inputId = ns("addPregunta"),
                 "Agregar pregunta",
                 class = "btn btn-primary"
               )
             )
      )
    )
  )
}
    
#' cuestionario_bloques Server Function
#'
#' @noRd 

input_btns <- function(inputId, users, tooltip, icon = "", status = "default", label = "") {
  tag <- lapply(
    X = users,
    FUN = function(x) {
      res <- tags$button(
        class = paste0("btn btn-", status),
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s',  {priority: 'event'})",
          inputId, x
        ),
        label,
        icon(icon),
        `data-toggle` = "tooltip"
      )
      res <- tagList(res, tags$script(HTML("$('[data-toggle=\"tooltip\"]').tooltip();")))
      doRenderTags(res)
    }
  )
  unlist(tag, use.names = FALSE)
}

mod_cuestionario_bloques_server <- function(input, output, session, bloque = NULL, parent_session = NULL){
  ns <- session$ns
  observeEvent(input$addPregunta,{
    
    showModal(modalDialog(title = "Pregunta",
                          mod_cuestionario_pregunta_ui(ns(glue::glue("cuestionario_pregunta_ui_{uiCount$val}"))),
                          footer = actionButton(ns("agregar"),"Agregar"),
                          easyClose = T
    )
    )
  })
  callModule(mod_cuestionario_pregunta_server, glue::glue("cuestionario_pregunta_ui_1"), valores = NULL, parent_session = parent_session)
  preguntas <- reactiveValues()
  uiCount <- reactiveValues(val = 1)
  observeEvent(input$agregar,{
    removeModal()
    preguntas[[as.character(uiCount$val)]] <- callModule(mod_cuestionario_pregunta_server, glue::glue("cuestionario_pregunta_ui_{uiCount$val}"), valores = NULL, parent_session = parent_session)
    insertUI(selector = glue::glue("#{ns('addPregunta')}"),where = "beforeBegin",
             ui = div(class= "ButtonWDeleteAddon", id=paste0("pregunta-",uiCount$val),HTML(
               input_btns(ns("eliminar"), users = uiCount$val, tooltip = paste0("Eliminar: ",preguntas[[as.character(uiCount$val)]]()$Pregunta), icon ="trash-o", status = "danger"),
               input_btns(ns("editar"), users = uiCount$val, tooltip = paste0("Editar ", preguntas[[as.character(uiCount$val)]]()$Pregunta), label = preguntas[[as.character(uiCount$val)]]()$Pregunta)
             ))
    )
    uiCount$val <- uiCount$val+1
  })
  actualEditable <- reactiveValues(value = 0)
  observeEvent(input$editar,{
    showModal(modalDialog(title = "Pregunta",
                          mod_cuestionario_pregunta_ui(ns(glue::glue("cuestionario_pregunta_ui_{input$editar}"))),
                          footer = actionButton(ns("editarModal"),"Editar"),
                          easyClose = T
    )
    )
    callModule(mod_cuestionario_pregunta_server, glue::glue("cuestionario_pregunta_ui_{input$editar}"), valores = preguntas[[as.character(input$editar)]](), parent_session = parent_session)
    actualEditable$value <- input$editar
  })
  observeEvent(input$editarModal,{
    removeModal()
    preguntas[[as.character(actualEditable$value)]] <- callModule(mod_cuestionario_pregunta_server, glue::glue("cuestionario_pregunta_ui_{actualEditable$value}"), valores = NULL, parent_session = parent_session)
    removeUI(selector = paste0("#pregunta-",actualEditable$value))
    insertUI(selector = glue::glue("#{ns('addPregunta')}"),where = "beforeBegin",
             ui = div(class= "ButtonWDeleteAddon", id = paste0("pregunta-",actualEditable$value),HTML(
               input_btns(ns("eliminar"), users = actualEditable$value, tooltip = paste0("Eliminar: ",preguntas[[as.character(actualEditable$value)]]()$Pregunta), icon ="trash-o", status = "danger"),
               input_btns(ns("editar"), users = actualEditable$value, tooltip = paste0("Editar ", preguntas[[as.character(actualEditable$value)]]()$Pregunta), label = preguntas[[as.character(actualEditable$value)]]()$Pregunta)
             ))
    )
    
  })
  observeEvent(input$eliminar,{
    shinyalert::shinyalert(title = "Eliminar", 
                           text = glue::glue("¿Está seguro que desea eliminar la pregunta: {preguntas[[as.character(input$eliminar)]]()$Pregunta}?"),
                           showCancelButton = T,showConfirmButton = T,cancelButtonText = "No",
                           confirmButtonText = "Sí", 
                           callbackR = function(x) if(x) {
                             preguntas[[as.character(input$eliminar)]] <- reactive(tibble(nombre = NA, descripcion = NA, contacto = NA, duracion = NA))
                             removeUI(selector = paste0("#pregunta-",input$eliminar))
                           })
  })
  ev <- reactive({
    seq_len(uiCount$val-1) %>% map(~preguntas[[as.character(.x)]]() %>% mutate(bloque = bloque)) %>% do.call(rbind,.) %>% na.omit()
  })
  
  return(ev)
}
    
## To be copied in the UI
# mod_cuestionario_bloques_ui("cuestionario_bloques_ui_1")
    
## To be copied in the server
# callModule(mod_cuestionario_bloques_server, "cuestionario_bloques_ui_1")
 
