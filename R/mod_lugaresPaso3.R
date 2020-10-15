#' lugaresPaso3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import htmltools
mod_lugaresPaso3_ui <- function(id, titulo){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             shinydashboardPlus::boxPlus(
               title = titulo,
               collapsible = T,
               width = 12,
               actionButton(
                 inputId = ns("addEvento"),
                 "Agregar evento",
                 class = "btn btn-primary"
               )
             )
      )
    )
  )
}
    
#' lugaresPaso3 Server Function
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
mod_lugaresPaso3_server <- function(input, output, session, lugar, parent_session = NULL){
  ns <- session$ns
  
  observeEvent(input$addEvento,{

    showModal(modalDialog(title = "Evento",
                          mod_evento_ui(ns(glue::glue("evento_ui_{uiCount$val}"))),
                          footer = actionButton(ns("agregar"),"Agregar"),
                          easyClose = T
                          )
              )
  })
 
  eventos <- reactiveValues()
  uiCount <- reactiveValues(val = 1)
  observeEvent(input$agregar,{
    removeModal()
    eventos[[as.character(uiCount$val)]] <- callModule(mod_evento_server, glue::glue("evento_ui_{uiCount$val}"), valores = NULL, parent_session = parent_session)
    insertUI(selector = glue::glue("#{ns('addEvento')}"),where = "beforeBegin",
             ui = div(class= "ButtonWDeleteAddon", id=paste0("evento-",uiCount$val),HTML(
                 input_btns(ns("eliminar"), users = uiCount$val, tooltip = paste0("Eliminar: ",eventos[[as.character(uiCount$val)]]()$nombre), icon ="trash-o", status = "danger"),
                 input_btns(ns("editar"), users = uiCount$val, tooltip = paste0("Editar ", eventos[[as.character(uiCount$val)]]()$nombre), label = eventos[[as.character(uiCount$val)]]()$nombre)
                                                    ))
    )
    uiCount$val <- uiCount$val+1
    
  })
  actualEditable <- reactiveValues(value = 0)
  observeEvent(input$editar,{
    showModal(modalDialog(title = "Evento",
                          mod_evento_ui(ns(glue::glue("evento_ui_{input$editar}"))),
                          footer = actionButton(ns("editarModal"),"Editar"),
                          easyClose = T
      )
    )
    callModule(mod_evento_server, glue::glue("evento_ui_{input$editar}"), valores = eventos[[as.character(input$editar)]](), parent_session = parent_session)
    actualEditable$value <- input$editar
  })
  observeEvent(input$editarModal,{
    removeModal()
    eventos[[as.character(actualEditable$value)]] <- callModule(mod_evento_server, glue::glue("evento_ui_{actualEditable$value}"), valores = NULL, parent_session = parent_session)
    removeUI(selector = paste0("#evento-",actualEditable$value))
    insertUI(selector = glue::glue("#{ns('addEvento')}"),where = "beforeBegin",
             ui = div(class= "ButtonWDeleteAddon", id = paste0("evento-",actualEditable$value),HTML(
               input_btns(ns("eliminar"), users = actualEditable$value, tooltip = paste0("Eliminar: ",eventos[[as.character(actualEditable$value)]]()$nombre), icon ="trash-o", status = "danger"),
               input_btns(ns("editar"), users = actualEditable$value, tooltip = paste0("Editar ", eventos[[as.character(actualEditable$value)]]()$nombre), label = eventos[[as.character(actualEditable$value)]]()$nombre)
             ))
    )
    
  })
  observeEvent(input$eliminar,{
    shinyalert::shinyalert(title = "Eliminar", 
                           text = glue::glue("¿Está seguro que desea eliminar el evento: {eventos[[as.character(input$eliminar)]]()$nombre}?"),
                           showCancelButton = T,showConfirmButton = T,cancelButtonText = "No",
                           confirmButtonText = "Sí", 
                           callbackR = function(x) if(x) {
                             eventos[[as.character(input$eliminar)]] <- reactive(tibble(nombre = NA, descripcion = NA, contacto = NA, duracion = NA))
                             removeUI(selector = paste0("#evento-",input$eliminar))
                           })
  })
  ev <- reactive({
    seq_len(uiCount$val-1) %>% map(~eventos[[as.character(.x)]]() %>% mutate(lugar = lugar)) %>% do.call(rbind,.) %>% na.omit()
  })
  
  return(ev)
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
