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
             div(class = "tramoContainer",
                 shinydashboardPlus::boxPlus(
                   title = titulo,
                   collapsible = T,
                   width = 12,
                   closable = F,
                   actionButton(
                     inputId = ns("addEvento"),
                     "Agregar evento",
                     class = "btn btn-primary"
                   )
                 ),
                 div(class = "tramo",
                     textOutput(ns("tramoTexto"))
                     )
           )
      )
    )
  )
}
    
#' lugaresPaso3 Server Function
#'
#' @noRd 

mod_lugaresPaso3_server <- function(input, output, session, lugar, parent_session = NULL, horariosOcupados = NULL, horariosPaso1 = NULL, infoTramo = NULL, index = 0){
  ns <- session$ns
  
  observeEvent(input$addEvento,{

    showModal(modalDialog(title = "Evento",
                          mod_evento_ui(ns(glue::glue("evento_ui_{uiCount$val}"))),
                          footer = actionButton(ns("agregar"),"Agregar"),
                          easyClose = T
                          )
              )
    callModule(mod_evento_server, glue::glue("evento_ui_{uiCount$val}"), parent_session = parent_session, horariosOcupados = horariosOcupados, horariosPaso1 = horariosPaso1)
  })
 
  eventos <- reactiveValues()
  uiCount <- reactiveValues(val = 1)
  observeEvent(input$agregar,{
    removeModal()
    eventos[[as.character(uiCount$val)]] <- callModule(mod_evento_server, glue::glue("evento_ui_{uiCount$val}"), parent_session = parent_session, horariosPaso1 = horariosPaso1)
    insertUI(selector = glue::glue("#{ns('addEvento')}"),where = "beforeBegin",
             ui = div(class= "ButtonWDeleteAddon", id=paste0("evento-",uiCount$val),HTML(
                 input_btns(ns("eliminar"), users = uiCount$val, tooltip = paste0("Eliminar: ",eventos[[as.character(uiCount$val)]]()$nombre), icon ="trash-o", status = "danger"),
                 input_btns(ns("editar"), users = uiCount$val, tooltip = paste0("Editar ", eventos[[as.character(uiCount$val)]]()$nombre), label = eventos[[as.character(uiCount$val)]]()$nombre)
                                                    ))
    )
    # Se guardan horarios ocupados
    
    horariosOcupados$index <- append(horariosOcupados$index, uiCount$val)
    horariosOcupados$lugar <- append(horariosOcupados$lugar, lugar)
    horariosOcupados$evento <- append(horariosOcupados$evento, eventos[[as.character(uiCount$val)]]()$nombre)
    horariosOcupados$fecha <- append(horariosOcupados$fecha, eventos[[as.character(uiCount$val)]]()$fechaEvento)
    horariosOcupados$horaInicio <- append(horariosOcupados$horaInicio, eventos[[as.character(uiCount$val)]]()$inicioEvento)
    horariosOcupados$horaFinal <- append(horariosOcupados$horaFinal, eventos[[as.character(uiCount$val)]]()$finEvento)
    
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
    
    callModule(mod_evento_server, glue::glue("evento_ui_{input$editar}"), valores = eventos[[as.character(input$editar)]](), parent_session = parent_session, editar = T, horariosOcupados = horariosOcupados, horariosPaso1 = horariosPaso1, index = input$editar)
    actualEditable$value <- input$editar
  })
  observeEvent(input$editarModal,{
    removeModal()
    eventos[[as.character(actualEditable$value)]] <- callModule(mod_evento_server, glue::glue("evento_ui_{actualEditable$value}"), valores = NULL, parent_session = parent_session)
    removeUI(selector = paste0("#evento-",actualEditable$value))
    data <- eventos[[as.character(actualEditable$value)]]()
    insertUI(selector = glue::glue("#{ns('addEvento')}"),where = "beforeBegin",
             ui = div(class= "ButtonWDeleteAddon", id = paste0("evento-",actualEditable$value),HTML(
               input_btns(ns("eliminar"), users = actualEditable$value, tooltip = paste0("Eliminar: ",data$nombre), icon ="trash-o", status = "danger"),
               input_btns(ns("editar"), users = actualEditable$value, tooltip = paste0("Editar ", data$nombre), label = data$nombre)
             ))
    )
    
    # Se edita el horario ocupado por índice
    if(!is.null(horariosOcupados) & length(horariosOcupados$lugar) > 0){
      for(i in 1:length(horariosOcupados$lugar)){
        if(horariosOcupados$index[i] == actualEditable$value){
          horariosOcupados$evento[i] <- data$nombre
          horariosOcupados$fecha[i] <- data$fechaEvento
          horariosOcupados$horaInicio[i] <- data$inicioEvento
          horariosOcupados$horaFinal[i] <- data$finEvento
        }
      }
    }
    
  })
  observeEvent(input$eliminar,{
    shinyalert::shinyalert(title = "Eliminar", 
                           text = glue::glue("¿Está seguro que desea eliminar el evento: {eventos[[as.character(input$eliminar)]]()$nombre}?"),
                           showCancelButton = T,showConfirmButton = T,cancelButtonText = "No",
                           confirmButtonText = "Sí", 
                           callbackR = function(x) if(x) {
                             eventos[[as.character(input$eliminar)]] <- reactive(tibble(nombre = NA, descripcion = NA, contacto = NA, duracion = NA))
                             
                             # Se eliminan horarios ocupados
                             if(!is.null(horariosOcupados) & length(horariosOcupados$lugar) > 0){
                               for(i in 1:length(horariosOcupados$lugar)){
                                 if(horariosOcupados$index[i] == input$eliminar){
                                   horariosOcupados$fecha[i] <- "1995-01-01"
                                   horariosOcupados$horaInicio[i] <- ""
                                   horariosOcupados$horaFinal[i] <- ""
                                 }
                               }
                             }
                             browser()
                             # Se elimina elemento de la ui
                             removeUI(selector = paste0("#evento-",input$eliminar))
                           })
  })
  ev <- reactive({
    browser()
    seq_len(uiCount$val-1) %>% map(~eventos[[as.character(.x)]]() %>% mutate(lugar = lugar)) %>% do.call(rbind,.) %>% na.omit()
  })
  
  return(ev)
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
