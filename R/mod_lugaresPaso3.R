#' lugaresPaso3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import htmltools lubridate DescTools
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

mod_lugaresPaso3_server <- function(input, output, session, lugar, lugarAntes, parent_session, eventos, uiCount, paso1, tiempoActual, tiempoAntes, index){
  ns <- session$ns
  # Add evento
  observeEvent(input$addEvento, {
    showModal(modalDialog(title = "Evento",
                          mod_evento_ui(ns(glue::glue("evento_ui_{uiCount$val}"))),
                          footer = actionButton(ns("agregar"),"Agregar"),
                          easyClose = T
    )
    )
    callModule(mod_evento_server, glue::glue("evento_ui_{uiCount$val}"), parent_session = parent_session, valores = NULL, paso1 = paso1, editar = as.logical(F), index = as.numeric(uiCount$val), lugar=lugar)
  })
  output$tramoTexto <- renderText({
    if(tiempoActual != ""){
      glue::glue("{floor(tiempoActual/60)} hrs. con {round(tiempoActual %% 60)} minuto(s)") 
    }
  })
  observeEvent(input$agregar, {
    evt <- callModule(mod_evento_server, glue::glue("evento_ui_{uiCount$val}"), parent_session = parent_session, valores = NULL, paso1 = paso1, editar = as.logical(F), index = as.numeric(uiCount$val), lugar = lugar)
    if(validarInfoEvento(evt())){
      allValido <- TRUE
      if(uiCount$val > 1){
        allValido <- validarHorarioOcupado(evt, eventos, uiCount) # Retorna false si está ocupado
        if(allValido)
          allValido <- validarAscendenciaHorario(evt, eventos, uiCount) # Retorna false si después de tantas fechas, hay una fecha menor a ellas
        if(index != 1 && allValido){
          res <- validarAlcanceTiempo(evt,eventos,tiempoAntes, lugarAntes, lugar, uiCount) # Retorna false si no alcanza el tiempo y el tiempo
          allValido <- as.logical(res[1])
          if(!allValido){
            shinyalert::shinyalert(title = "Advertencia", 
                                   text = glue::glue("La duración del tramo para llegar a {lugar} es de {floor(tiempoAntes/60)} hrs. con {round(tiempoAntes %% 60)} minuto(s), y de acuerdo al inicio de este evento solo tendrás {floor(res[2] / 60)} hrs. y {round(res[2] %% 60)} mins. para llegar a tu destino. ¿Deseas continuar?"),
                                   showCancelButton = T,showConfirmButton = T,cancelButtonText = "No",
                                   confirmButtonText = "Sí",
                                   callbackR = function(x) if(x) {
                                     eventos[[as.character(uiCount$val)]] <- evt()
                                     insertUI(selector = glue::glue("#{ns('addEvento')}"),where = "beforeBegin",
                                              ui = div(class= "ButtonWDeleteAddon", id=paste0("evento-",uiCount$val),HTML(
                                                input_btns(ns("eliminar"), users = uiCount$val, tooltip = paste0("Eliminar: ",eventos[[as.character(uiCount$val)]]$nombre), icon ="trash-o", status = "danger"),
                                                input_btns(ns("editar"), users = uiCount$val, tooltip = paste0("Editar ", eventos[[as.character(uiCount$val)]]$nombre), label = eventos[[as.character(uiCount$val)]]$nombre)
                                              ))
                                     )
                                     uiCount$val <- uiCount$val+1
                                     removeModal()
                                   }
            )
          }
        }
      }
      
      if(allValido == T){
        eventos[[as.character(uiCount$val)]] <- evt()
        insertUI(selector = glue::glue("#{ns('addEvento')}"),where = "beforeBegin",
                 ui = div(class= "ButtonWDeleteAddon", id=paste0("evento-",uiCount$val),HTML(
                   input_btns(ns("eliminar"), users = uiCount$val, tooltip = paste0("Eliminar: ",eventos[[as.character(uiCount$val)]]$nombre), icon ="trash-o", status = "danger"),
                   input_btns(ns("editar"), users = uiCount$val, tooltip = paste0("Editar ", eventos[[as.character(uiCount$val)]]$nombre), label = eventos[[as.character(uiCount$val)]]$nombre)
                 ))
        )
        uiCount$val <- uiCount$val+1
        removeModal()
      } 
    }
  })
  
  # Editar section
  actualEditable <- reactiveValues(value = 0)
  
  observeEvent(input$editar,{
    showModal(modalDialog(title = glue::glue("Evento {eventos[[as.character(input$editar)]]$nombre}"),
                          mod_evento_ui(ns(glue::glue("evento_ui_{input$editar}"))),
                          footer = actionButton(ns("editarModal"),"Editar"),
                          easyClose = T
    )
    )
    callModule(mod_evento_server, glue::glue("evento_ui_{input$editar}"), parent_session = parent_session, valores = eventos[[as.character(input$editar)]], paso1 = paso1, editar = as.logical(T), index = input$editar, lugar = lugar)
    actualEditable$value <- input$editar
  })
  
  observeEvent(input$editarModal, {
    evt <- callModule(mod_evento_server, glue::glue("evento_ui_{actualEditable$value}"), parent_session = parent_session, valores = NULL, paso1 = paso1, editar = as.logical(F), index = as.numeric(uiCount$val), lugar = lugar)
    if(validarInfoEvento(evt())){
      allValido <- TRUE
      if(uiCount$val > 1){
        allValido <- validarHorarioOcupado(evt, eventos, uiCount, actualEditable$value) # Retorna false si está ocupado
        if(allValido)
          allValido <- validarAscendenciaHorario(evt, eventos, uiCount, actualEditable$value) # Retorna false si después de tantas fechas, hay una fecha menor a ellas
        if(index != 1 && allValido){
          res <- validarAlcanceTiempo(evt,eventos,tiempoAntes, lugarAntes, lugar, uiCount) # Retorna false si no alcanza el tiempo y el tiempo
          allValido <- as.logical(res[1])
          if(!allValido){
            shinyalert::shinyalert(title = "Advertencia", 
                                   text = glue::glue("La duración del tramo para llegar a {lugar} es de {floor(tiempoAntes/60)} hrs. con {round(tiempoAntes %% 60)} minuto(s), y de acuerdo al inicio de este evento solo tendrás {floor(res[2] / 60)} hrs. y {round(res[2] %% 60)} mins. para llegar a tu destino. ¿Deseas continuar?"),
                                   showCancelButton = T,showConfirmButton = T,cancelButtonText = "No",
                                   confirmButtonText = "Sí",
                                   callbackR = function(x) if(x) {
                                     eventos[[as.character(actualEditable$value)]] <- evt()
                                     removeUI(selector = paste0("#evento-", actualEditable$value))
                                     insertUI(selector = glue::glue("#{ns('addEvento')}"),where = "beforeBegin",
                                              ui = div(class= "ButtonWDeleteAddon", id=paste0("evento-",actualEditable$value),HTML(
                                                input_btns(ns("eliminar"), users = actualEditable$value, tooltip = paste0("Eliminar: ",eventos[[as.character(actualEditable$value)]]$nombre), icon ="trash-o", status = "danger"),
                                                input_btns(ns("editar"), users = actualEditable$value, tooltip = paste0("Editar ", eventos[[as.character(actualEditable$value)]]$nombre), label = eventos[[as.character(actualEditable$valuel)]]$nombre)
                                              ))
                                     )
                                     removeModal()
                                   }
            )
          }
        }
      }
      
      if(allValido == T){
        eventos[[as.character(actualEditable$value)]] <- evt()
        removeUI(selector = paste0("#evento-", actualEditable$value))
        insertUI(selector = glue::glue("#{ns('addEvento')}"),where = "beforeBegin",
                 ui = div(class= "ButtonWDeleteAddon", id=paste0("evento-",actualEditable$value),HTML(
                   input_btns(ns("eliminar"), users = actualEditable$value, tooltip = paste0("Eliminar: ",eventos[[as.character(actualEditable$value)]]$nombre), icon ="trash-o", status = "danger"),
                   input_btns(ns("editar"), users = actualEditable$value, tooltip = paste0("Editar ", eventos[[as.character(actualEditable$value)]]$nombre), label = eventos[[as.character(actualEditable$value)]]$nombre)
                 ))
        )
        removeModal()
      } 
    }
  })
  # End editar
  
  # Eliminar section
  observeEvent(input$eliminar,{
    shinyalert::shinyalert(title = "Eliminar", 
                           text = glue::glue("¿Está seguro que desea eliminar el evento: {eventos[[as.character(input$eliminar)]]$nombre}?"),
                           showCancelButton = T,showConfirmButton = T,cancelButtonText = "No",
                           confirmButtonText = "Sí", 
                           callbackR = function(x) if(x) {
                             eventos[[as.character(input$eliminar)]] <- reactive(tibble(nombre = NA, 
                                                                                        lugar = NA,
                                                                                        direccion = NA,
                                                                                        descripcion = NA, 
                                                                                        contacto = NA,
                                                                                        telefono = NA,
                                                                                        correo = NA,
                                                                                        fechaEvento = NA, 
                                                                                        inicioEvento = NA,
                                                                                        finEvento = NA))()
                             # Se elimina elemento de la ui
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
 
