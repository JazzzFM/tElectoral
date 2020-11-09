#' giraPaso1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import lubridate
mod_giraPaso1_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Instancia de gira"),
    p("A continuación, llene los campos para completar el paso 1"),
    fluidRow(
      column(width = 12,
             textInput(inputId = ns("Responsable"), label = "Responsable" , placeholder = "..."))
    ),
    fluidRow(
      column(width = 12,
             textAreaInput(inputId = ns("Descripcion"), label = "Descripcion" , placeholder = "...", rows = 5))
    ),
    fluidRow(
      column(width = 4,
             selectizeInput(inputId = ns("LugarInicio"), label = "Lugar de inicio", choices = c("Seleccione un lugar" = "", DB_Mich2 %>% select(CABECERA_MUNICIPAL) %>% pull(CABECERA_MUNICIPAL)) )
      ),
      column(width = 4,
             dateInput(inputId = ns("FechaInicio"), label = "Fecha de inicio", format = "dd/mm/yyyy", language = "es", value = Sys.Date(), min = Sys.Date() )
      ),
      column(width = 4,
             selectizeInput(inputId = ns("HorarioInicio"), label = "Hora de inicio", choices =  c("Seleccione hora" = "", seq(
               from=as.POSIXct("2012-1-1 0:00", tz="UTC"),
               to=as.POSIXct("2012-1-1 23:00", tz="UTC"),
               by="30 min"
             ) %>% format(.,"%R"))
            )
      )
    ),
    fluidRow(
      column(width = 4,
             selectizeInput(inputId = ns("LugarFinal"), label = "Lugar de destino", choices = c("Seleccione un lugar" = "", DB_Mich2 %>% select(CABECERA_MUNICIPAL) %>% pull(CABECERA_MUNICIPAL)) )
      ),
      column(width = 4,
             dateInput(inputId = ns("FechaFinal"), label = "Fecha de finalización", format = "dd/mm/yyyy", language = "es", value = Sys.Date(), min = Sys.Date() )
      ),
      column(width = 4,
             selectizeInput(inputId = ns("HorarioFinal"), label = "Hora de finalización", choices = c("Seleccione hora" = "", seq(
               from=as.POSIXct("2012-1-1 0:00", tz="UTC"),
               to=as.POSIXct("2012-1-1 23:00", tz="UTC"),
               by="30 min"
             ) %>% format(.,"%R"))
             )
      )
    ),
    actionButton(inputId = ns("guardar"), "Guardar", class = "btn-primary")
  )
}

#' giraPaso1 Server Function
#'
#' @noRd 
mod_giraPaso1_server <- function(input, output, session, gira = NULL, parent_session, reseted){
  ns <- session$ns
  
  observeEvent(input$guardar,{
    check <- c("Responsable","Descripcion","LugarInicio","HorarioInicio","LugarFinal","HorarioFinal", "FechaInicio", "FechaFinal") %>%
      mandatory(input = input, .)
    if(check){
      if(input$LugarInicio != input$LugarFinal){
        if(input$FechaInicio != input$FechaFinal){
          showTab(inputId = "TabsGira", target = "paso2", session = parent_session)
          gira$paso1 <- tibble::tibble(
            Responsable = input$Responsable, 
            Descripcion = input$Descripcion, 
            LugarInicio = input$LugarInicio, 
            FechaInicio = input$FechaInicio,
            HorarioInicio = input$HorarioInicio, 
            LugarFinal = input$LugarFinal,
            FechaFinal = input$FechaFinal,
            HorarioFinal = input$HorarioFinal
          )
        }else{
          if(input$HorarioInicio == input$HorarioFinal){
            shinyalert::shinyalert(title = "Los horarios de inicio y fin no deben coincidir en el mismo día")  
          }else{
            i <- input$HorarioInicio
            f <- input$HorarioFinal
            res <- function(i,f){
              if(hm(i) < hm(f))
                return (hm(f) - hm(i))
              else
                return (hm(i) - hm(f))
            }
            shinyalert::shinyalert(title = "Advertencia", 
                                   text = glue::glue("¿Está seguro que desea iniciar una gira con solo {res(i,f)} de duración?"),
                                   showCancelButton = T,showConfirmButton = T,cancelButtonText = "No",
                                   confirmButtonText = "Sí", 
                                   callbackR = function(x) if(x) {
                                     showTab(inputId = "TabsGira", target = "paso2", session = parent_session)
                                     gira$paso1 <- tibble::tibble(
                                       Responsable = input$Responsable, 
                                       Descripcion = input$Descripcion, 
                                       LugarInicio = input$LugarInicio, 
                                       FechaInicio = input$FechaInicio,
                                       HorarioInicio = input$HorarioInicio, 
                                       LugarFinal = input$LugarFinal,
                                       FechaFinal = input$FechaFinal,
                                       HorarioFinal = input$HorarioFinal
                                     )
                                   })
          }
        }
      }else{
        shinyalert::shinyalert(title = "El origen y destino deben ser diferentes")  
      }
    } else{
      shinyalert::shinyalert(title = "Formato incompleto")
    }
  })
  observe({
    if(reseted$value){
      updateTextInput(session = parent_session, inputId = ns("Responsable"), value = "")
      updateTextAreaInput(session = parent_session, inputId = ns("Descripcion"), value = "")
      updateSelectInput(session = parent_session, inputId = ns("LugarInicio"), selected = "")
      updateDateInput(session = parent_session, inputId = ns("FechaInicio"), value = Sys.Date())
      updateSelectizeInput(session = parent_session, inputId = ns("HorarioInicio"), selected = "")
      updateSelectInput(session = parent_session, inputId = ns("LugarFinal"), selected = "")
      updateDateInput(session = parent_session, inputId = ns("FechaFinal"), value = Sys.Date())
      updateSelectizeInput(session = parent_session, inputId = ns("HorarioFinal"), selected = "")
      reseted$resPaso1 <- T
    }
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 

