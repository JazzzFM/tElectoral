#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny tibble purrr
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  # Login
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(db = tibble(user ="admin",
                                                                    password = "1"))
  )
  # Portada
  callModule(mod_portada_server, "portada_ui_1")
  # Investigación
  callModule(mod_investigacionAnalisis_server, "investigacionAnalisis_ui_1")
  # Protocolo de cuestionarios
  callModule(mod_comunicacion_server, "comunicacion_ui_1", session)
  # End cuestionario
  
  # Gira
  callModule(mod_gira_server, "gira_ui_1", session)

  # Pantalla de eventos
  callModule(mod_analisisEventos_server, "analisisEventos_ui_1")

  # Evaluación gira
  callModule(mod_evaluacionGira_server, "evaluacionGira_ui_1", session)

}
