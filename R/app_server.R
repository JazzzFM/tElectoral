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
  
  bd <- reactiveValues(
    eventos = leerBd(pool,eventosBd),
    giras = leerBd(pool,girasBd),
    evaluacionEventos = leerBd(pool,evaluacionEventosBd),
    encuestas = leerBd(pool, formGeneralBd),
    listadoDisMuestral = leerBd(pool, formDisMuestralBd),
    listadoIntVoto = leerBd(pool, formIntVotoBd)
  )
  # Portada
  callModule(mod_portada_server, "portada_ui_1")
  # Investigación
  callModule(mod_investigacionAnalisis_server, "investigacionAnalisis_ui_1")
  callModule(mod_investigacionFormularioGeneral_server, "investigacionFormularioGeneral_ui_1", bd, res_auth, parent_session = session)
  callModule(mod_investigacionCompartido_server, "investigacionCompartido_ui_1", bd, res_auth, session)
  #callModule(mod_investigacionFormularioIntVoto_server, "investigacionFormularioIntVoto_ui_1", res_auth, session, NULL)
  #callModule(mod_investigacionFormularioDisMuestral_server, "investigacionFormularioDisMuestral_ui_1", bd, res_auth, parent_session = session)
  #callModule(mod_investigacionEncuestas_server, "investigacionEncuestas_ui_1")
  # End Investigación
  # Protocolo de cuestionarios
  callModule(mod_comunicacion_server, "comunicacion_ui_1", session)
  # End cuestionario
  
  # Gira
  callModule(mod_gira_server, "gira_ui_1", session, 
             # bd = bd, 
             usuario = res_auth)

  # Pantalla de eventos
  callModule(mod_analisisEventos_server, "analisisEventos_ui_1", bd)

  # Evaluación gira
  callModule(mod_evaluacionGira_server, "evaluacionGira_ui_1", session, bd, res_auth)

}
