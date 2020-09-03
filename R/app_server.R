#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny tibble
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  #Imágenes login
  output$fsi <- renderImage({
    return(list(
      src = app_sys("app/www/img/login-forma-superior-izquierda.svg")
    ))
  }, deleteFile = FALSE)
  
  output$fii <- renderImage({
    return(list(
      src = app_sys("app/www/img/login-forma-inferior-izquierda.svg")
    ))
  }, deleteFile = FALSE)
  
  output$fc <- renderImage({
    return(list(
      src = app_sys("app/www/img/login-forma-centro.svg")
    ))
  }, deleteFile = FALSE)
  
  output$fid <- renderImage({
    return(list(
      src = app_sys("app/www/img/login-forma-inferior-derecho.svg")
    ))
  }, deleteFile = FALSE)
  
  output$fsd <- renderImage({
    return(list(
      src = app_sys("app/www/img/login-puntos-superior-derecho.svg")
    ))
  }, deleteFile = FALSE)
  #end imágenes login
  # Login
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(db = tibble(user ="admin",
                                                                    password = "1"))
  )
  # Portada
  callModule(mod_portada_server, "portada_ui_1")
  # Investigación
  callModule(mod_investigacionAnalisis_server, "investigacionAnalisis_ui_1")
  

}
