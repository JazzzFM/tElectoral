#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinydashboard
#' @noRd
#' @ggplot2
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Dashboard
    dashboardPage(
      # tags$head(
      #   includeCSS(app_sys("app/www/tElectoral.css")),
      #   tags$link(href="https://fonts.googleapis.com/css?family=Muli:400,600,700&display=swap", rel = "stylesheet"),
      #   tags$script(async = TRUE, src = "https://platform.twitter.com/widgets.js")
      # ),
      header = dashboardHeader(title = "Tablero electoral"),
      sidebar = dashboardSidebar(#expand_on_hover = F,
        # Sidebar #####
        sidebarMenu(
          menuItem("Registro",
                   tabName = "registro",
                   icon = icon("pen")),
          menuItem("Inicio",
                   tabName = "inicio", 
                   icon = icon("dashboard")
          ),
          menuItem("Investigación",
                   tabName = "investigacion",
                   icon = icon("dashboard")),
          menuItem("Protocolo de cuestionarios",
                   tabName = "comunicacion",
                   icon = icon("dashboard")),
          menuItem("Gira",
                   tabName = "gira",
                   icon = icon("th")),
          menuItem("Gira2",
                   tabName = "gira2",
                   icon = icon("th")),
          menuItem("Finanzas",
                   tabName = "finanzas",
                   icon = icon("dashboard")),
          menuItem("Legal",
                   tabName = "legal",
                   icon = icon("th")),
          menuItem("Organización",
                   tabName = "organizacion",
                   icon = icon("dashboard")),
          menuItem("Estructura",
                   tabName = "estructura",
                   icon = icon("th")
          )
        )
        # Cuerpo #######
      ),
      body =dashboardBody(
        tabItems(
          tabItem(tabName = "registro",
                  mod_registro_ui("registro_ui_1")
          ),
          tabItem(tabName = "inicio", 
                  mod_portada_ui("portada_ui_1")),
          tabItem(tabName = "investigacion",
                  mod_investigacionAnalisis_ui("investigacionAnalisis_ui_1")),
          tabItem(tabName = "comunicacion",
                  mod_comunicacion_ui("comunicacion_ui_1")),
          tabItem(tabName = "gira2",
                  mod_lugaresGira_ui("lugaresGira_ui_1"))
          
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'tElectoral'
    ),
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

