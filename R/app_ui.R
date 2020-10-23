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
      header = dashboardHeader(title = "Tablero electoral"),
      sidebar = dashboardSidebar(#expand_on_hover = F,
        # Sidebar #####
        sidebarMenu(
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
                   icon = icon("th"),
                   menuSubItem("Crear", tabName = "giraCrear"),
                   menuSubItem("Evaluar", tabName = "giraEvaluar"),
                   menuSubItem("Análisis", tabName = "analisisEventos")
                    ),
          menuItem("Representantes generales",
                   tabName = "representanteGeneral",
                   icon = icon("dashboard")),
          menuItem("Representantes de casilla",
                   tabName = "representantes",
                   icon = icon("dashboard")),
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
      body = dashboardBody(
        tabItems(
          tabItem(tabName = "inicio", 
                  mod_portada_ui("portada_ui_1")),
          tabItem(tabName = "investigacion",
                  mod_investigacionAnalisis_ui("investigacionAnalisis_ui_1")),
          tabItem(tabName = "comunicacion",
                  mod_comunicacion_ui("comunicacion_ui_1")),
          tabItem(tabName = "analisisEventos",
                  mod_analisisEventos_ui("analisisEventos_ui_1")
                  ),
          tabItem(tabName = "giraCrear",
                  mod_gira_ui("gira_ui_1")),
          tabItem(tabName = "representantes",
                  mod_representantes_ui("representantes_ui_1")
          ),
          tabItem(tabName = "representanteGeneral",
                  mod_representantesGenerales_ui("representantesGenerales_ui_1")
          ),
          tabItem(tabName = "giraEvaluar",
                  mod_evaluacionGira_ui("evaluacionGira_ui_1")
          )
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

