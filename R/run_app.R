#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = shinymanager::secure_app(
        tags_top = div(class="headLogin",
                       div(
                         class = "bg-container-svg",
                         imageOutput("fsi"),
                         imageOutput("fii"),
                         imageOutput("fc"),
                         imageOutput("fid"),
                         imageOutput("fsd")
                       ),
                       tags$head(
                         includeCSS(app_sys("app/www/login.css")),
                         tags$link(href="https://fonts.googleapis.com/css?family=Muli:400,600,700&display=swap", rel = "stylesheet")
                       ),
                       tags$footer(
                         tags$script(async = TRUE, src = "//cdnjs.cloudflare.com/ajax/libs/jquery-validate/1.14.0/jquery.validate.js"),
                         includeScript(app_sys("app/www/login.js"))
                       )
                       , div(class="icon expanded white", icon("lock"))),
        app_ui), 
      server = app_server
    ), 
    golem_opts = list(...)
  )
}
