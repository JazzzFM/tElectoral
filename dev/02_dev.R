# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "thinkr" )
usethis::use_package("shinymanager")
usethis::use_package("tibble")
usethis::use_package( "shinydashboard" )
usethis::use_package( "shinyWidgets" )
usethis::use_package( "htmltools" )
usethis::use_package( "shinydashboardPlus" )
usethis::use_package( "shinyjs" )
usethis::use_package("sf")
usethis::use_package("httr")
usethis::use_package("leaflet")
usethis::use_package( "shinyTime" )
usethis::use_package( "shinyalert" )
usethis::use_package( "purrr" )
usethis::use_package( "stringr" )
usethis::use_package( "lubridate" )
usethis::use_package( "htmltools" )

# no se instaló bs4Dash a propósito, para ver que pasa

## Add modules ----
## Create a module infrastructure in R/
# Portada
golem::add_module( name = "portada" ) # Name of the module
# Investigación
golem::add_module( name = "investigacionAnalisis",
                   fct = "graficos") # Name of the module
# Comunicación
golem::add_module( name = "comunicacion" )
# Registro
golem::add_module( name = "registro" )
golem::add_module( name = "registroGira" )

## Add helper functions ----
## Creates ftc_* and utils_*
# Inegi
golem::add_fct(name = "inegi" )
golem::add_fct( "helpers" ) 
golem::add_utils( "helpers" )
golem::add_utils( "mandatorio" )
golem::add_utils( "alinear" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_js_file( "panelE" )
golem::add_js_file( "login" )
golem::add_css_file( "panelE")
golem::add_css_file( "tElectoral")
golem::add_css_file( "login")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("tElectoral")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

