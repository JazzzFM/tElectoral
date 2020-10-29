## code to prepare `DB_MichGeograf` dataset goes here
library(tidyverse)
library(magrittr)
library(sf)

MichGeograf <- st_read("~/GerenciaPoder/Mapa/MUNICIPIO.shp",options = "ENCODING=WINDOWS-1252")

usethis::use_data(DB_MichGeograf, overwrite = TRUE)
