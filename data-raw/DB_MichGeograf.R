## code to prepare `DB_MichGeograf` dataset goes here
library(tidyverse)
library(magrittr)
library(sf)
DB_MichGeograf <- st_read("~/GerenciaPoder/Mapa/MUNICIPIO.shp", options = "ENCODING=WINDOWS-1252") %>% 
  st_transform(st_crs(4326)) %<>% mutate(n = sample(1:200,size = nrow(.)))
usethis::use_data(DB_MichGeograf, overwrite = TRUE)
