## code to prepare `DB_MichGeograf` dataset goes here
library(tidyverse)
library(magrittr)
library(sf)
DB_MichGeograf <- st_read("~/GerenciaPoder/Mapa/MUNICIPIO.shp", options = "ENCODING=WINDOWS-1252") %>% 
  st_transform(st_crs(4326)) %<>% mutate(n = sample(c(0, 1, 2, 3, 4, 5), 
                                                    prob = c(.3, .3, .1, .1, .0, 0),
                                                    size = 113, replace= T  ))

usethis::use_data(DB_MichGeograf, overwrite = TRUE)
