load("data/DB_Mich.rda")

library(sf)
library(httr)
library(tidyverse)
# Funciones #####
# Extraer info de un lugar
textoID <- function(municipio,edo){
  token.sakbe <- "0wOVFwSP-xe99-5OHN-SuxC-ZANBhoDAt6i0"
  # Conseguir parámetros para el ruta a ruta: id
  path <- "http://gaia.inegi.org.mx/sakbe_v3.1/buscadestino"
  parametros <- list(type= "json",
                     buscar=paste(municipio$CABECERA_MUNICIPAL, municipio$MUNICIPIO, edo,sep = ", "),
                     num=1,
                     key=token.sakbe)
  request <- POST(path, query = parametros, verbose())
  
}

# Agregar info a todos los municipios
id_sakbe <- function(bd, edo){
  aux <- 1:nrow(bd) %>% 
    map_df(~ {
      a <- textoID(bd[.x,],edo = edo)
      b <- a %>% content(as="parsed") %>% pluck(1,1) %>% as_tibble()
      b %>% 
        mutate(coord =map(.x=.$geojson ,~jsonlite::fromJSON(.x) %>% 
                            pluck("coordinates"))) %>% 
        select(id_dest, coord)
    })
  bind_cols(bd, aux)
}

# Rutas
rutaPAP <- function(dest_i, dest_f){
  # Origen y Destino
  token.sakbe <- "0wOVFwSP-xe99-5OHN-SuxC-ZANBhoDAt6i0"
  # Ruta
  path <- "http://gaia.inegi.org.mx/sakbe_v3.1/optima"
  parametros <- list(dest_i= dest_i,
                     dest_f= dest_f,
                     v= 2,
                     type="json",
                     key=token.sakbe)
  request <- POST(path, query = parametros, verbose())
  contenido <- content(request, as = "parsed") %>% 
    pluck("data") %>% 
    as_tibble() 
}

# Crea tibble de relaciones
relMun <- function(municipios){
  N <- municipios %>% nrow()
  # Indices
  indices <- tibble(Var1=map(2:N, ~rep(.x, .x-1)) %>% unlist())
  indices <- indices %>%
    group_by(Var1) %>%
    mutate(Var2=row_number())
  # Rutas
  a <- map2_df(.x = indices$Var1, .y = indices$Var2,
               ~rutaPAP(dest_i = municipios$id_dest[.x],
                        dest_f = municipios$id_dest[.y]) %>% 
                 mutate(origen=municipios$CABECERA_MUNICIPAL[.x],
                        destino=municipios$CABECERA_MUNICIPAL[.y]))
}

# Procedimiento #####

# 1) Agregar las variables sakbe-inegi a la base DB_MICH
DB_Mich2 <- id_sakbe(DB_Mich %>% slice(1:15), "Michoacán")

# 2) Crear el tibble de relaciones
munRPAP <- relMun(DB_Mich2 %>% arrange(CABECERA_MUNICIPAL))


## code to prepare `Distancias` dataset goes here
usethis::use_data(DB_Mich2, overwrite = T)
usethis::use_data(munRPAP, overwrite = T)
