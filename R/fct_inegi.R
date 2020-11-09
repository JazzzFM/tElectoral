# Crear matriz distancia de los municipios seleccionados
crearMD <- function(info){
  N <- n_distinct(c(info$origen, info$destino))
  # Convertir a matriz
  (b <- info %>%
      select(-c(geojson:peaje), tiempo_min) %>%
      pivot_wider(names_from = destino,
                  values_from= tiempo_min) %>%
      column_to_rownames(var="origen") %>%
      as.matrix())
  # Matriz simétrica
  c <- matrix(nrow = N, ncol=N)
  c[lower.tri(c)] <- b[lower.tri(b,diag = T)]
  c[upper.tri(c)] <- t(b)[upper.tri(b,diag = T)]
  diag(c) <- 0
  c
}

# Calcular el camino más corto entre los municipios seleccionados
camino_mas_corto <- function(municipios_seleccionados, info, municipios, inicio, fin){
  municipios_seleccionados <- municipios_seleccionados #%>% sort()
  # Reservar info de origen y destino de paso 1
  
  # Filtrar la matriz de información
  globalInfo <- info
  info <- info %>%
    filter(origen %in% municipios_seleccionados,
           destino %in% municipios_seleccionados)
  # Matriz de distancias
  mDist <- crearMD(info)
  # Encontrar el camino más corto
  rmc <- solve_TSP(TSP(x = mDist))
  # Encontrar ruta
  ruta <- rmc %>%
    as.integer()
  ruta_info <- map2_df(.x=ruta[1:(length(ruta)-1)],
          .y=ruta[2:length(ruta)],
          ~munRPAP %>%
            filter((origen == municipios_seleccionados [.x] & destino == municipios_seleccionados [.y]) |
                     (origen == municipios_seleccionados [.y] & destino == municipios_seleccionados [.x])
                   ) %>%
            mutate(origen=municipios_seleccionados [.x],
                   destino=municipios_seleccionados [.y]))
  # se obtienen origen de tramo 1 y destino de tramo final en los datos de la ruta más corta
  lugarOrigen <-  top_n(ruta_info, -1)
  lugarDestino <- top_n(ruta_info, 1)
  
  # se obtiene info geo de origen y destino con respecto a paso 1
  infoOrigen <- globalInfo %>%
    filter(origen %in% inicio,
           destino %in% lugarOrigen$origen)
  
  infoDestino <- globalInfo %>%
    filter(origen %in% lugarDestino$destino,
           destino %in% fin)
  ruta_info <- ruta_info %>% add_row(infoOrigen, .before = 1)
  ruta_info <- ruta_info %>% add_row(infoDestino)
  # Gráficas
  # b <- map(.x=ruta_info$geojson,
  #          ~jsonlite::fromJSON(.x) %>% pluck("coordinates") %>% reduce(rbind)) %>%
  #   reduce(rbind) %>%
  #   as_tibble()
  b <- pmap(.l=ruta_info, 
            function(geojson, origen, destino, ...){
              jsonlite::fromJSON(geojson) %>% pluck("coordinates") %>% do.call(rbind,.) %>% 
                as_tibble() %>% 
                mutate(
                  ruta = glue::glue("{origen} - {destino}"))
            }) %>%
    do.call(rbind,.) %>% sf::st_as_sf(coords=c("V1","V2"), remove=T, crs = sf::st_crs(4326))
  
  cabeceras <-  municipios %>% select(CABECERA_MUNICIPAL,coord) %>%  filter(CABECERA_MUNICIPAL %in% !!municipios_seleccionados) %>% 
    deframe %>% do.call(rbind,.) %>% data.frame %>% 
    rownames_to_column(var = "origen") %>% left_join(ruta_info %>% select(origen,costo_caseta,long_km)) %>% 
    sf::st_as_sf(coords=c("X1","X2"), remove=T, crs = sf::st_crs(4326))
  
  # mapa <- b %>%
  #   leaflet() %>%
  #   addTiles() %>%
  #   addCircles(lng =~ V1,
  #              lat=~V2,
  #              radius = .1)
  
  pal <- colorFactor(palette = topo.colors(nrow(ruta_info)),domain = unique(b$ruta))
  # limites <- st_bbox(b)
  mapa <- b %>% 
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     # minZoom = 9, maxZoom = 9,
                                     dragging = FALSE)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircles(
               radius = .1, color = ~pal(ruta),
               label = ~ruta) %>% 
    addMarkers(data = cabeceras,popup = ~glue::glue("<b> Cabecera municipal:</b> {origen} <br>
                                                    <b> Distancia por recorrer: </b>{ifelse(paste(is.na(long_km),'km'), 'No hay registro',long_km)} <br>
                                                    <b>costo_caseta:</b> {ifelse(is.na(costo_caseta), 'No hay registro',costo_caseta)}")) #%>% 
    # fitBounds( lng1 = limites$xmin, 
    #               lat1 = limites$ymin,
    #               lng2 = limites$xmax,
    #               lat2 = limites$ymax )

  # Info
  destinos <- c(ruta_info$origen, last(ruta_info$destino)) 
  minutos <- ruta_info$tiempo_min
  ruta_info <- glue::glue("Tramo: {ruta_info$origen}-{ruta_info$destino} {floor(ruta_info$tiempo_min/60)} y {round(ruta_info$tiempo_min %% 60)} minuto(s)")

  list(mapa, ruta_info, destinos, minutos)

}
