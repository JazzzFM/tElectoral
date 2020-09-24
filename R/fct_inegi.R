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
camino_mas_corto <- function(municipios_seleccionados, info, municipios){
  municipios_seleccionados <- municipios_seleccionados %>% sort()
  # Filtrar la matriz de información
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
                     (origen == municipios_seleccionados [.y] & destino == municipios_seleccionados [.x])) %>%
            mutate(origen=municipios_seleccionados [.x],
                   destino=municipios_seleccionados [.y]))
  # Gráficas
  b <- map(.x=ruta_info$geojson,
           ~jsonlite::fromJSON(.x) %>% pluck("coordinates") %>% reduce(rbind)) %>%
    reduce(rbind) %>%
    as_tibble()
  mapa <- b %>%
    leaflet() %>%
    addTiles() %>%
    addCircles(lng =~ V1,
               lat=~V2,
               radius = .1)

  # Info
  ruta_info <- glue::glue("Tramo: {ruta_info$origen}-{ruta_info$destino} {floor(ruta_info$tiempo_min/60)} y {round(ruta_info$tiempo_min %% 60)} minuto(s)")


  list(mapa, ruta_info)

}
