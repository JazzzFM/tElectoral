# Ruta punto a punto
rutaPAP <- function(localidad_origen, localidad_destino, edo="Chiapas"){
  # Origen y Destino
  token.sakbe <- "0wOVFwSP-xe99-5OHN-SuxC-ZANBhoDAt6i0"
  idOrigen <- textoID(localidad_origen)
  idDestino <- textoID(localidad_destino)
  # Ruta
  path <- "http://gaia.inegi.org.mx/sakbe_v3.1/optima"
  parametros <- list(dest_i= idOrigen %>% content(as="parsed") %>% pluck(1,1,"id_dest"),
                     dest_f= idDestino %>% content(as="parsed") %>% pluck(1,1,"id_dest"),
                     v= 2,
                     type="json",
                     key=token.sakbe)
  request <- POST(path, query = parametros, verbose())
  contenido <- content(request, as = "parsed")
  # Coordenadas
  coordenadasIni <- idOrigen %>%
    content(as="parsed") %>% 
    pluck("data", 1,"geojson") %>%
    jsonlite::fromJSON() %>% pluck("coordinates")
  coordenadasDest <- idDestino %>%
    content(as="parsed") %>% 
    pluck("data", 1,"geojson") %>%
    jsonlite::fromJSON() %>% pluck("coordinates")
  # Mapa
  mapa <- rutaLeaflet(contenido) %>% 
    addMarkers(lng = coordenadasIni[1], lat = coordenadasIni[2]) %>% 
    addMarkers(lng = coordenadasDest[1], lat = coordenadasDest[2]) 
  # Info
  info <- contenido %>% pluck("data") %>% as_tibble() %>% select(-geojson)
  list(mapa,info)
}
# Extraer info de un lugar
textoID <- function(localidad_origen){
  token.sakbe <- "0wOVFwSP-xe99-5OHN-SuxC-ZANBhoDAt6i0"
  # Conseguir parámetros para el ruta a ruta: id
  path <- "http://gaia.inegi.org.mx/sakbe_v3.1/buscadestino"
  parametros <- list(type= "json",
                     buscar=paste(localidad_origen, "Chiapas",sep = ", "),
                     num=1,
                     key=token.sakbe)
  request <- POST(path, query = parametros, verbose())
  # request %>% content(as="parsed") %>% pluck(1,1,"id_dest")
  
}
# Mapa
rutaLeaflet <- function(ruta){
  a <- jsonlite::fromJSON(ruta$data$geojson) %>%
    pluck("coordinates") %>% reduce(rbind)
  a %>% as_tibble()%>% 
    leaflet() %>%
    addTiles() %>% 
    addCircles(lng =~ V1,
               lat=~V2,
               radius = .1) 
}


# archivos <- list.files("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/SHP/2017/07 - Chiapas/",pattern = ".shp")
# cmun.sf <- read_sf(paste0("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/SHP/2017/07 - Chiapas/",archivos[9]))
# i <- 6
# r <- rutaPAP(cmun.sf$LOCALIDAD_.1[i],cmun.sf$LOCALIDAD_.1[118-i])

