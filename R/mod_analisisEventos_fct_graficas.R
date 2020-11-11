# bd <- select(DB_Mich, c(MUNICIPIO, lugar =CABECERA_MUNICIPAL)) %>%
#   mutate(fecha= seq(from = dmy_hm("06-01-21 11:00"), to = dmy_hm("06-02-21 11:00"), length.out =113),
#     asistentes = sample(c("Debían de haber sido más personas",
#                                "Adecuado",
#                                "Debían haber sido menos personas"),
#                              prob = c(.3, .7, .2), size = 113, replace= T ),
#          animo = sample(c('Interesados',
#                          'Participativos',
#                          'Emocionados',
#                          'Desesperados',
#                          'Molestos',
#                          'Aburridos',
#                          'Otro'),
#                        size=113, replace=T,
#                        prob=c(.1,.1,.2,.1,.1,.1,.2)),
# 
#          animo_otro = sample(c('deprimido',
#                       'triste',
#                       'ansioso',
#                       'borracho'),
#                     size=113, replace=T),
# 
#          incidente = sample(c('Perdida del sonido o audio', 'Perdida de corriente eléctrica',
#                             'Los asistentes no llegaron a tiempo','El recinto no era adecuado',
#                             'Insultos por parte de los participantes', 'Enfrentamiento entre participantes',
#                             'Salida repentina de participantes antes de que finalice el evento',
#                             'Constantes interrupciones al discurso del candidato',
#                             'Hackeo del evento (en caso de ser virtual)', 'Otro'),
#                           size=113, replace=T),
#          incidente_otro = sample(c('Se congelaba la pantalla',
#                           'Hubo balazos',
#                           'Se rompió el templete',
#                           'Las autoridad local no lo permitió'),
#                     size=113, replace=T,
#                     prob=c(.3,.1,.2,.2)),
# 
#          duracion = sample(c("Debía de haber durado menos tiempo",
#                               "Adecuado", "Debía de haber durado más tiempo"),
#                            prob = c(.3, .7, .2), size = 113, replace= T ),
# 
#          calidad =  sample(c("Muy buena calidad",
#                                       "Buena calidad",
#                                       "Mala calidad" ,
#                                       "Muy mala calidad"),
#                                    prob = c(.5, .7, .3, .2), size = 113, replace= T ),
#          calif = sample(c(0:10), size = 113, replace = T,
#                       prob=c(.005,.01,.02,.1,.2,.3,.4,.6,.5,.4,.3))
#   )
# 
# # lineaCalificacion(bd, fecha = fecha, calificacion = calif, lugar = lugar, asistentes = asistentes)
#Tema ggplots
tema <- function(){
    theme(text = element_text(family = "Avenir Next", size = 2*18/.pt),
          plot.title = element_text(size = 2*22/.pt,
                                    colour =  "#13384D",
                                    hjust = 0),
          axis.text.y = element_text(color = "#F8737D"),
          axis.text.x = element_text(color = "#13384D"),
          axis.line.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          panel.grid = element_blank()
    )
}


# ## gauge
# 
promedioGauge <- function(bd, calificacion){
  aux <- bd %>% summarise(promedio = round(mean({{calificacion}}, na.rm = T), 1)) %>%
    mutate(color= case_when(promedio>= 6 ~"#2E8087", T ~"#C93446"))
  aux %>%    ggplot() +
    annotate(x=1, xend=1, y=0, yend=10,size=10*1.1, color = aux$color,
             geom = "segment", alpha=.5)+
    geom_segment(aes(x = 1, y = 0, xend = 1, yend = promedio),
                 color = aux$color,
                 lineend = "round", linejoin = "round",
                 size =  10, arrow = arrow(length = unit(0, "inches"))  ) +
    geom_text(aes(label=round(promedio,digits = 1)),color = aux$color,
              x=-5, y=5,size=60/.pt, fontface="bold")+
    coord_polar(theta = "y") +
    scale_x_continuous(limits = c(-5,2)) +
    scale_y_continuous(limits = c(0, 10))+
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank()
    )
}
# promedioGauge(bd, calif)

lineaCalificacion <- function(bd, fecha, calificacion, lugar, asistentes){
  bd <- bd %>%  mutate(lugar =str_to_title({{lugar}}), asistentes={{asistentes}},
                       calificacion = {{calificacion}},
                       fecha_tt =floor_date({{fecha}}, unit = "hour"),
                       fecha = datetime_to_timestamp(fecha_tt)) 
  
  Graph <- bd %>% hchart(hcaes(x = fecha, y  = calificacion), type = "area", color = "#F8737D") %>% 
    hc_yAxis(min = 0, max = 10, title = list(text = "Calificación" , style = list( fontSize = "16px", color = "#41657A")), 
             gridLineWidth =0,
             labels = list(style = list(fontSize = "18px", color = "#41657A"))) %>% 
    hc_xAxis( title = list(text = "Fecha del evento", style = list( fontSize = "16px", color = "#41657A")), type = "datetime",
              labels = list(step = 2,style = list(fontSize = "18px",color = "#13384D")),
              crosshair = list(ebabled= T, color= "#F8737D", dashStyle="shortdash",
                               width= 2, snap = F, zIndex= 5),
              lineWidth =0, tickWidth =0) %>%
    hc_plotOptions(area = list(fillOpacity= .3,
                               fillColor = list(
                                 linearGradient = list(x1 = 0, x2 = 0, y1 = 0, y2 = 1),
                                 stops = list(
                                   c(0, '#F8737D'),
                                   c(1, '#FFF')   ) ),
                               crisp=F, lineWidth = 4, marker = list(radius =0))) %>% 
    hc_title(text = "<b>Calificación  </b>", align = "left", style = list(fontSize = "22px", color = "#13384D")) %>% 
    hc_tooltip(borderWidth =0,shadow = F,
               headerFormat = '<span style="font-size: 20px">{point.key}</span><br/>',
               useHTML = TRUE,
               pointFormat = '<span style="font-size: 18px"> </span></b>Calificación: <b>{point.y}</b><br>Lugar: <b>{point.lugar}</b></b><br>Número de asistentes: <b>{point.asistentes}</b>',
               style = list(fontSize = "16px", color = "#41657A")) %>% 
    hc_chart(style = list(fontColor = "#1C313D", fontFamily= "Avenir Next"))
  return(Graph)
}
# lineaCalificacion(bd, fecha  = fecha, calificacion = calif, lugar = lugar, asistentes = asistentes)

# Radar 
distRadar <- function(bd, pregunta, otro, x, titulo =""){
  
  bd_2 <- bd %>% filter({{ pregunta }} %in% c('Otro'))
  
  bd_1 = count(bd, {{ pregunta }}) %>%
    mutate(n  = round(100*n/sum(n),2)) %>%
    arrange(-n)
  
  nTot <- bd_1 %>%
    select(n) %>%
    mutate(sum = sum(n)) %>%
    select(sum)
  
  nTot <- nTot[1,1]$sum
  
  bd_1 <- bd_1 %>%
    filter(!{{ pregunta }} %in% c('Otro'))
  
  bd_1 <- bd_1 %>% mutate(n  = round(n/100, 2)) %>%
    spread(value = n, key = {{ pregunta }})
  
  bd_2 <- count(bd_2, {{ otro }}) %>%
    mutate(porcentaje  = round(100*n/sum(n), 2)) %>%
    filter(porcentaje > x)
  
  bd_2 <- select(bd_2, -porcentaje) 
  
  bd_2 <- bd_2 %>% mutate(n = round(n/nTot,2)) %>%
    spread(value = n, key = {{ otro }})
  
  if(nrow(bd_1) != nrow(bd_2)){

      Graph <- ggradar(bd_1,  base.size = 15, font.radar = "sans", 
                       values.radar = c("0%", "50%", "100%")) + 
      labs(title = titulo) +
      theme(plot.background = element_rect(fill = "white", color = "white"))
  }else{
  df <- data.frame(bd_1, bd_2)
  
   Graph <- ggradar(df,  base.size = 15, font.radar = "sans",
                    values.radar = c("0%", "50%", "100%")) + 
     labs(title = titulo) +
     # theme(plot.background = element_rect(fill = "white", color = "white")) +
     theme_minimal()+
     theme(plot.background = element_rect(fill = "white", color = "white"),
           panel.grid = element_blank(),
           axis.text.y = element_blank(),
           axis.text.x =element_blank(),
           text = element_text(family = "Avenir Next", size = 20),
           plot.title = element_text(size = 22,
                                     colour =  "#13384D",
                                     hjust = 0, face="bold"),
           axis.line = element_blank(),
           legend.title = element_blank(),
           legend.position = "none" )
   
    }
  return(Graph)
}

# distRadar(bd, pregunta = animo, otro = animo_otro, x = 30, titulo = "Animo de los asistentes")
# Agregar en la función la regla de otro si supera el umbral modificable

barras_animo <- function(DB, pregunta, Otro, x){
  DB <- DB %>% mutate(pregunta_2 = {{ pregunta }},
                      Otro_2 = {{ Otro }})
  
  DB_AUX <- DB %>% filter(pregunta_2 %in% c('Otro'))
  
  frec_1 = count(DB, pregunta_2)
  frec_2 = count(DB_AUX, Otro_2)
  
  frec_1 <- frec_1 %>% 
    mutate(porcentaje = (100*n/sum(n))) %>%
    mutate(label = sprintf("%1.1f%%", porcentaje)) %>%
    arrange(-n)
  
  nTot <- frec_1 %>% 
    select(n) %>% 
    mutate(sum = sum(n)) %>% 
    select(sum)
  
  nTot <- nTot[1,1]$sum
  
  frec_1 <- frec_1 %>% 
    filter(!pregunta_2 %in% c('Otro')) %>% 
    head(4)
  
  frec_2 <- frec_2 %>% 
    mutate(porcentaje = (100*n/sum(n))) %>%
    mutate(label = sprintf("%1.1f%%", porcentaje)) %>%
    mutate(pregunta_2 = Otro_2) %>% 
    select(c(pregunta_2,"n","porcentaje", "label")) %>% 
    arrange(-n) %>%
    filter(porcentaje > x) 
  
  frec_2 <- frec_2 %>% 
    select(c(pregunta_2, n)) %>% 
    mutate(porcentaje = (100*n/nTot)) %>% 
    mutate(label = sprintf("%1.1f%%", porcentaje)) %>% 
    arrange(-n)
  
  frec <- frec_1 %>% 
    union(frec_2) 
  
  frec <- frec %>% 
    mutate(n = n/sum(n))
  
  Graph <- ggplot(frec, aes(x = reorder(pregunta_2, -n), y = n)) +
    scale_y_continuous(labels = scales::percent) + tema_ggplot() +
    geom_bar(fill='#55C1FF', color = "#55C1FF", width = 0.7, alpha = 0.5, stat = "identity") +
    labs(title = "En general, ¿cómo describiría el ánimo de los asistentes?", x = "", y = "") +
    tema_barras_animo()
  
  return(Graph)
}

#Burbujas

burbujas <- function(bd, pregunta1, pregunta2){
  bd <- bd  %>% 
    gather(grupo, resp, c({{pregunta1}}, {{pregunta2}})) %>% 
    group_by(grupo) %>%  count(resp) %>% 
    mutate(n = round(n/sum(n), 2), 
             etiqueta = case_when(resp%in% c("Debían haber sido menos personas",
                                           "Debía de haber durado menos tiempo")~ "Debió haber sido menor",
                                resp%in% c("Debían de haber sido más personas", 
                                           "Debía de haber durado más tiempo")~"Debió haber sido mayor",
                                resp%in% c("Adecuado")~"Adecuado",
           ),
           etiqueta2 = case_when(grupo == "duracion"~"Duración del evento", 
                                 grupo == "asistentes"~"Número de asistentes"
           ),
           color = case_when(etiqueta == "Debió haber sido menor" ~"#B8D8D8",
                             etiqueta == "Debió haber sido mayor" ~"#7A9E9F",
                             etiqueta == "Adecuado" ~"#4F6367",
           ),etiqueta = str_wrap(etiqueta, 15), etiqueta2 = str_wrap(etiqueta2, 15)) 
  
  p<- bd %>% ggplot( aes(x = etiqueta, y = etiqueta2, color = color, size = n) )+
    geom_point(stat = "identity", alpha= .6)+
    scale_color_identity()+ theme_minimal()+
    scale_size_area(max_size = 70)+
    labs(x = "Respuesta", y = "Aspecto", title = str_wrap("Cantidad de Asistentes y Duración de Evento", 30))+
    theme(text = element_text(family = "Avenir Next", size = 20),
          plot.title = element_text(size = 22,
                                    colour =  "#13384D",
                                    hjust = 0, face="bold"),
          axis.text.y = element_text(color = "#41657A"),
          axis.text.x = element_text(color = "#41657A"),
          axis.line.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          panel.grid = element_blank())

  return(p)
}
# burbujas(bd, pregunta1 = asistentes, pregunta2 = duracion)

tema_barras_animo <- function(){
  theme_classic() +
    theme(legend.title = element_blank(), 
          panel.background = element_rect(fill = "#56636B", color = "#56636B"),
          plot.background = element_rect(fill = "#56636B", color = "#56636B"),
          legend.position = "none",
          axis.text.x = element_text(size = 12, colour = "#ECE2D0"),
          axis.text.y = element_text(size = 12, colour = "#ECE2D0"),
          plot.title = element_text(size = 16, color = "#ECE2D0"),
    )
}

barras_animo <- function(DB, pregunta, Otro, x){
  DB <- DB %>% mutate(pregunta_2 = {{ pregunta }},
                      Otro_2 = {{ Otro }})
  
  DB_AUX <- DB %>% filter(pregunta_2 %in% c('Otro'))
  
  frec_1 = count(DB, pregunta_2)
  frec_2 = count(DB_AUX, Otro_2)
  
  frec_1 <- frec_1 %>% 
     mutate(porcentaje = (100*n/sum(n))) %>%
     mutate(label = sprintf("%1.1f%%", porcentaje)) %>%
     arrange(-n)
   
  nTot <- frec_1 %>% 
            select(n) %>% 
              mutate(sum = sum(n)) %>% 
                select(sum)
  
  nTot <- nTot[1,1]$sum
   
  frec_1 <- frec_1 %>% 
     filter(!pregunta_2 %in% c('Otro')) %>% 
     head(4)
  
  frec_2 <- frec_2 %>% 
    mutate(porcentaje = (100*n/sum(n))) %>%
    mutate(label = sprintf("%1.1f%%", porcentaje)) %>%
    mutate(pregunta_2 = Otro_2) %>% 
    select(c(pregunta_2,"n","porcentaje", "label")) %>% 
    arrange(-n) %>%
    filter(porcentaje > x) 
  
  frec_2 <- frec_2 %>% 
              select(c(pregunta_2, n)) %>% 
              mutate(porcentaje = (100*n/nTot)) %>% 
              mutate(label = sprintf("%1.1f%%", porcentaje)) %>% 
              arrange(-n)
  
  frec <- frec_1 %>% 
            union(frec_2) 
  
  frec <- frec %>% 
            mutate(n = n/sum(n))

  Graph <- ggplot(frec, aes(x = reorder(pregunta_2, -n), y = n)) +
                    scale_y_continuous(labels = scales::percent) +
    geom_bar(fill='#55C1FF', color = "#55C1FF", width = 0.7, alpha = 0.5, stat = "identity") +
    labs(title = "En general, ¿cómo describiría el ánimo de los asistentes?", x = "", y = "") +
    tema_barras_animo()
   
  return(Graph)
}

tema_barras_n_asist <- function(){
  theme_classic() +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(), 
          axis.title.x = element_blank(),
          panel.background = element_rect(fill = "#FF687B", color = "#FF687B"),
          plot.background = element_rect(fill = "#FF687B", color = "#FF687B"),
          legend.position = "none",
          axis.text.x = element_text(size = 20, colour = "#FEFFFF"),
          axis.text.y = element_blank(),
          plot.title = element_text(size = 20, color = "#FEFFFF")
    ) 
}

barras_n_assist <- function(DB){
  
  barras_2 = count(DB, considera_num_asist)
  
  barras_2 <- barras_2 %>% mutate(porcentaje = (100*n/sum(n))) %>%
     mutate(label = sprintf("%1.1f%%", porcentaje))
  
  barras_2 <- barras_2 %>%
                arrange(-n) %>% head(4)

  Graph <-ggplot(barras_2, mapping = aes(x = forcats::fct_reorder(considera_num_asist, -n),
                                          y = label, label = forcats::fct_reorder(considera_num_asist, -n))) +
     geom_bar(fill = "#FEFFFF", color = "#FEFFFF", stat = "identity") +
     coord_flip() + tema_barras_n_asist() +
     labs(title = "Número de Asistentes") +
     geom_fit_text(position = "stack", reflow = TRUE, size = 15,
                   color = "#A7A6A6")
  return(Graph)
}

tema_lolipop <- function(){
  theme_classic() +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(), 
          axis.title.x = element_blank(),
          #panel.background = element_rect(fill = "#FF687B", color = "#FF687B"),
          #plot.background = element_rect(fill = "#FF687B", color = "#FF687B"),
          legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(size = 20)
    )
}

lolipop_cRecursos <- function(DB, pregunta){
  cr = count(DB, {{pregunta }})
  cr <- cr %>% mutate(porcentaje = (100*n/sum(n)))%>%
    mutate(labela = sprintf("%1.1f%%", porcentaje)) %>%
    arrange(-n)

  Graph <- ggplot(cr, aes(x = porcentaje, y = reorder({{ pregunta }}, -n), label = labela, color = n, fill = n)) +
    geom_segment(aes(x = 0, y = {{ pregunta }}, xend = porcentaje, yend = {{ pregunta }})) +
    geom_point(size = 20) + tema_lolipop() +
    geom_text(color = "white", size = 8) +
    labs(title = "Calidad de Recursos")
  
  cr <- mutate(labela = {{ pregunta }})
  Graph <- Graph +
  geom_fit_text(position = "stack", reflow = TRUE, size = 15,
                  color = "#A7A6A6")

  return(Graph)
}

paletaRecursos <- function(bd, pregunta, titulo = ""){
  # De la base de datos, sacar porcentaje por pregunta y redondear
  bd <-  bd %>%
    count({{pregunta}}) %>% na.omit() %>%
    mutate(pct = n/sum(n),
           pregunta = gsub({{pregunta}}, pattern = " calidad", replacement = ""),
           colores  = case_when(pregunta  == "Muy buena"~"#0B3954",
                                pregunta  == "Buena"~"#087E8B",
                                pregunta  == "Mala"~"#FF5A5F",
                                pregunta  == "Muy mala"~"#C81D25"),
           pregunta = factor(pregunta, c("Muy buena", "Buena", "Mala", "Muy mala")))
  
  p<-bd %>% ggplot(aes(x= pregunta, y=pct)) +
    geom_segment( aes( xend=pregunta,y=0, yend=pct, color = colores) , size = 3, alpha=.8)+
    geom_point(aes(y = pct,color = colores), size =25, stroke = 2,alpha=.8) +
    # coord_flip()+
    scale_color_identity()+
    geom_text( aes(label = pct %>%  percent(accuracy = 1), y = pct), 
               color = "white", fontface="bold", size = 8) +
    scale_y_continuous(labels=scales::percent,limits = c(0,max(bd$pct) + .1)) +
    labs(title =titulo, x = "", y = "" )+
    geom_hline(yintercept = 0, linetype = "solid", size = .4, color = "#BFD7EA")+
    theme_minimal()+
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x =element_text(size = 20, colour =  "#13384D"),
          text = element_text(family = "Avenir Next", size = 20),
        plot.title = element_text(size = 22,
                                  colour =  "#13384D",
                                  hjust = 0, face="bold"),
        axis.line = element_blank(),
        legend.title = element_blank(),
        legend.position = "none" )

  return(p)
}

llMapaEstado <- function(Estado){
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g representantes de casilla",
    Estado$NOMBRE, Estado$n
  ) %>% lapply(htmltools::HTML)
  
  pal <- colorNumeric("Reds", domain = unique(Estado$n))
  
  Graph <-  leaflet(Estado, options = leafletOptions(minZoom = 8)) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.light",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
      addPolygons(
        fillColor = ~pal(n),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))%>%
        addLegend(pal = pal, values = ~n, opacity = 0.7, title = "No. Representantes",
                  position = "bottomright") 
    
  return(Graph)
}

#llMapaEstado(DB_MichGeograf)

#paletaRecursos(bd, pregunta = calidad, titulo = "Nivel de calidad de los recursos tecnológicos empleados")

# MICH <- st_read("~/GerenciaPoder/Mapa/MUNICIPIO.shp",options = "ENCODING=WINDOWS-1252")
# ggMapaEstado <- function(Estado){
#   Graph <- ggplot(Estado) + geom_sf(fill = "#F2D479", color = "#FFFFF7") + theme_minimal() + 
#            theme(axis.text = element_blank(),panel.grid.major = element_blank(),
#            plot.background = element_rect(fill = "#FFFFF7",linetype = 0))
#   
#   return(Graph)
# }
# 

