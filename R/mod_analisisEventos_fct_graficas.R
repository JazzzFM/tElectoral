# Eventos , municipios, incidentes-> datos 
# rep grafica facets para bubble chart

## gauge
# bd <- tibble(x = sample(0:10, size = 20, replace = T))

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
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
        )
}

## linea
# fecha sea con hora
# bd <- tibble(fecha = seq(from = today(), to = today()+90, by = 10 ),
#              calif = sample(6:10, 10, replace = T),
#        lugares = sample( c("Apodaca", "Cadereyta Jiménez", "El Carmen", "García",
#                          "San Pedro Garza García", "General Escobedo", "Monterrey"),
#                        10, replace = T),
#        personas = sample(500:3000, 10))

lineaCalificacion <- function(bd, fecha, calificacion, lugar, asistentes){
bd <- bd %>%  mutate(lugar = {{lugar}}, asistentes={{asistentes}},
                     fecha = {{fecha}}, calificacion = {{calificacion}}) 
p<- bd %>% hchart(hcaes(x = fecha, y  = calificacion), type = "area", color = "#F8737D") %>% 
  hc_yAxis(min = 0, max = 10, title = list(text = "Calificación"), 
           gridLineWidth =0,
           labels = list(style = list(fontSize = "15px", color = "#F8737D"))) %>% 
  hc_xAxis( title = list(text = "Fecha del evento"),
            labels = list(step = 2,style = list(fontSize = "15px", color = "#43515C")),
            crosshair = list(ebabled= T, color= "#F8737D", dashStyle="shortdash", width= 2, snap = F),
            lineWidth =0, tickWidth =0) %>%
  hc_plotOptions(area = list(fillOpacity= .3,
                             fillColor = list(
                               linearGradient = list(x1 = 0, x2 = 0, y1 = 0, y2 = 1),
                               stops = list(
                                 c(0, '#F8737D'),
                                 c(1, '#FFF')   ) ),
                             crisp=F, lineWidth = 5, marker = list(radius =0))) %>% 
  hc_title(text = "Calificación  ", align = "right", style = list(fontSize = "20px", color = "#0C4147")) %>% 
  hc_tooltip(borderWidth =0,shadow = F,
             headerFormat = '<span style="font-size: 18px">{point.key}</span><br/>',
    pointFormat = '</b><br>Calificación: <b>{point.y}</b><br>Total de asistentes: <b>{point.lugar}</b></b><br>Total de asistentes: <b>{point.asistentes}</b>',
    style = list(fontSize = "15px", color = "#14373B"))
return(p)
}
 # lineaCalificacion(bd , fecha = fecha, calificacion = calif, lugar = lugares, asistentes = personas)       


# Radar 
# Agregar en la función la regla de otro si supera el umbral modificable
# bd <- tibble(animo = sample(c("Interesados", "Participativos" , "Emocionados",
#                               "Desesperados", "Molestos", "Aburridos", "Otro"),
#                             size = 100, replace = T,
#                             prob = c(.4,.2, .1, .05, .05,.05, .1) ),
#              otro = sample(c("Tristes", "Angustiados", "Ruidosos"),
#                             size = 100, replace = T,
#                             prob = c(.7, .3, .01) ))

distRadar <- function(bd, pregunta, n, titulo =""){
bd %>%  count({{pregunta}}) %>%  mutate(n  = round(n/sum(n),2)) %>% 
  spread(value = n, key = {{pregunta}}) %>% 
  ggradar()+ 
    labs(title = titulo)+
    theme(plot.background = element_rect(fill = "white", color = "white"))
}

# distRadar(bd, pregunta = animo)


# Burbujas
# bd <- tibble(asistentes =sample(c("Debían de haber sido más personas", "Adecuado", "Debían haber sido menos personas"),
#                                   prob = c(.3, .7, .2), size = 150, replace= T ),
#              recursos =sample(c("Muy buena calidad","Buena calidad", "Mala calidad" , "Muy mala calidad"),
#                                 prob = c(.5, .7, .3, .2), size = 150, replace= T ),
#              tiempo =sample(c("Debía de haber durado menos tiempo", "Adecuado", "Debía de haber durado más tiempo"),
#                                 prob = c(.3, .7, .2), size = 150, replace= T ) )
# 
# 
# aux <- bd  %>% 
#   gather(grupo, resp, c(asistentes, tiempo)) %>% 
#   group_by(grupo) %>%  count(resp) %>% 
#  mutate(n = round(n/sum(n), 2), 
#          etiqueta = case_when(resp%in% c("Debían haber sido menos personas",
#                                       "Debía de haber durado menos tiempo")~ "Debió haber sido menor",
#                            resp%in% c("Debían de haber sido más personas", 
#                                       "Debía de haber durado más tiempo")~"Debió haber sido mayor",
#                            resp%in% c("Adecuado")~"Adecuado",
#                            ),
#         etiqueta2 = case_when(grupo == "tiempo"~"Duración del evento", 
#                               grupo == "asistentes"~"Número de asistentes"
#         ),
#         color = case_when(etiqueta == "Debió haber sido menor" ~"#EB6A8A",
#                           etiqueta == "Debió haber sido mayor" ~"#18658C",
#                           etiqueta == "Adecuado" ~"#3C908B",
#                           )) 
# 
# aux %>% ggplot( aes(x = etiqueta, y = etiqueta2, color = color, size = n) )+
#   geom_point(stat = "identity", alpha= .7)+
#     scale_color_identity()+ theme_minimal()+
#   scale_size_area(max_size = 40)+
#   labs(x = "Respuesta", y = "Pregunta", title = "")+
#   theme(legend.position = "none",
#         panel.grid = element_blank())
#     
#     
# 
