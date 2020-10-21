
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
                             fillcolor = list(
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
# bd <- tibble(animo = sample(c("Interesados", "Participativos" , "Emocionados",
#                               "Desesperados", "Molestos", "Aburridos", "Otro"),
#                             size = 100, replace = T,
#                             prob = c(.4,.2, .1, .05, .05,.05,.05) ))

distRadar <- function(bd, pregunta, n, titulo =""){
bd %>%  count({{pregunta}}) %>%  mutate(n  = round(n/sum(n),2)) %>% 
  spread(value = n, key = {{pregunta}}) %>% 
  ggradar()+ 
    labs(title = titulo)+
    theme(plot.background = element_rect(fill = "white", color = "white"))
}


