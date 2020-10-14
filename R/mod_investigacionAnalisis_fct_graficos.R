# Tema

tema_intCred <- function(){
  fuente <- "Georgia"   
  # Tema base
  theme_minimal() %+replace%  
    theme(
      # Ejes
      axis.line.x = element_line()
    )
  
}


# Probabilidad de ganar
probGanar <- function(bd, candidato){
  pCand <- bd %>% 
    filter(cand==candidato) %>% 
    pull("prob")
  
  g <- bd %>% 
    ggplot()+
    # Marcas
    geom_rect(aes(xmin=0, xmax=nCand+1, ymin=0, ymax=25),alpha=.8, fill="tomato")+
    geom_rect(aes(xmin=0, xmax=nCand+1, ymin=25, ymax=50),alpha=.5, fill="#ffadad")+
    geom_rect(aes(xmin=0, xmax=nCand+1, ymin=50, ymax=75),alpha=.5, fill="#ADECFF")+
    geom_rect(aes(xmin=0, xmax=nCand+1, ymin=75, ymax=100),alpha=.8, fill="#0081A7")+
    # Indicadores
    geom_rect(aes(xmin=rw, xmax=rw+.8, ymin=0, ymax=prob, fill=cand),size=.3,color="white") +
    geom_text(aes(x=-nCand,y=0, label=scales::percent(pCand/100)), size=10)+
    coord_polar(theta = "y")+
    labs(title = "Probabilidad de triunfo")+
    xlim(c(-nCand,nCand+1))+
    ylim(c(0,100))+
    tema_probGanar()
  return(g)
  
}

tema_probGanar <- function(){
  fuente <- "Georgia"   
  # Tema base
  theme_minimal() %+replace%  
    theme(
      # Ratio
      aspect.ratio = 1,
      # Fondo
      # Texto
      plot.title = element_text(family = fuente,hjust = .5),
      # Retícula
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Ejes
      axis.text.y = element_blank(),
      axis.title = element_blank()
    )
  
}

iVotoBarras <- function(DB){
  
  barras <- DB %>% group_by(candidato) %>% summarise(voto = mean(votacion)*100)  %>% mutate(label = sprintf("%1.1f%%", voto))
  Graph <- ggplot(barras, mapping = aes(x = forcats::fct_reorder(candidato,voto), y = voto, fill = candidato))+ geom_bar(stat = "identity")+
    coord_flip() + theme_minimal() + labs(title = "Intención de Voto",subtitle = "(2020)",caption = "Data from simulation",
                                          y = "Porcentaje de voto",
                                          x = "candidatos") +
    geom_text(aes(label = label, hjust = 1.2), color = "white")+
    theme(legend.position = "none",  axis.title.y = element_blank()) +
    scale_fill_manual(values=(c("#685369","#849324", "#F7ACCF", "#4E8098")))
  
  return(Graph)
}


hPollofPolls <- function(DB){
  
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$weekdays<- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
  hcoptslang$shortMonths <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  hcoptslang$months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  hcoptslang$thousandsSep <- c(",")
  options(highcharter.lang = hcoptslang)
  
  DB <-DB %>% mutate(votacion_r = round(votacion*100),
                votacion_min = round(min*100),
                votacion_max = round(max*100),
                votacion = votacion *100,
                min = min * 100,
                max = max * 100)
  
  tt <- tooltip_table(c("", "estamacion","Votacion min", "Votacion max"),
                      c("{point.series.name}", "{point.votacion_r}%", "{point.votacion_min}%", "{point.votacion_max}%"))
  
  Graph <- DB%>% hchart(hcaes(x = fecha,  low = min,
                              high = max, group = candidato),
                              type = "arearange")%>% 
    hc_title(text = "Poll of Polls") %>%
    hc_subtitle(text = "Data from Simulation") %>% 
    hc_add_series(data = DB,
                  hcaes(x = fecha, y = votacion,
                        group = candidato),
                        type = "line") %>% 
    hc_yAxis(title = list(text = "Porcentaje"), labels = list(format = "{value}%") ) %>%
    hc_xAxis(crosshair = T) %>% 
    hc_plotOptions(line = list(colorByPoint = F, showInLegend = F)) %>% 
    hc_tooltip(pointFormat = tt, useHTML = TRUE) %>%
    hc_add_theme(hc_theme_hcrt()) %>%
    hc_legend(enabled = TRUE) %>% 
    hc_colors(c("#685369","#849324", "#F7ACCF", "#4E8098"))
  
  return(Graph)
}

hVotoPopu <- function(DB){
  Graph <- ggplot(bd, aes(votacion, fill = candidato, colour = candidato)) +
    geom_density(alpha = 0.9, na.rm = TRUE, ) + theme_minimal() + 
    facet_wrap(~candidato, nrow = 2) + 
    scale_x_continuous(labels = scales::percent) +
    labs(title = "Voto Popular", subtitle = "(2020)", caption = "Data from simulation") +
    theme(legend.position = "none",  axis.title.y = element_blank()) +
    scale_fill_manual(values=(c("#685369","#849324", "#F7ACCF", "#4E8098"))) +
    scale_color_manual(values=c("#685369","#849324", "#F7ACCF", "#4E8098"))

  return(Graph)
}

cajaResume <- function(title2){

  Graph <- ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth() +
  theme(title = {title2},
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

return(Graph)
}
  
