# Temas
tema_intCred <- function(){
  fuente <- "Georgia"   
  # Tema base
  theme_minimal() %+replace%  
    theme(
      # Ejes
      axis.line.x = element_line()
    )
  
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
      axis.text.x = element_text(size = 40/.pt),
      axis.title = element_blank()
    )
  
}

# Probabilidad de ganar
probGanar <- function(bd, candidato, nCand){
  pCand <- bd %>% 
    filter(cand == candidato) %>% 
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

procesamiento_graph <- function(DB){
  
  BB <- select(DB, c("estado","fecha_final","partido","voto"))
  BB <- BB %>% mutate(fecha = fecha_final,
                      candidato = partido) %>%
    group_by(fecha, candidato) %>% 
    summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>% 
    ungroup()
  
  X <- BB %>% group_by(fecha) %>% 
    summarise(across(where(is.numeric), sum, .names ="Tot_{col}", na.rm=TRUE)) %>% 
    ungroup() %>% na.omit()
  
  
  BB <- BB %>% 
    full_join(y = X, by = "fecha")
  
  PROM <- BB %>% group_by(fecha) %>% 
    summarise(across(c(where(is.numeric), -Tot_voto), mean, .names ="prom_r_{col}", na.rm=TRUE)) %>% 
    ungroup() %>% na.omit()
  
  BB <- BB %>% 
    full_join(y = PROM, by = "fecha")
  
  BB <- BB %>% mutate(votacion = voto/Tot_voto,
                      prom_r_voto = prom_r_voto/Tot_voto,
                      sigma = (votacion - prom_r_voto)^2)
  
  Vari <- BB %>% group_by(fecha) %>% 
    summarise(across(sigma, sum, .names ="var", na.rm=TRUE)) %>% 
    ungroup() %>% na.omit()
  
  BB <- BB %>% 
    full_join(y = Vari, by = "fecha")
  
  BB <- BB %>% mutate(fecha = dmy(fecha),
                      min = votacion - var/sqrt(30),
                      max = votacion + var/sqrt(30))
  
  BB <- BB %>% arrange(fecha)
  BB <- BB %>% filter(!candidato %in% c('Aún no sabe',
                                        'Aún no decide',
                                        'Otro', 'No respuesta',
                                        'No declara', 'No votaré',
                                        'No sabe', 'Ns/Nc',
                                        'Indefinidos','Ninguno',
                                        'Anulará su voto',
                                        'No ha tomado una decisión'))
  
  BB <- filter(BB, votacion > 0.10)

  BAUX = tibble(candidato = c("PRI", "PAN", "MORENA", "PRD", "PES", "PVME",
                              "PT", "MC", "INDEPENDIENTE"), 
                colores = c("#00A453", "#00539B", "#600B10", "#FED90E",
                           "#7030A0", "#FD2017", "#00B83A", "#F05606",
                           "#E29578"))
  
  BB <- BB %>% full_join(y = BAUX, by = "candidato")

  return(BB)
}

iVotoBarras <- function(DB){
  barras <- DB %>% group_by(candidato) %>% summarise(voto = mean(votacion)*100)  %>% mutate(label = sprintf("%1.1f%%", voto))
  Graph <- ggplot(barras, mapping = aes(x = forcats::fct_reorder(candidato,voto), y = voto, fill = candidato))+ geom_bar(stat = "identity")+
    coord_flip() + theme_minimal() + labs(title = "Intención de Voto",subtitle = "(2020)",caption = "Data from simulation",
                                          y = "Porcentaje de voto",
                                          x = "candidatos") +
    geom_text(aes(label = label, hjust = 1.2), color = "white")+
    theme(legend.position = "none",  axis.title.y = element_blank()) +
    scale_fill_manual(values=(c("#685369","#849324", "#F7ACCF", "#4E8098", "#767347", "BEA07A", "6F6358", "#A396B4", "#798BA6")))
  
}

hPollofPolls <- function(DB){
  # Funciones para volver al español
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$weekdays<- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
  hcoptslang$shortMonths <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  hcoptslang$months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  hcoptslang$thousandsSep <- c(",")
  options(highcharter.lang = hcoptslang)
  # Formato redondeado
  DB <-DB %>% mutate(votacion_r = round(votacion*100),
                     votacion_min = round(min*100),
                     votacion_max = round(max*100),
                     votacion = votacion*100,
                     min = min * 100,
                     max = max * 100)
  # Tooltip
  tt <- tooltip_table(c("{point.series.name}"),
                      c("{point.votacion_r} %"))
  # Gráfica
  Graph <- DB %>% 
    hchart(hcaes(x = fecha,  low = min,
                 high = max, group = candidato, fill = candidato, color = colores),
                 type = "arearange", fillOpacity = 0.15, 
                 enableMouseTracking = F)%>%
    hc_colors(colores) %>% 
    hc_title(text = "Poll of Polls") %>%
    hc_subtitle(text = "Data from Different Survey Houses") %>% 
    hc_add_series(data = DB,
                  hcaes(x = fecha, y = votacion,
                        group = candidato, fill = candidato, color = colores),
                        type = "line") %>% 
    hc_colors(colores) %>% 
    hc_yAxis(title = list(text = "Porcentaje"), labels = list(format = "{value}%") ) %>%
    hc_xAxis(crosshair = T) %>% 
    hc_plotOptions(line = list(colorByPoint = F, showInLegend = F)) %>% 
    hc_tooltip(sort = F,
               share = F,
               borderWidth = 0,
               split = T,
               pointFormat = tt, 
               useHTML = TRUE) %>%
    hc_add_theme(hc_theme_hcrt()) %>%
     hc_legend(enabled = TRUE)
    
  return(Graph)
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

cajaResume <- function(DB, x){

  Graph <- ggplot(DB, aes(x, y)) +
  geom_smooth(color = "white", se = FALSE, size = 1.5) + theme_light() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_blank())
  
  if(x == 1){
    annotation <- data.frame(
      x = c(0.29),
      y = c(0.12),
      label = c("Número de Encuestas")
    )
        Graph <- Graph +
          theme(panel.background = element_rect(fill = "gray"),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank()) + 
          geom_text(data = annotation, aes( x = x, y = y, label = label, weight = 2),
                     color = "White", size = 8, angle = 0, fontface = "bold")
               #  annotate("text", x = 0.36, y=0.13,
               #           label= "82%",
               #           colour = "White", size = 25) +
               #  annotate("text", x = 0.32, y=0.10,
               #     label = "Probabilidad",
               #     colour = "White", size = 11) +
               # annotate("text", x = 0.33, y=0.09,
               #     label = "De Triunfo",
               #     colour = "White", size = 11) 
    return(Graph)
  }
  
  if(x == 2){
    annotation <- data.frame(
      x = c(0.29),
      y = c(0.12),
      label = c("Días para la Elección")
    )
    Graph <- Graph + 
      theme(panel.background = element_rect(fill = "tomato"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) + 
      geom_text(data = annotation, aes( x = x, y = y, label = label, weight = 2),
                color = "White", size = 8, angle = 0, fontface = "bold") 
      
      # annotate("text", x = 0.36, y=0.13,
      #          label= "22%",
      #          colour = "White", size = 25) +
      # annotate("text", x = 0.32, y=0.115,
      #          label= "21 Municipios",
      #          colour = "White", size = 11) +
      # annotate("text", x = 0.32, y=0.099,
      #          label= "96 Secciones",
      #          colour = "White", size = 11)
      # 
    return(Graph)
  }
  
  if(x == 3){
    annotation <- data.frame(
      x = c(0.29),
      y = c(0.12),
      label = c("Última Encuesta Realizada")
    )
    Graph <- Graph + 
      theme(panel.background = element_rect(fill = "brown"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) + 
      geom_text(data = annotation, aes( x = x, y = y, label = label, weight = 2),
                color = "White", size = 8, angle = 0, fontface = "bold")
      
      # annotate("text", x = 0.36, y=0.14,
      #          label= "22%",
      #          colour = "White", size = 25) +
      # annotate("text", x = 0.34, y=0.105,
      #          label= "21 Municipios",
      #          colour = "White", size = 11) +
      # annotate("text", x = 0.34, y=0.095,
      #          label= "96 Secciones",
      #          colour = "White", size = 11)
    return(Graph)
  }
  if(x == 4){
    Graph <- Graph + 
      theme(panel.background = element_rect(fill = "green"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) + 
      annotate("text", x = 0.36, y=0.14,
               label= "22%",
               colour = "White", size = 25) +
      annotate("text", x = 0.32, y=0.115,
               label= "21 Municipios",
               colour = "White", size = 11) +
      annotate("text", x = 0.32, y=0.109,
               label= "96 Secciones",
               colour = "White", size = 11)

    return(Graph)
  }
}
  
