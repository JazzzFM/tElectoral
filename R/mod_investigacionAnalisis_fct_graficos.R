#Colores
# colores <- tibble(partido = c("INDEPENDIENTE", "MC", "MORENA", "PAN", "PES",
#                               "PRD", "PRI", "PT", "PVEM"),
#                   color = c("#925AAD", "#ED6B40", "#751438", "#17418A", "#54218A",
#                             "#FAB855", "#EB0E0E", "#D63131", "#199121"))


#Preprocesamiento

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
                      min = votacion - var/sqrt(50),
                      max = votacion + var/sqrt(50))
  
  BB <- BB %>% arrange(fecha)
  BB <- BB %>% filter(!candidato %in% c('Aún no sabe',
                                        'Aún no decide',
                                        'Otro', 'No respuesta',
                                        'No declara', 'No votaré',
                                        'No sabe', 'Ns/Nc',
                                        'Indefinidos','Ninguno',
                                        'Anulará su voto',
                                        'No ha tomado una decisión'))
  
  BB <- filter(BB, votacion > 0.08)
  
  BAUX = tibble(candidato = c("PRI", "PAN", "MORENA", "PRD", "PES", "PVEM",
                              "PT", "MC", "INDEPENDIENTE"), 
                colores = c("#00A453", "#00539B", "#600B10", "#FED90E",
                            "#7030A0", "#FD2017", "#00B83A", "#F05606",
                            "#E29578"))
  
  BB <- BB %>% full_join(y = BAUX, by = "candidato")
  
  return(BB)
}

procesamientoFormularios <- function(DB) {
  
  BB <- DB %>% select(fechaAlta, partido, resultado) %>% 
    mutate(fecha = as.Date(fechaAlta),
           candidato = partido,
           voto = as.double(resultado),
           votacion = as.double(resultado)/100) %>%
    group_by(fecha, candidato) %>% 
    summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>% 
    ungroup()
  
  BB <- BB %>% mutate(fecha = ymd(fecha))
  
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

  BB <- BB %>% mutate(min = votacion - var/sqrt(50),
                      max = votacion + var/sqrt(50))

  BB <- BB %>% arrange(fecha)

  BAUX = tibble(candidato = c("PRI", "PAN", "MORENA", "PRD", "PES", "PVEM",
                              "PT", "MC", "INDEPENDIENTE"),
                colores = c("#00A453", "#00539B", "#600B10", "#FED90E",
                            "#7030A0", "#FD2017", "#00B83A", "#F05606",
                            "#E29578"))

  BB <- BB %>% full_join(y = BAUX, by = "candidato") %>% na.omit()
  
  return(BB)
}

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
  theme_minimal() + 
    theme(
      # Ratio
      aspect.ratio = 1,
      # Fondo
      # Texto
      plot.title = element_text(family = "Avenir Next",hjust = .5),
      # Retícula
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Ejes
      #axis.text.y = element_blank(),
      axis.text.x = element_text(size = 40/.pt),
      axis.title = element_blank()
    )
}

# Probabilidad de ganar
probGanar <- function(bd, candidato, nCand){

  pCand <- bd %>% 
    filter(cand == {{candidato}}) %>% 
    pull("prob") 
  
  c <- bd %>% 
    filter(cand == {{candidato}})
  
  bd <- bd %>% filter(cand != {{candidato}}) %>% head(4) %>%
        union(c) %>% data.frame(x = 1:5 )  %>% arrange(prob)
  
  Graph <- ggplot(bd, aes(x = x, y = 0, xend = x, yend = prob, fill = candidato, colour = candidato)) +
    # Marcas
    geom_rect(aes(xmin=0, xmax=nCand+1, ymin=0, ymax=25), alpha = 0.2, fill = "#C5C3C4")+
    geom_rect(aes(xmin=0, xmax=nCand+1, ymin=25, ymax=50), alpha = 0.4, fill = "#C5C3C4")+
    geom_rect(aes(xmin=0, xmax=nCand+1, ymin=50, ymax=75), alpha = 0.6, fill = "#C5C3C4")+
    geom_rect(aes(xmin=0, xmax=nCand+1, ymin=75, ymax=100), alpha = 0.8, fill = "#C5C3C4")+
    scale_fill_manual(values = c("#C5C3C4", "#C5C3C4", "#C5C3C4", "#C5C3C4", "#C5C3C4")) +
    # Indicadores
    #geom_rect(aes(xmin=rw, xmax=rw+.8, ymin=0, ymax=prob, fill=candidato), size=.3,color="white") +
    geom_segment(lineend = "round", linejoin = "round", size = 3, arrow = arrow(length = unit(.0001, "inches"))) +
    scale_color_manual(values = c("#600B10", "#00539B", "#FAB855", "#EB0E0E", "#199121", "#C5C3C4")) +
    coord_polar(theta = "y") +
    geom_text(aes(x=-nCand, y=0, label=scales::percent(pCand/100)), size=8) +
   labs(title = "Probabilidad de triunfo")+
   xlim(c(-nCand,nCand+1))+
   ylim(c(0,100))+
   tema_probGanar() +
    theme(
      text = element_text(family = "Avenir Next", size = 20),
      plot.title = element_text(size = 22,
                                colour =  "#13384D",
                                hjust = 0, face="bold"),
      axis.text.y = element_blank(),
      axis.line.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.title = element_blank(),
      panel.grid = element_blank()
    )
  
  return(Graph)
}

probGanarOld <- function(bd, candidato, nCand){
  
  pCand <- bd %>% 
    filter(cand == {{candidato}}) %>% 
    pull("prob") 
  
  c <- bd %>% 
    filter(cand == {{candidato}})
  
  bd <- bd %>% filter(cand != {{candidato}}) %>% head(4)
  bd <- union(c, bd)
  
  # browser()
  g <- bd %>% 
    ggplot()+
    # Marcas
    geom_rect(aes(xmin=0, xmax=nCand+1, ymin=0, ymax=25), alpha = 0.05, fill = "#C5C3C4")+
    geom_rect(aes(xmin=0, xmax=nCand+1, ymin=25, ymax=50), alpha = 0.10, fill = "#C5C3C4")+
    geom_rect(aes(xmin=0, xmax=nCand+1, ymin=50, ymax=75), alpha = 0.15, fill = "#C5C3C4")+
    geom_rect(aes(xmin=0, xmax=nCand+1, ymin=75, ymax=100), alpha = 0.20, fill = "#C5C3C4")+
    # Indicadores
    geom_rect(aes(xmin=rw, xmax=rw+.8, ymin=0, ymax=prob, fill=candidato), size=.3,color="white") +
    coord_polar(theta = "y")+
    geom_text(aes(x=-nCand, y=0, label=scales::percent(pCand/100)), size=8) +
    scale_fill_manual(values = c("INDEPENDIENTE" = "#925AAD", 
                                 "MC" = "#ED6B40", 
                                 "MORENA" = "#751438",
                                 "PAN"  = "#17418A",
                                 "PES" = "#54218A",
                                 "PRD" = "#FAB855",
                                 "PRI" = "#EB0E0E",
                                 "PT" = "#D63131", 
                                 "PVEM" ="#2F9C37"))+
    labs(title = "Probabilidad de triunfo")+
    xlim(c(-nCand,nCand+1))+
    ylim(c(0,100))+
    tema_probGanar() +
    theme(
      text = element_text(family = "Avenir Next", size = 20),
      plot.title = element_text(size = 22,
                                colour =  "#13384D",
                                hjust = 0, face="bold"),
      axis.text.y = element_blank(),
      axis.line.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.title = element_blank(),
      panel.grid = element_blank()
    )
  return(g)
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
  paleta <- tibble(candidato = c("INDEPENDIENTE", "MC", "MORENA", "PAN", "PES",
                                "PRD", "PRI", "PT", "PVEM"),
                    colores = c("#925AAD", "#ED6B40", "#751438", "#17418A", "#54218A",
                              "#FAB855", "#EB0E0E", "#D63131", "#199121")) %>%  
    arrange(candidato)
  DB <-DB %>% mutate(votacion_r = round(votacion*100),
                     votacion_min = round(min*100),
                     votacion_max = round(max*100),
                     votacion = votacion *100,
                     min = min * 100,
                     max = max * 100) %>% 
    # na.omit() %>% 
    left_join(paleta) 
  # Tooltip
  tt <- tooltip_table(c("{point.series.name}: "),
                      c("{point.votacion_r}%"))
  # browser()
  # Gráfica
  Graph <- DB%>% 
    hchart(hcaes(x = fecha,  low = min, 
                 high = max, group = candidato),
           type = "arearange", enableMouseTracking= F, fillOpacity = 0.15)%>% 
    hc_title(text = "<b>Intención de voto estimada por fecha</b>", align = "left", style = list(fontSize = "22px", color = "#13384D")) %>%
    # hc_subtitle(text = "Data from Different Survey Houses") %>% 
    hc_add_series(data = DB,
                  hcaes(x = fecha, y = votacion,
                        group = candidato),
                  type = "line") %>% 
    hc_colors(colors = paleta$colores) %>% 
    hc_yAxis(title = list(text = "Estimación", style = list( fontSize = "16px", color = "#41657A")), labels = list(format = "{value}%") , style = list(fontSize = "18px",color = "#13384D")) %>%
    hc_xAxis(crosshair = T, 
             labels = list(step = 2,style = list(fontSize = "18px",color = "#13384D")),
             title = list(text = "Fecha", style = list( fontSize = "16px", color = "#41657A"))) %>% 
    hc_plotOptions(line = list(colorByPoint = F, showInLegend = F),
                   arearange = list(lineWidth = 0)) %>% 
    hc_tooltip(sort = F,
               shared = T,
               borderWidth= 0,
               split = T,
               pointFormat = tt, 
               headerFormat = '<span style="font-size: 20px">{point.key}</span><br/>',
               style = list(fontSize = "16px", color = "#41657A"), 
               useHTML = TRUE) %>%
    # hc_add_theme(hc_theme_hcrt()) %>%
    hc_legend(enabled = T) %>% 
    # hc_colors(DB$colores) %>% 
    hc_chart(style = list(fontColor = "#1C313D", fontFamily= "Avenir Next"),zoomType = "x")
  
  
  return(Graph)
}

hPollofPolls2 <- function(DB){
  # Funciones para volver al español
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$weekdays<- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
  hcoptslang$shortMonths <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  hcoptslang$months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  hcoptslang$thousandsSep <- c(",")
  options(highcharter.lang = hcoptslang)
  
  # Formato redondeado
  paleta <- tibble(candidato = c("MORENA", "PAN", "PRD", "PRI"),
                   colores = c("#751438", "#17418A","#ED6B40", "#EB0E0E")) %>%
            arrange(candidato)
  
  DB <- bd %>% mutate(votacion_r = round(votacion*100),
                     votacion_min = round(min*100),
                     votacion_max = round(max*100),
                     votacion = votacion *100,
                     min = min * 100,
                     max = max * 100) %>% 
    # na.omit() %>% 
    left_join(paleta) 
  
  ################################################
  # juntar con procesamiento de formularios
  D <- leerBd(pool, formIntVotoRegistroBd) %>% collect()
  DBf <- procesamientoFormularios(D)
  
  DBf <- DBf %>% mutate(votacion_r = round(votacion*100),
                      votacion_min = round(min*100),
                      votacion_max = round(max*100),
                      votacion = votacion *100,
                      min = min * 100,
                      max = max * 100) %>% 
                      left_join(paleta)
  
  #DB <- union(DB, DBf)
  
  #################################################
  # Tooltip
  tt <- tooltip_table(c("{point.series.name}: "),
                      c("{point.votacion_r}%"))

  # Gráfica
  Graph <- DB %>% 
    hchart(hcaes(x = fecha,  low = 0, 
                 high = votacion, group = candidato),
           type = "arearange", enableMouseTracking= F, fillOpacity = 0.15)%>% 
    hc_title(text = "<b>Intención de voto estimada por fecha</b>", align = "left", style = list(fontSize = "22px", color = "#13384D")) %>%
    # hc_subtitle(text = "Data from Different Survey Houses") %>% 
    hc_add_series(data = DB,
                  hcaes(x = fecha, y = votacion,
                        group = candidato),
                  type = "line") %>% 
    hc_colors(colors = paleta$colores) %>% 
    hc_yAxis(title = list(text = "Estimación", style = list( fontSize = "16px", color = "#41657A")), labels = list(format = "{value}%") , style = list(fontSize = "18px",color = "#13384D")) %>%
    hc_xAxis(crosshair = T, 
             labels = list(step = 2,style = list(fontSize = "18px",color = "#13384D")),
             title = list(text = "Fecha", style = list( fontSize = "16px", color = "#41657A"))) %>% 
    hc_plotOptions(line = list(colorByPoint = F, showInLegend = F),
                   arearange = list(lineWidth = 0)) %>% 
    hc_tooltip(sort = F,
               shared = T,
               borderWidth= 0,
               split = T,
               pointFormat = tt, 
               headerFormat = '<span style="font-size: 20px">{point.key}</span><br/>',
               style = list(fontSize = "16px", color = "#41657A"), 
               useHTML = TRUE) %>%
    # hc_add_theme(hc_theme_hcrt()) %>%
    hc_legend(enabled = T) %>% 
    # hc_colors(DB$colores) %>% 
    hc_chart(style = list(fontColor = "#1C313D", fontFamily= "Avenir Next"),zoomType = "x")
  
  
  return(Graph)
}

iVotoBarras <- function(DB){
  paleta <- tibble(candidato = c("INDEPENDIENTE", "MC", "MORENA", "PAN", "PES",
                                 "PRD", "PRI", "PT", "PVEM"),
                   colores = c("#925AAD", "#ED6B40", "#751438", "#17418A", "#54218A",
                               "#FAB855", "#EB0E0E", "#D63131", "#199121")) %>%  arrange(candidato)
    
    DB <- DB %>% select(candidato, votacion)
  
    DBformulario <- leerBd(pool, formIntVotoRegistroBd) %>%
                    collect() %>% 
                    select(partido, resultado) %>% 
                    mutate(candidato = partido,
                           votacion = as.double(resultado)/100) %>%
                    select(candidato, votacion)
    
     barras <- union(DB, DBformulario)
    
     barras <-  barras %>% group_by(candidato)%>%
     summarise(voto = mean(votacion)*100) %>% 
     mutate(label = sprintf("%1.1f%%", voto)) %>% 
     left_join(paleta) %>% na.omit()
     
     barras <- data.frame(barras %>% arrange(voto), y = 1:5)
   
  Annotations <- data.frame(x = barras %>% select(voto), y = 1:5, barras %>% select(label))
  candidates <- data.frame(barras %>% select(candidato), y = 1:5, x = c(3.0, 0.7, 0.7, 0.7, 1.5))

  Graph <- ggplot(barras, aes(x = 0, y = y, xend = voto, yend = y, fill = colores, colour = colores)) +
              geom_segment(lineend = "round", linejoin = "round", size = 9.5, arrow = arrow(length = unit(.0001, "inches")))  +
              annotate("text", label = Annotations$label, x = Annotations$voto-2.3, y = Annotations$y, size = 6, colour = "white") +
              scale_fill_identity() + theme_minimal() +
              labs(title = "Intención de Voto", subtitle = "(2020)", caption = "", x = "Porcentaje de voto", y = "candidatos") +
              annotate("text", label = candidates$candidato, x = candidates$x, y = candidates$y + 0.3, size = 5, colour = "#8b878d") +
              scale_color_manual(values= c("#17418A", "#751438", "#925AAD", "#EB0E0E", "#FAB855", "#925AAD")) +
              theme(
                axis.title.y = element_blank(),
                axis.title.x = element_text(color = "#8b878d"),
                text = element_text(family = "Avenir Next", size = 20),
                plot.title = element_text(size = 22,
                                colour =  "#13384D",
                                hjust = 0, face = "bold"),
                axis.text.y = element_blank(),
                axis.text.x = element_text(family = "Avenir Next", size = 15),
                axis.line.x = element_blank(),
                panel.grid.major.y = element_blank(),
                legend.title = element_blank(),
                legend.position = "none",
                panel.grid.major.x = element_blank(),
                panel.grid = element_blank())

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
  if(x == 1){
    X_1 <- DB %>% nrow()
    
    annotation <- data.frame(x = c(2), y = c(3),
                             label = paste(X_1, "de Encuestas", sep=' '))
    
    Graph <- ggplot(DB, aes(x = (1:5), y = (1:5))) +
            theme_minimal() +
          theme(panel.background = element_rect(fill = "gray"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank(),
                axis.title.x = element_blank(),
                axis.text = element_blank()) +
          geom_text(data = annotation, aes( x = x, y = y, label = label),
              color = "White", size = 7, angle = 0, fontface = "bold")
       
    return(Graph)
  }
  
  if(x == 2){
    start <- datetime <- ymd_hms(now("GMT"))
    end <- ymd_hms("2021-06-06 5:21:00", tz = "GMT")
    d <- as.numeric(round(end - start)) 
    
    annotation <- data.frame(x = c(2), y = c(3),
                            label = paste(d, " Días para la Elección", sep=' '))
    Graph <-  ggplot(DB, aes(x = (1:5), y = (1:5))) +
      theme_minimal() + 
      theme(panel.background = element_rect(fill = "tomato"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text = element_blank()) + 
      geom_text(data = annotation, aes( x = x, y = y, label = label),
                color = "White", size = 7, angle = 0, fontface = "bold")
  
    return(Graph)
  }
  
  if(x == 3){
    
    v<-select((DB_MichEncuesta), fecha_final)
    v <-tail(v, 1)
    annotation <- data.frame(x = c(2), y = c(3),
                             label = paste("Última Encuesta:", v, sep = ' '))
    
    Graph <- ggplot(DB, aes(x = (1:5), y = (1:5))) +
    theme_minimal() +
      theme(panel.background = element_rect(fill = "brown"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text = element_blank()) + 
      geom_text(data = annotation, aes( x = x, y = y, label = label, weight = 2),
                color = "White", size = 7, angle = 0, fontface = "bold")
    
    return(Graph)
  }
  if(x == 4){
    annotation <- data.frame(x = c(0.32), y = c(0.12),
                             label = c("Probabilidad de Triunfo"))
    
    Graph <- ggplot(DB, aes(x, y)) +
      geom_smooth(color = "white", se = FALSE, size = 1.5) + theme_light() +
      theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text = element_blank()) +
      geom_text(data = annotation, aes( x = x, y = y, label = label),
                color = "White", size = 7, angle = 0, fontface = "bold")

    return(Graph)
  }
}

gglevantamiento <- function(BD) {
  data <- BD %>% select(modoLevantamiento)
  Graph <- ggplot(data, aes(x = modoLevantamiento, fill = modoLevantamiento)) + 
           geom_bar(position = "dodge") + 
           theme_minimal() +
           labs(title = "Modo de Levantamiento") +
           theme(
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            text = element_text(family = "Avenir Next", size = 20),
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
           
  return(Graph)
}
