procesamiento <- function(column){
  DB <- DBI::dbGetQuery(pool, paste(
        "SELECT",
        as.character(column),
        "FROM tElectoralTest_investigacion_cuestionario;", 
        sep = " "))
   
  return(DB)
}

ggClaridadObjetivos <- function(DB){
  
  DB <- DB %>% 
        mutate(n = 1) %>% 
        group_by(nivelClaridad) %>% 
        summarise(across(n, sum)) %>% 
        ungroup()
  
  n <- DB %>% nrow()
  
  barras <- data.frame(DB, y = n:1)
  
  Annotations <- data.frame(DB %>% select(n), y = n:1)
  niveles <- tibble(DB %>% select(nivelClaridad), y = n:1)

  Graph <- ggplot(barras, aes(x = 0, y = y, xend = n, yend = y, fill = as.factor(nivelClaridad), group = nivelClaridad, color = as.factor(nivelClaridad))) +
           geom_segment(lineend = "round", linejoin = "round", size = 9.5,
                        arrow = arrow(length = unit(.0001, "inches"))) + 
           scale_color_brewer(palette="Spectral") +
           scale_color_brewer(palette="Spectral") +
           annotate("text", hjust = 1, label = Annotations$n, x = Annotations$n, y = Annotations$y, size = 6, colour = "white") +
           theme_minimal() +
           labs(title = "Nivel de claridad", subtitle = "", caption = "", x = "", y = "") +
           annotate("text", label = niveles$nivelClaridad, vjust = 0, hjust = 0, x = 0, y = niveles$y + 0.3, size = 5, colour = "#8b878d") +
           theme(
            axis.title.y = element_blank(),
            axis.title.x = element_text(color = "#751438"),
            text = element_text(family = "Open Sans", size = 20),
            plot.title = element_text(size = 22,
                                colour =  "#751438",
                                hjust = 0,),
            axis.text.y = element_blank(),
            axis.text.x = element_text( color = "#751438",family = "Avenir Next", size = 15),
            axis.line.x = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.title = element_blank(),
            legend.position = "none",
            panel.grid.major.x = element_blank(),
            panel.grid = element_blank()
              )
  
  return(Graph)
}

#BD <- procesamiento("nivelClaridad") 
#ggClaridadObjetivos(BD)

ggOperacionalizacion <- function(DB){
  
  DB <- DB %>%
    mutate(n = 1) %>% 
    group_by(operacionalizacion) %>%
    summarise(across(n, sum)) %>%
    ungroup()

  DB <- DB %>%
        mutate(opera = case_when(
          operacionalizacion != "No" ~ paste("Sí", sapply(strsplit(operacionalizacion, ", "),"[", 2), sep = " "),
          operacionalizacion == "No" ~ "No"))
  
  n = DB %>% nrow()

  barras <- data.frame(DB, y = n:1)

  Annotations <- data.frame(DB %>% select(n), y = n:1)
  niveles <- tibble(DB %>% select(opera), y = n:1)

  Graph <- ggplot(barras, aes(x = 0, y = y, xend = n, yend = y, fill = as.factor(operacionalizacion), group = operacionalizacion, color = as.factor(operacionalizacion))) +
    geom_segment(lineend = "round", linejoin = "round", size = 9.5,
                 arrow = arrow(length = unit(.0001, "inches"))) +
    scale_color_brewer(palette="Spectral") +
    scale_fill_brewer(palette="Spectral") +
    annotate("text", hjust = 1, label = Annotations$n, x = Annotations$n, y = Annotations$y, size = 6, colour = "white") +
    theme_minimal() +
    labs(title = "Operacionalización", subtitle = "", caption = "", x = "", y = "") +
    annotate("text", label = niveles$opera, vjust = 0, hjust = 0, x = 0, y = niveles$y + 0.3, size = 5, colour = "#8b878d") +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_text(color = "#751438"),
      text = element_text(family = "Open Sans", size = 20),
      plot.title = element_text(size = 22,
                                colour =  "#751438",
                                hjust = 0),
      axis.text.y = element_blank(),
      axis.text.x = element_text(family = "Open Sans", size = 15),
      axis.line.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid = element_blank()
    )

  return(Graph)
}

ggPoblacionObjetivo <- function(DB){
  
  DB <- DB %>%
    mutate(n = 1) %>% 
    group_by(poblacionObjetivo) %>%
    summarise(across(n, sum)) %>%
    ungroup()
  
  DB <- DB %>%
    mutate(opera = case_when(
      poblacionObjetivo != "No" ~ paste("Sí", sapply(strsplit(poblacionObjetivo, ", "),"[", 2), sep = " "),
      poblacionObjetivo == "No" ~ "No"))
  
  n = DB %>% nrow()
  
  barras <- data.frame(DB, y = n:1)
  
  Annotations <- data.frame(DB %>% select(n), y = n:1)
  niveles <- tibble(DB %>% select(opera), y = n:1)
  
  Graph <- ggplot(barras, aes(x = 0, y = y, xend = n, yend = y, fill = poblacionObjetivo , group = poblacionObjetivo, color = poblacionObjetivo)) +
    geom_segment(lineend = "round", linejoin = "round", size = 9.5,
                 arrow = arrow(length = unit(.0001, "inches"))) +
    scale_color_brewer(palette="Spectral") +
    annotate("text", hjust = 1, label = Annotations$n, x = Annotations$n, y = Annotations$y, size = 6, colour = "white") +
    theme_minimal() +
    labs(title = "Población objetivo", subtitle = "", caption = "", x = "", y = "") +
    annotate("text", label = niveles$opera, vjust = 0, hjust = 0, x = 0, y = niveles$y + 0.3, size = 5, colour = "#8b878d") +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_text(color = "#751438"),
      text = element_text(family = "Open Sans", size = 20),
      plot.title = element_text(size = 22,
                                colour =  "#751438",
                                hjust = 0),
      axis.text.y = element_blank(),
      axis.text.x = element_text(family = "Open Sans", size = 15),
      axis.line.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid = element_blank()
    )
  
  return(Graph)
}

#DB <- procesamiento("poblacionObjetivo")
#ggPoblacionObjetivo(DB)

ggCantidadBloquesMin <- function(BD){
  tot <- BD %>% nrow()
  mid <- round(tot/2)
  aux_2 <- BD %>% 
    select(cantidadBloques) %>% min()
  
  aux <- BD %>% 
    select(cantidadBloques) %>% 
    filter(cantidadBloques == aux_2) %>% 
    mutate(color = case_when(cantidadBloques >= mid ~"#C0D294", T ~"#9D1742"))
  
  Graph <- aux %>% ggplot() +
    annotate(x=1, xend=1, y=0, yend=10, size=10*1.1, color = aux$color,
             geom = "segment", alpha= 0.3)+
    geom_segment(aes(x = 1, y = 0, xend = 1, yend = aux_2),
                 color = aux$color,
                 lineend = "round", linejoin = "round",
                 size =  10, arrow = arrow(length = unit(0, "inches"))  ) +
    geom_text(aes(label = paste0(round(cantidadBloques, digits = 1)), "%"), color = aux$color,
              x=-5, y=5, size=60/.pt, fontface="bold")+
    coord_polar(theta = "y") +
    scale_x_continuous(limits = c(-5,2)) +
    scale_y_continuous(limits = c(0, 10))+
    theme_minimal() +
    labs(title = "Cantidad mínima de bloques", caption = "") +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          text = element_text(family = "Open Sans", size = 20),
          plot.title = element_text(size = 22,
                                    colour =  "#751438",
                                    hjust = 0),
          axis.line.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "none"
    )
  
  return(Graph)
}

getmoda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

ggCantidadBloquesModa <- function(BD){
  tot <- BD %>% nrow()
  mid <- round(tot/2)
  aux_2 <- BD  %>% 
    select(cantidadBloques) %>%
    pull(cantidadBloques) %>% 
    getmoda()
  
  aux <- BD %>% 
    select(cantidadBloques) %>% 
    filter(cantidadBloques == aux_2) %>% 
    mutate(color= case_when(cantidadBloques >= mid ~"#C0D294", T ~"#9D1742"))
  
  Graph <- aux %>% ggplot() +
    annotate(x=1, xend=1, y=0, yend=10, size=10*1.1, color = aux$color,
             geom = "segment", alpha=.5)+
    geom_segment(aes(x = 1, y = 0, xend = 1, yend = aux_2),
                 color = aux$color,
                 lineend = "round", linejoin = "round",
                 size =  10, arrow = arrow(length = unit(0, "inches"))  ) +
    geom_text(aes(label = paste0(round(cantidadBloques, digits = 1)), "%"), color = aux$color,
              x=-5, y=5, size=60/.pt, fontface="bold")+
    coord_polar(theta = "y") +
    scale_x_continuous(limits = c(-5,2)) +
    scale_y_continuous(limits = c(0, 10))+
    theme_minimal() +
    labs(title = "Cantidad frecuente de bloques") +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          text = element_text(family = "Open Sans", size = 20),
          plot.title = element_text(size = 22,
                                    colour =  "#751438",
                                    hjust = 0),
          axis.line.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "none"
          
    )
  return(Graph)
}

ggCantidadBloquesMax <- function(BD){
  tot <- BD %>% nrow()
  mid <- round(tot/2)
  
  aux_2 <- BD %>% 
    select(cantidadBloques) %>% max()
  
  aux <- BD %>% 
    select(cantidadBloques) %>% 
    filter(cantidadBloques == aux_2) %>% 
    mutate(color= case_when(cantidadBloques >= mid ~"#C0D294", T ~"#9D1742"))
  
  Graph <- aux %>% ggplot() +
    annotate(x=1, xend=1, y=0, yend=10, size=10*1.1, color = aux$color,
             geom = "segment", alpha=.5)+
    geom_segment(aes(x = 1, y = 0, xend = 1, yend = aux_2),
                 color = aux$color,
                 lineend = "round", linejoin = "round",
                 size =  10, arrow = arrow(length = unit(0, "inches"))  ) +
    geom_text(aes(label = paste0(round(cantidadBloques, digits = 1)), "%"), color = aux$color,
              x=-5, y=5, size=60/.pt, fontface="bold")+
    coord_polar(theta = "y") +
    scale_x_continuous(limits = c(-5,2)) +
    scale_y_continuous(limits = c(0, 10))+
    theme_minimal() +
    labs(title = "Cantidad máxima de bloques") +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          text = element_text(family = "Open Sans", size = 20),
          plot.title = element_text(size = 22,
                                    colour =  "#751438",
                                    hjust = 0),
          axis.line.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "none"
          
    )
  return(Graph)
}
