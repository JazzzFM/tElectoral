procesamiento <- function(column){
  DB <- DBI::dbGetQuery(pool, paste(
        "SELECT",
        as.character(column),
        "FROM tElectoralTest_investigacion_cuestionario;", 
        sep = " "))  %>% 
        mutate(n = 1) 
  
  return(DB)
}

ggClaridadObjetivos <- function(BD){
  DB <- DB %>% 
        group_by(nivelClaridad) %>% 
        summarise(across(n, sum)) %>% 
        ungroup()
  
  n = DB %>% nrow()
  
  barras <- data.frame(DB, y = n:1)
  
  Annotations <- data.frame(DB %>% select(n), y = n:1)
  niveles <- tibble(DB %>% select(nivelClaridad), y = n:1)

  Graph <- ggplot(barras, aes(x = 0, y = y, xend = n, yend = y, fill = nivelClaridad, group = nivelClaridad, color = nivelClaridad)) +
           geom_segment(lineend = "round", linejoin = "round", size = 9.5,
                        arrow = arrow(length = unit(.0001, "inches"))) + 
           scale_fill_brewer(palette="Spectral") +
           annotate("text", hjust = 1, label = Annotations$n, x = Annotations$n, y = Annotations$y, size = 6, colour = "white") +
           theme_minimal() +
           labs(title = "Nivel de Claridad", subtitle = "", caption = "", x = "", y = "") +
           annotate("text", label = niveles$nivelClaridad, vjust = 0, hjust = 0, x = 0, y = niveles$y + 0.3, size = 5, colour = "#8b878d") +
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
            panel.grid = element_blank()
              )
  
  return(Graph)
}

# procesamiento("nivelClaridad") %>%
# ggClaridadObjetivos()

ggOperacionalizacion <- function(BD){
  DB <- DB %>% 
    group_by(nivelClaridad) %>% 
    summarise(across(n, sum)) %>% 
    ungroup()
  
  n = DB %>% nrow()
  
  barras <- data.frame(DB, y = n:1)
  
  Annotations <- data.frame(DB %>% select(n), y = n:1)
  niveles <- tibble(DB %>% select(nivelClaridad), y = n:1)
  
  Graph <- ggplot(barras, aes(x = 0, y = y, xend = n, yend = y, fill = nivelClaridad, group = nivelClaridad, color = nivelClaridad)) +
    geom_segment(lineend = "round", linejoin = "round", size = 9.5,
                 arrow = arrow(length = unit(.0001, "inches"))) + 
    scale_fill_brewer(palette="Spectral") +
    annotate("text", hjust = 1, label = Annotations$n, x = Annotations$n, y = Annotations$y, size = 6, colour = "white") +
    theme_minimal() +
    labs(title = "Nivel de Claridad", subtitle = "", caption = "", x = "", y = "") +
    annotate("text", label = niveles$nivelClaridad, vjust = 0, hjust = 0, x = 0, y = niveles$y + 0.3, size = 5, colour = "#8b878d") +
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
      panel.grid = element_blank()
    )
  
  #tbl(pool, "tElectoralTest_investigacion_cuestionario") %>% collect() %>% select(operacionalizacion) %>% mutate(opera = stri_replace_last(operacionalizacion, fixed = ",", " &"))
  
  return(Graph)
}
