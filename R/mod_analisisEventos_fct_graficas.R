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

barras_animo <- function(DB){
  frec = count(DB, animo_asist) 
  frec <- frec %>% mutate(porcentaje = (100*n/sum(n))) %>% mutate(label = sprintf("%1.1f%%", porcentaje))
  frec <- frec %>% arrange(-n) %>% head(4)
  
  Graph <- ggplot(frec, aes(x = reorder(animo_asist, -n), y = label)) + 
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
  barras_2 <- barras_2 %>% mutate(porcentaje = (100*n/sum(n))) %>% mutate(label = sprintf("%1.1f%%", porcentaje))
  barras_2 <- barras_2 %>% arrange(-n) %>% head(4)
  
  Graph <-ggplot(barras_2, mapping = aes(x = forcats::fct_reorder(considera_num_asist, -n),
                                         y = label, label = forcats::fct_reorder(considera_num_asist, -n))) +
    geom_bar(fill = "#FEFFFF", color = "#FEFFFF", stat = "identity") +
    coord_flip() + tema_barras_n_asist() +
    labs(title = "Número de Asistentes") + 
    geom_fit_text(position = "stack", reflow = TRUE, size = 15, 
                  color = "#A7A6A6") 
  return(Graph)
}


## gauge
# bd <- tibble(x = sample(0:10, size = 20, replace = T))
# aux <- bd %>% summarise(promedio = round(mean(x, na.rm = T), 1)) %>% 
#   mutate(color= case_when(promedio>= 6 ~"#2E8087", T ~"#C93446"))
# aux %>%    ggplot() +
#   annotate(x=1, xend=1, y=0, yend=10,size=10*1.1, color = aux$color,
#            geom = "segment", alpha=.5)+
#   geom_segment(aes(x = 1, y = 0, xend = 1, yend = promedio),
#                color = aux$color,
#                lineend = "round", linejoin = "round",
#                size =  10, arrow = arrow(length = unit(0, "inches"))  ) +
#   geom_text(aes(label=round(promedio,digits = 1)),color = aux$color,
#             x=-5, y=5,size=60/.pt, fontface="bold")+
#   coord_polar(theta = "y") +
#   scale_x_continuous(limits = c(-5,2)) +
#   scale_y_continuous(limits = c(0, 10))+
#   theme_minimal()+
#   theme(panel.grid = element_blank(),
#         axis.text = element_blank(),
#         axis.title = element_blank()
#         )

