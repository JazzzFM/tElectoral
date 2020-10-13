library(highcharter)

# Load Database 
# bd <- tibble(cand1 = rnorm(n = 30, sd = .06, mean = .3),
#             cand2 = rnorm(n = 30, sd = .05, mean = .20),
#             cand3 = rnorm(n = 30, sd = .06, mean = .10),
#             cand4 = rnorm(n = 30, sd = .04, mean = .25),
#             fecha = seq(from = as.Date("2020/12/01"),as.Date("2021/06/25"), by = "week" )) %>%
#      gather(candidato, votacion, cand1:cand4) %>%
#      mutate(min = votacion-rnorm(mean = .03, sd = .01, n =120),
#             max = votacion+rnorm(mean = .03, sd = .01, n =120))

Barras_Intencion <- function(DB){
  
  barras <- DB %>% group_by(candidato) %>% summarise(voto = mean(votacion)*100)  %>% mutate(label = sprintf("%1.1f%%", voto))
  
  
  Graph <- ggplot(barras, mapping = aes(x = fct_reorder(candidato,voto), y = voto, fill = candidato))+ geom_bar(stat = "identity")+
    coord_flip() + theme_minimal() + labs(title = "Intención de Voto",subtitle = "(2020)",caption = "Data from simulation",
                                          y = "Porcentaje de voto",
                                          x = "candidatos") +
    geom_text(aes(label = label, hjust = 1.2), color = "white")+
    theme(legend.position = "none",  axis.title.y = element_blank())
  
  return(Graph)
}

#H <- Barras_Intencion(bd)

HC_PollOfPolls <- function(DB){
  polls <- DB%>% mutate(porcentaje_votacion=votacion*100)
  data <- select(polls, c(fecha, candidato, porcentaje_votacion))  
  
  Graph <- hchart(data, "line", hcaes(x = fecha, y = porcentaje_votacion, group = candidato, color = candidato))%>%
    hc_exporting(enabled = TRUE) %>% 
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 2) %>%
    hc_title(text="Poll of Polls", align="center") %>%
    hc_subtitle(text="Data from simulation",align="center")
  
  return(Graph)
}

    #HC_PollOfPolls(bd)