## Funciones de para grficar en la tab de investigación
tema<-function(){
  fuente<-"Avenir"
  colorB<-"#464F51"
  theme_minimal() %+replace%
    theme(
      # Eje
      axis.ticks.x=element_blank(),
      axis.title.x = element_text(color = colorB),
      axis.text.y = element_text(hjust=0),
      # Panel
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # Leyenda
      legend.position="bottom",
      legend.title = element_blank(),
      # legend.position = "none",
      # Titulo
      plot.title = element_text(hjust =0,colour="#464F51"),
      plot.title.position = "plot",
      #Texto
      text=element_text(size=16,  family=fuente, colour="#464F51"))+
    theme(axis.line.y = element_line(color="black", size = 0),
          axis.line.x = element_line(color="#C7A373", size = 0),
          plot.title = element_text(hjust =0,colour="#464F51", size=20))
}
## 1. Gráfica de intervalos a lo largo del tiempo
# Base de datos falsa
# bd <- tibble(cand1 = rnorm(n = 30, sd = .06, mean = .3),
#              cand2 = rnorm(n = 30, sd = .05, mean = .20),
#              cand3 = rnorm(n = 30, sd = .06, mean = .10),
#              cand4 = rnorm(n = 30, sd = .04, mean = .25),
#              fecha = seq(from = as.Date("2020/12/01"),as.Date("2021/06/25"), by = "week" )) %>%
#   gather(candidato, votacion, cand1:cand4) %>%
#   mutate(min = votacion-rnorm(mean = .03, sd = .01, n =120),
#          max = votacion+rnorm(mean = .03, sd = .01, n =120)) %>%as_tibble()

#plot
tendencia_votos <- function(bd){
  p<-bd %>%  ggplot(aes(x =fecha, y = votacion))+
    geom_ribbon(aes(ymin=min, ymax=max, fill = candidato), alpha =.2)+
    # geom_point(aes(color = candidato))+
    geom_line(aes (color = candidato))+
    scale_y_continuous(labels=scales::percent)+
    labs(title = "Tendencia de votación",
         y = "Votos",
         x = "Fecha")+
    tema()
  return(p)
}


## 2. Chart de bolitas por partido
# bd <-tibble(MORENA = rnorm(n = 1, sd = 6, mean = 30),
#            PAN = rnorm(n = 1, sd = 5, mean = 20),
#            PRI = rnorm(n = 1, sd = 6, mean = 10),
#            MC = rnorm(n = 1, sd = 4, mean = 25)) %>%
#   gather(partido, estimacion )

# 1. creae malla <- expand grid
## REVISAR COMO FUNCINAN LAS LLAVES PARA PONER LA DE ESTIMACIÓN Y PARTIDO
bd_item <- function(bd){
  bd <- bd %>% mutate(estimacion =ceiling( estimacion*100/sum(estimacion)))  %>%
    arrange(desc(estimacion))
  bd <- expand_grid(x =1:10, y = 1:10) %>% 
    # agregar el vector de candidatos que dependa de el numero de candiadtos y n 
    mutate(partido = map2(.x = bd$partido, .y = bd$estimacion, ~rep(x = .x, each = .y) ) %>%
             unlist() %>%  head(100)) 
  return(bd)
}
# 3. plot

grafica_item<- function(bd){
  p<- bd %>%  ggplot(aes(x = x, y = y))+
    geom_point(aes(color = partido), size = 8, alpha = .8)+
    labs(title = "Votación estimada al día de hoy",
         y = "",
         x = "")+
    scale_color_manual(values = c(
      MORENA = "#750528",
      PAN = "#1F3DC2",
      PRI = "#CC1F1F",
      MC = "#FF6A14" ))+
    tema()+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y= element_blank())
  return(p)
}

##Función para hacer la base y la gráfica

item_bd_plot <- function(bd){
  bd <- bd_item(bd)
  p<- grafica_item (bd)
  return(p)
}
