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



  
