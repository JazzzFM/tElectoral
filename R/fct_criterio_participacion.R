load("data/DB_Mich.rda")
DB_Mich
# Base de datos temporal: Informacion falsa
# Extraer una muestra aleatoria de tamano 10 de nombres de municipio

fake_visitas <-tibble(NOMBRE_MUNICIPIO=sample(size=10, DB_Mich$NOMBRE_MUNICIPIO))
# Vector de visitas=simulaciones de una poisson
fake_visitas<-fake_visitas %>% mutate(VISITAS=rpois(n=10,lambda = 1)+1)


###################################################
votos_nulos <- select(DB_Mich, contains("NULOS"))
var_nulos <- c(variable.names(votos_nulos)) 

prom_votos <- DB_Mich %>% 
  unite("Promedio de votos", c(variable.names(DB_Mich), -NOMBRE_MUNICIPIO, -var_nulos), mean) 


###################################################

promedio <- DB_Mich %>%
            mutate(PROM = rowMeans(c(variable.names(DB_Mich), -NOMBRE_MUNICIPIO, -var_nulos)))


criterio_participacion <- function(DB_ESTADO, DB_Frec_Vis) {
  # Primero se debe obtener el promedio de votos por municipio
  # Luego, la relevancia del partido
  # Esta funcion nos calcula la diferencia entre la frecuencia relativa 
  # es decir la frecuencia con la que se ha visitado el municipio y el "peso electoral" 
  # Esta función debe ordenar a los municipios en una estructura de manera ordenada
  # Para este primer acercamiento se usa una muestra aleatoria de vectores de poisson
  
  
  return DB_Ordenada
  
}