load("data/DB_Mich.rda")
DB_Mich

# Se crea una base de datos temporal: Informacion falsa (la intención es que se actualice con datos reales)
# Extraer una muestra aleatoria de tamaño 10 de nombres de municipio
# Vector de visitas=simulaciones de una poisson

fake_visitas <-tibble(MUNICIPIO=sample(size=10, DB_Mich$MUNICIPIO))
fake_visitas <-fake_visitas %>% mutate(VISITAS=rpois(n=10,lambda = 1)+1)
  
criterio_participacion <- function(DB_ESTADO, DB_VISITAS){
  # Calcular la frecuencia relativa de las visitas c
  # DB_VISITAS debe tener mínimo una columna de MUNICIPIO y otra de VISITAS
  # DB_ESTADO debe tener mínimo una columna de MUNICIPIO y otra de TOTAL_VOTOS
  
  VISITAS <- select(DB_VISITAS, c(MUNICIPIO, VISITAS)) %>% 
    mutate(FREC_R_VIS = VISITAS / sum(VISITAS, na.rm = TRUE))
  
  # Unir con full join DB_VISITAS y DB_ESTADO, NA->0
  DB_AUX <- DB_ESTADO %>% full_join(y=VISITAS, by="MUNICIPIO") %>% 
    replace_na(list(FREC_R_VIS=0))
   
  # Con mutate CP vistas (relativas - total_votos)^2
    
  DB_AUX <- select(DB_AUX, c(MUNICIPIO, TOTAL_VOTOS, FREC_R_VIS )) %>%
    mutate(CRITERIOP = (FREC_R_VIS - TOTAL_VOTOS)^2)
   
  # # # Arrange CP visitas

  DB_AUX <- DB_AUX %>% arrange(desc(CRITERIOP))
  DB_ORDENADA <- select(DB_AUX, c(MUNICIPIO, TOTAL_VOTOS, CRITERIOP))
   
  return(DB_AUX)
}

R<-criterio_participacion(DB_Mich, fake_visitas)
