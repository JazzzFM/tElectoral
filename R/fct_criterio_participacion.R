# load("data/DB_Mich.rda")


# Se crea una base de datos temporal: Informacion falsa (la intención es que se actualice con datos reales)
# Extraer una muestra aleatoria de tamaño 10 de nombres de municipio
# Vector de visitas=simulaciones de una poisson

criterio_participacion <- function(DB_ESTADO, DB_VISITAS,n){

  # Calcular la frecuencia relativa de las visitas c
  # DB_VISITAS debe tener mínimo una columna de MUNICIPIO y otra de VISITAS
  # DB_ESTADO debe tener mínimo una columna de MUNICIPIO y otra de TOTAL_VOTOS
  

  VISITAS <- select(DB_VISITAS, c(CABECERA_MUNICIPAL, VISITAS)) %>% 
    mutate(FREC_R_VIS = VISITAS / sum(VISITAS, na.rm = TRUE))
  
  # Unir con full join DB_VISITAS y DB_ESTADO, NA->0
  DB_AUX <- DB_ESTADO %>% full_join(y=VISITAS, by="CABECERA_MUNICIPAL") %>% 
    replace_na(list(VISITAS=0,FREC_R_VIS=0))
   
  # Con mutate CP vistas (relativas - total_votos)^2
    
  DB_AUX <- select(DB_AUX, c(CABECERA_MUNICIPAL, TOTAL_VOTOS, FREC_R_VIS, VISITAS)) %>%
    mutate(CRITERIOP = ((2*FREC_R_VIS - 2*TOTAL_VOTOS)*(n+1)+1)/(n+1)^2,
           TOTAL_VOTOS=scales::percent(round(TOTAL_VOTOS,2)))
   
  # # # Arrange CP visitas

  DB_AUX <- DB_AUX %>% 
    arrange(CRITERIOP) %>% 
    select(CABECERA_MUNICIPAL,VISITAS, TOTAL_VOTOS, CRITERIOP)
   
  return(DB_AUX)
}

# R<-criterio_participacion(DB_Mich, fake_visitas)

