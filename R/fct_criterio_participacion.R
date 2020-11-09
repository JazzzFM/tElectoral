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

  DB_AUX <- DB_AUX %>% arrange(desc(CRITERIOP))
  # DB_ORDENADA <- select(DB_AUX, c(MUNICIPIO, VISITAS, TOTAL_VOTOS, CRITERIOP))
  
  # antes estaba return(DB_AUX) pero no está ordenada
  return(DB_AUX)
}


criterio_participacion_pri <- function(DB_ESTADO, DB_VISITAS){
  # Calcular la frecuencia relativa de las visitas c
  # DB_VISITAS debe tener mínimo una columna de MUNICIPIO y otra de VISITAS
  # DB_ESTADO debe tener mínimo una columna de MUNICIPIO y otra de TOTAL_VOTOS
  
  VISITAS <- select(DB_VISITAS, c(MUNICIPIO, VISITAS)) %>% 
    mutate(FREC_R_VIS = VISITAS / sum(VISITAS, na.rm = TRUE))
  
  # Base de datos del partido no contando coaliciones
  DB_PRI <- select(DB_Mich,c(MUNICIPIO, contains("PRI"),
                             -MUN_PAN_PRI_PVEM_PANAL_11,-MUN_PRI_PRD_PVEM_CONVERGENCIA_PANAL_11,
                             -PRES_PRI_PVEM_18,-DPL_PRI_PVEM_11,-DPL_PRI_PVEM_15,-GOB_PRI_PVEM_11,
                             -GOB_PRI_PVEM_15,-MUN_PAN_PRI_PVEM_11,-MUN_PAN_PRI_PANAL_11,-MUN_PRI_PVEM_11,
                             -PRES_PRI_PVEM_PANAL_18,-PRES_PRI_PVEM_PANAL_18))
  
  
  DB_PRI_T <- DB_PRI %>% mutate(TOTAL_VOTOS = rowSums(.[grep("PRI", names(.))], na.rm = TRUE), 
                      TOTAL_R_VOTOS = TOTAL_VOTOS/sum(TOTAL_VOTOS)) 
  
  
  # Unir con full join DB_VISITAS y DB_ESTADO, NA->0
  DB_AUX <- DB_PRI_T %>% full_join(y=VISITAS, by="MUNICIPIO") %>% 
    replace_na(list(FREC_R_VIS=0, VISITAS=0))
  
  # Con mutate CP vistas (relativas - total_votos)^2
  
  DB_AUX <- select(DB_AUX, c(MUNICIPIO, TOTAL_R_VOTOS, VISITAS, FREC_R_VIS)) %>%
    mutate(CRITERIOP = (FREC_R_VIS - TOTAL_R_VOTOS)^2)
  
  # # # Arrange CP visitas
  
  DB_AUX <- DB_AUX %>% arrange(desc(CRITERIOP))
  DB_ORDENADA <- select(DB_AUX, c(MUNICIPIO, VISITAS, TOTAL_R_VOTOS, CRITERIOP))
  
  # antes estaba return(DB_AUX) pero no está ordenada
  return(DB_ORDENADA)

  # DB_AUX <- DB_AUX %>% 
  #   arrange(CRITERIOP) %>% 
  #   select(CABECERA_MUNICIPAL,VISITAS, TOTAL_VOTOS, CRITERIOP)
  #  
  # return(DB_AUX)
}

# R<-criterio_participacion(DB_Mich, fake_visitas)
