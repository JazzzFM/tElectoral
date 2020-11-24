validarAlcanceTiempo <- function(evt,eventos,tiempoAntes, lugarAntes, lugar){
  valido <- TRUE
  fechaAnterior <- glue::glue("1985-01-01 00:30") %>% ymd_hm()
  horaFinalAnterior <- glue::glue("1985-01-01 00:30") %>% ymd_hm()
  for(i in seq_along(eventos)){
    if(!is.null(eventos[[i]]$inicioEvento)){
      tmp <- eventos[[i]]
      
      if(tmp$lugar == lugarAntes){
        # Se reasigna fecha mayor
        if(tmp$fechaEvento >= fechaAnterior){
          fechaAnterior <- tmp$fechaEvento
        }
        # Se reasigna hora final mayor
        if(tmp$finEvento >= horaFinalAnterior){
          horaFinalAnterior <- tmp$finEvento
        }
      } 
    }
  }
  # Obtener horario positivo
  tiempoPositivo <- NULL
  
  if(horaFinalAnterior < evt()$inicioEvento)
    tiempoPositivo <- (evt()$inicioEvento - horaFinalAnterior)
  else
    tiempoPositivo <- (horaFinalAnterior - evt()$inicioEvento )
  
  if(tiempoPositivo < hm(glue::glue("00:{floor(tiempoAntes)}"))){
    valido <- FALSE
  }
  return (c(valido, as.numeric(tiempoPositivo, units = "mins")))
}