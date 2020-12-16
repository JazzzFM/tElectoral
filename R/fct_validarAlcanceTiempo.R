validarAlcanceTiempo <- function(evt,eventos,tiempoAntes, lugarAntes, lugar){
  browser()
  valido <- TRUE
  fechaAnterior <- glue::glue("1985-01-01 00:30") %>% ymd_hm()
  horaFinalAnterior <- glue::glue("1985-01-01 00:30") %>% ymd_hm()
  for(i in seq_along(eventos)){
    if(!is.null(eventos[[i]])){
      tabla <- eventos[[i]]
      for(x in 1:length(eventos[[i]]$inicioEvento)){ # Se considera a inicioEvento para contar filas
        row <- tabla[x,]
        if(row$lugar == lugarAntes){
          # Se reasigna fecha mayor
          if(row$fechaEvento >= fechaAnterior){
            fechaAnterior <- row$fechaEvento
          }
          # Se reasigna hora final mayor
          if(row$finEvento >= horaFinalAnterior){
            horaFinalAnterior <- row$finEvento
          }
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