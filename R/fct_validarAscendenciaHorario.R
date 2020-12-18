validarAscendenciaHorario <- function(evt, eventos, editableIndex = 0){
  valido <- TRUE
  for(i in seq_along(eventos)){
    if(editableIndex != i && !is.null(eventos[[i]])){
      
      tabla <- eventos[[i]]
      for(x in 1:length(eventos[[i]]$inicioEvento)){ # Se considera a inicioEvento para contar filas
        row <- tabla[x,]
        if(evt()$fechaEvento < row$fechaEvento){
          valido <- FALSE
        }
        if(evt()$inicioEvento < row$inicioEvento){
          valido <- FALSE
        }
        if(evt()$finEvento < row$finEvento){
          valido <- FALSE
        }
      }
      if(!valido){
          shinyalert::shinyalert(title = "¡Fecha menor!",
                                 text = glue::glue("No es posible crear un evento con una fecha menor a la de los eventos anteriores (siendo en este caso el evento {eventos[[i]]$nombre} en {eventos[[i]]$lugar}). Verifique por favor."))
        break
      }
    }
  }
  return (valido)
}