validarAscendenciaHorario <- function(evt, eventos, uiCount, editableIndex = 0){
  valido <- TRUE
  for(i in 1:sum(uiCount$val,-1)){
    if(editableIndex != i && !is.na(eventos[[as.character(i)]]$inicioEvento)){
      browser()
      if(evt()$fechaEvento < eventos[[as.character(i)]]$fechaEvento){
        valido <- FALSE
      }
      if(evt()$inicioEvento < eventos[[as.character(i)]]$inicioEvento){
        valido <- FALSE
      }
      if(evt()$finEvento < eventos[[as.character(i)]]$finEvento){
        valido <- FALSE
      }
      
      if(!valido){
          shinyalert::shinyalert(title = "¡Fecha menor!",
                                 text = glue::glue("No es posible crear un evento con una fecha menor a la de los eventos anteriores (siendo en este caso el evento {eventos[[as.character(i)]]$nombre} en {eventos[[as.character(i)]]$lugar}). Verifique por favor."))
        break
      }
    }
  }
  return (valido)
}