validarHorarioOcupado <- function(evt, eventos, uiCount, editableIndex = 0){
  valido <- TRUE
  for(i in 1:sum(uiCount$val,-1)){
    if(editableIndex != i && !is.na(eventos[[as.character(i)]]$inicioEvento)){
      h1 <- c(eventos[[as.character(i)]]$inicioEvento, eventos[[as.character(i)]]$finEvento)
      h2 <- c(evt()$inicioEvento, evt()$finEvento)
      if(h1 %overlaps% h2){
        h1 <- format(as.POSIXct(eventos[[as.character(i)]]$inicioEvento,format="%H:%M:%S"),"%H")
        m1 = format(as.POSIXct(eventos[[as.character(i)]]$inicioEvento,format="%H:%M:%S"),"%M")
        
        h2 <- format(as.POSIXct(eventos[[as.character(i)]]$finEvento,format="%H:%M:%S"),"%H")
        m2 = format(as.POSIXct(eventos[[as.character(i)]]$finEvento,format="%H:%M:%S"),"%M")
        
        shinyalert::shinyalert(title = "¡Horario Ocupado!", 
                               text = glue::glue("Este horario ya está ocupado por el evento {eventos[[as.character(i)]]$nombre} en {eventos[[as.character(i)]]$lugar} de {paste(h1,m1,sep=':')} a {paste(h2,m2,sep=':')}"))
        valido <- FALSE
        break
      } 
    }
  }
  return (valido)
}