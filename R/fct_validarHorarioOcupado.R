validarHorarioOcupado <- function(evt, eventos, editableIndex = 0){
  valido <- TRUE
  for(i in seq_along(eventos)){
    if(editableIndex != i && !is.null(eventos[[i]]$inicioEvento)){
      h1 <- c(eventos[[i]]$inicioEvento, eventos[[i]]$finEvento)
      h2 <- c(evt()$inicioEvento, evt()$finEvento)
      if(h1 %overlaps% h2){
        h1 <- format(as.POSIXct(eventos[[i]]$inicioEvento,format="%H:%M:%S"),"%H")
        m1 = format(as.POSIXct(eventos[[i]]$inicioEvento,format="%H:%M:%S"),"%M")
        
        h2 <- format(as.POSIXct(eventos[[i]]$finEvento,format="%H:%M:%S"),"%H")
        m2 = format(as.POSIXct(eventos[[i]]$finEvento,format="%H:%M:%S"),"%M")
        
        shinyalert::shinyalert(title = "¡Horario Ocupado!", 
                               text = glue::glue("Este horario ya está ocupado por el evento {eventos[[i]]$nombre} en {eventos[[i]]$lugar} de {paste(h1,m1,sep=':')} a {paste(h2,m2,sep=':')}"))
        valido <- FALSE
        break
      } 
    }
  }
  return (valido)
}