validarHorarioOcupado <- function(evt, eventos, editableIndex = 0){
  valido <- TRUE
  for(i in seq_along(eventos)){
    if(editableIndex != i && !is.null(eventos[[i]])){
      tabla <- eventos[[i]]
      for(x in 1:length(eventos[[i]]$inicioEvento)){ # Se considera a inicioEvento para contar filas
        row <- tabla[x,]
        
        h1 <- c(row$inicioEvento, row$finEvento)
        h2 <- c(evt()$inicioEvento, evt()$finEvento)
        if(h1 %overlaps% h2){
          h1 <- format(as.POSIXct(row$inicioEvento,format="%H:%M:%S"),"%H")
          m1 = format(as.POSIXct(row$inicioEvento,format="%H:%M:%S"),"%M")
          
          h2 <- format(as.POSIXct(row$finEvento,format="%H:%M:%S"),"%H")
          m2 = format(as.POSIXct(row$finEvento,format="%H:%M:%S"),"%M")
          
          shinyalert::shinyalert(title = "¡Horario Ocupado!", 
                                 text = glue::glue("Este horario ya está ocupado por el evento {row$nombre} en {row$lugar} de {paste(h1,m1,sep=':')} a {paste(h2,m2,sep=':')}"))
          valido <- FALSE
          break
        }
      }
      if(!valido){
        break
      }
    }
  }
  return (valido)
}