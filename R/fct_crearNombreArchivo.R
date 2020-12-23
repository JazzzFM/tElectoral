crearNombreArchivo <- function(nombreActual, valoresAConcatenar, putNameAtLast = F){
  allValido <- TRUE
  mensaje <- ""
  newName <- ""
  
  if(str_count(nombreActual, "\\.") > 1){
    newName <- NULL
    allValido <- FALSE
    mensaje <- paste(mensaje, "El nombre contiene caracteres inválidos. Renombre su archivo e inténtelo de nuevo.", sep = "\n")
  }
  
  if(allValido){
    # Obtener extensión
    datosNombre <- as.list(strsplit(nombreActual, '\\.')[[1]]) # posición 1 es nombre, posición 2 es extensión
    
    for(x in 1:length(valoresAConcatenar)){
      validValue <- ""
      
      # Si es tiempo
      if(str_count(valoresAConcatenar[x], ":")){
        validValue <- str_replace_all(valoresAConcatenar[x], " ", "")
        validValue <- str_replace_all(validValue, ":", "-")
      }
      else{
        validValue <- valoresAConcatenar[x]
      }
      if(x == 1)
        newName <- glue::glue("{validValue}")
      else
        newName <- glue::glue("{newName}-{validValue}")
    }
    
    if(putNameAtLast)
      newName <- glue::glue("{newName}-{datosNombre[1]}.{datosNombre[2]}")
    else
      newName <- glue::glue("{datosNombre[1]}-{newName}.{datosNombre[2]}")
  }
  
  
  if(mensaje != ""){
    shinyalert::shinyalert(title = "¡Archivo inválido!", 
                           text = mensaje)
  }
  return (newName)
}