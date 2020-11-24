validarFormularioIntVoto <- function(tipoIntVoto, pregunta, noSabeNoContesto, careos){
  allValido <- TRUE
  mensaje <- ""
  if(!validarVacio(tipoIntVoto)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "Debe seleccionar un tipo de intención de voto.", sep = "\n")
  }
  if(!validarVacio(pregunta)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "La pregunta no puede estar vacía.", sep = "\n")
  }
  if(!validarVacio(noSabeNoContesto)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "Debe explicar cómo se contestó la pregunta 'No sabe/No contestó", sep = "\n")
  }
  if(careos <= 0){
    allValido <- FALSE
    mensaje <- paste(mensaje, "La cantidad de careos debe ser mayor a cero", sep = "\n")
  }
  if(mensaje != ""){
    shinyalert::shinyalert(title = "¡Formulario de intención de voto no válido!", 
                           text = mensaje)
  }
  return (allValido)
}