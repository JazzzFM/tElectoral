validarFormularioGeneral <- function(nombre, encuesta, objetivo, fechaInicio, fechaFin){
  allValido <- TRUE
  mensaje <- ""
  if(!validarVacio(nombre)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "El nombre de la encuesta no puede estar vacío.", sep = "\n")
  }
  if(!validarVacio(encuesta)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "La casa encuestadora no puede estar vacía.", sep = "\n")
  }else{
    if(!validarSoloLetras(encuesta)){
      allValido <- FALSE
      mensaje <- paste(mensaje, "La casa encuestadora no puede contener números o caracteres especiales.", sep = "\n")
    } 
  }
  if(!validarVacio(objetivo)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "El objetivo no puede estar vacío.", sep = "\n")
  }
  if(!validarVacio(as.character(fechaInicio))){
    allValido <- FALSE
    mensaje <- paste(mensaje, "Debe escoger una fecha de inicio para el formulario", sep = "\n")
  }
  if(!validarVacio(as.character(fechaFin))){
    allValido <- FALSE
    mensaje <- paste(mensaje, "Debe escoger una fecha de finalización para el formulario", sep = "\n")
  }
  
  if(validarVacio(as.character(fechaInicio)) && validarVacio(as.character(fechaFin))){
    if(fechaFin <= fechaInicio){
      allValido <- FALSE
      mensaje <- paste(mensaje, "La fecha final del formulario general no puede ser menor o igual a la fecha de inicio", sep = "\n")
    } 
  }
  if(mensaje != ""){
    shinyalert::shinyalert(title = "¡Formulario general no válido!", 
                           text = mensaje)
  }
  return (allValido)
}