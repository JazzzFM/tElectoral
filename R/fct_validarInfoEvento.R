validarInfoEvento <- function(evt){
  allValido <- TRUE
  mensaje <- ""
  if(!validarVacio(evt$nombre)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "El nombre no puede estar vacío.", sep = "\n")
  }else{
    if(!validarSoloLetras(evt$nombre)){
      allValido <- FALSE
      mensaje <- paste(mensaje, "El nombre no puede contener números o caracteres especiales.", sep = "\n")
    } 
  }
  if(!validarVacio(evt$direccion)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "La dirección no puede estar vacía.", sep = "\n")
  }
  if(!validarVacio(evt$descripcion)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "La descripción no puede estar vacía.", sep = "\n")
  }
  if(!validarVacio(evt$contacto)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "El nombre de contacto no puede estar vacío.", sep = "\n")
  }
  if(!validarVacio(evt$telefono)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "El número de teléfono no puede estar vacío.", sep = "\n")
  }else{
    if(!validarSoloNumeros(evt$telefono)){
      allValido <- FALSE
      mensaje <- paste(mensaje, "El número de teléfono debe tener solo números.", sep = "\n")
    }
  }
  if(!validarVacio(evt$correo)){
      allValido <- FALSE
      mensaje <- paste(mensaje, "El correo no puede estar vacío.", sep = "\n")
  }else{
    if(!validarEmail(evt$correo)){
      allValido <- FALSE
      mensaje <- paste(mensaje, "El correo no tiene un formato válido. Asegúrese de que su correo es del tipo ejemplo@dominio.com.", sep = "\n")
    } 
  }
  if(!validarVacio(as.character(evt$fechaEvento))){
    allValido <- FALSE
    mensaje <- paste(mensaje, "Debe escoger una fecha para el evento", sep = "\n")
  }
  if(!validarVacio(as.character(evt$inicioEvento))){
    allValido <- FALSE
    mensaje <- paste(mensaje, "Debe escoger una hora de inicio para el evento", sep = "\n")
  }
  if(!validarVacio(as.character(evt$finEvento))){
    allValido <- FALSE
    mensaje <- paste(mensaje, "Debe escoger una hora para finalizar el evento", sep = "\n")
  }
  if(validarVacio(as.character(evt$inicioEvento)) && validarVacio(as.character(evt$finEvento))){
    if(evt$finEvento < evt$inicioEvento){
      allValido <- FALSE
      mensaje <- paste(mensaje, "La hora final del evento no puede ser menor a la hora de inicio", sep = "\n")
    } 
  }
  if(mensaje != ""){
    shinyalert::shinyalert(title = "¡Evento no válido!", 
                           text = mensaje)
  }
  return (allValido)
}