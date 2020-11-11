validarFormularioDisMuestral <- function(modoLevantamiento, marcoMuestral, numeroEntrevistas,
                                         aleatoria, poliEtapa, estrat, conglo,
                                         nivelPolietap, nivelEstrat, nivelConglo,
                                         unidadMuestral, nivelConfianza, margenError){
  allValido <- TRUE
  mensaje <- ""
  if(!validarVacio(marcoMuestral)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "El campo del marco muestral no debe ser vacío.", sep = "\n")
  }else{
    if(!validarSoloLetras(marcoMuestral)){
      allValido <- FALSE
      mensaje <- paste(mensaje, "El campo del marco muestral no puede contener números o caracteres especiales.", sep = "\n")
    } 
  }
  if(!validarVacio(modoLevantamiento)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "Seleccione una opción para el Levantamiento.", sep = "\n")
  }
  if(!validarVacio(numeroEntrevistas)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "El número de entrevistas no debe ser vacío.", sep = "\n")
  }
  if(!validarVacio(aleatoria)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "Seleccione una opción para Aleatoria.", sep = "\n")
  }
  if(!validarVacio(poliEtapa)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "Seleccione una opción para Poloetápica.", sep = "\n")
  }
  if(!validarVacio(estrat)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "Seleccione una opción para Estratificada.", sep = "\n")
  }
  if(!validarVacio(conglo)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "Seleccione una opción para Conglomerados.", sep = "\n")
  }
  if(!validarVacio(nivelPolietap)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "El nivel de Poloetápica no debe ser vacío.", sep = "\n")
  }
  if(!validarVacio(nivelEstrat)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "El nivel de Estratificada. no debe ser vacío.", sep = "\n")
  }
  if(!validarVacio(nivelConglo)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "El nivel de Conglomerados no debe ser vacío.", sep = "\n")
  }
  if(!validarVacio(unidadMuestral)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "La Unidad muestral no debe ser vacía.", sep = "\n")
  }else{
    if(!validarSoloLetras(unidadMuestral)){
      allValido <- FALSE
      mensaje <- paste(mensaje, "La Unidad muestral muestral no puede contener números o caracteres especiales.", sep = "\n")
    } 
  }
  if(!validarVacio(nivelConfianza)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "El Nivel de confianza no debe ser vacío.", sep = "\n")
  }else{
    if(!validarSoloLetras(nivelConfianza)){
      allValido <- FALSE
      mensaje <- paste(mensaje, "El Nivel de confianza no puede contener números o caracteres especiales.", sep = "\n")
    } 
  }
  if(!validarVacio(margenError)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "El Margen de error no debe ser vacío.", sep = "\n")
  }else{
    if(!validarSoloLetras(margenError)){
      allValido <- FALSE
      mensaje <- paste(mensaje, "El Margen de error no puede contener números o caracteres especiales.", sep = "\n")
    } 
  }
  if(mensaje != ""){
    shinyalert::shinyalert(title = "¡Formulario de diseño muestral no válido!", 
                           text = mensaje)}
  return (allValido)
}