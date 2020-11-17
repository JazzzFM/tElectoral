validarFormularioDisMuestral <- function(fechaRegistro, modoLevantamiento, marcoMuestral, numeroEntrevistas,
                                         aleatoria, poliEtapa, estrat, conglo,
                                         nivelpoliEtapa, nivelEstrat, nivelConglo,
                                         unidadMuestral, nivelConfianza, margenError, observ){
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
    mensaje <- paste(mensaje, "Seleccione una opción para Polietápica.", sep = "\n")
  }else{
    if(poliEtapa == "Sí" && nivelpoliEtapa == 1){
      allValido <- FALSE
      mensaje <- paste(mensaje, "Si es Polietápica el nivel debe ser mayor a uno", sep = "\n")
    }
  }
  if(!validarVacio(estrat)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "Seleccione una opción para Estratificada.", sep = "\n")
  }else{
    if(estrat == "Sí" && nivelEstrat == 1){
      allValido <- FALSE
      mensaje <- paste(mensaje, "Si es Estratificada el nivel debe ser mayor a uno", sep = "\n")
    }
  }
  if(!validarVacio(conglo)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "Seleccione una opción para Conglomerados.", sep = "\n")
  }else{
    if(conglo == "Sí" && nivelConglo == 1){
      allValido <- FALSE
      mensaje <- paste(mensaje, "Si es Conglomerada el nivel debe ser mayor a uno", sep = "\n")
    }
  }
  if(!validarVacio(unidadMuestral)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "La Unidad muestral no debe ser vacía.", sep = "\n")
  }
  if(!validarVacio(nivelConfianza)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "El Nivel de confianza no debe ser vacío.", sep = "\n")
  }
  if(!validarVacio(margenError)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "El Margen de error no debe ser vacío.", sep = "\n")
  }
  if(mensaje != ""){
    shinyalert::shinyalert(title = "¡Formulario de diseño muestral no válido!", 
                           text = mensaje)}
  return (allValido)
}