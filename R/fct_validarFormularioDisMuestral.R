validarFormularioDisMuestral <- function(modoLevantamiento, marcoMuestral, numeroEntrevistas,
                                         aleatoria, poliEtapa, estratificada, conglomerados,
                                         nivelpoliEtapa, nivielEstratificada, nivielConglomerados,
                                         unidadMuestral, nivelConfianza, margenError){
  allValido <- TRUE
  mensaje <- ""
 
  if(!validarVacio(modoLevantamiento)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "Seleccione una opción para el Levantamiento.", sep = "\n")
  }
  if(!validarVacio(marcoMuestral)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "El campo del marco muestral no debe ser vacío.", sep = "\n")
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
  if(!validarVacio(estratificada)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "Seleccione una opción para Estratificada.", sep = "\n")
  }else{
    if(estratificada == "Sí" && nivielEstratificada == 1){
      allValido <- FALSE
      mensaje <- paste(mensaje, "Si es Estratificada el nivel debe ser mayor a uno", sep = "\n")
    }
  }
  if(!validarVacio(conglomerados)){
    allValido <- FALSE
    mensaje <- paste(mensaje, "Seleccione una opción para Conglomerados.", sep = "\n")
  }else{
    if(conglomerados == "Sí" && nivielConglomerados == 1){
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