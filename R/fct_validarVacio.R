validarVacio <- function(value, validarCero = FALSE){
  valido <- TRUE
  if(is.null(value) || is.na(value) || value == "")
    valido <- FALSE
  if(validarCero)
    if(value == 0)
      valido <- FALSE
  return (valido)
}