validarLugarAnterior <- function(eventos, index){
  valido <- TRUE
  if(is.null(eventos[[sum(index,-1)]])){
    
    shinyalert::shinyalert(title = "¡Atención!", 
                           text = glue::glue("Debe agregar eventos al lugar anterior. No es posible agregar eventos dejando lugares anteriores a este vacíos."))
    valido <- FALSE
  }
  return (valido)
}