leerBd <- function(pool,nombre){
  out <- tryCatch(
    {
      tbl(pool,nombre) %>% filter(activo == 1)
    },
    error=function(ex) {
      shinyalert::shinyalert(title = "¡Ha ocurrido algo!",
                             text = glue::glue("Ha ocurrido un error al leer la base de datos: ${ex}. Inténtelo nuevamente, y si el error persiste, comuníquese con soporte técnico."))
      return(ex)
    },
    warning=function(ex) {
      return(ex)
    }
  )    
  return(out)
}

insertBd <- function(pool,nombre, bd, showMessage = T){
  out <- tryCatch(
    {
      DBI::dbWriteTable(pool,nombre,value = bd, append = T)
      if(showMessage){
        shinyalert::shinyalert(title = "¡Operación exitosa!",
                               text = glue::glue("Los datos se guardaron correctamente.")) 
      }
    },
    error=function(ex) {
      shinyalert::shinyalert(title = "¡Ha ocurrido algo!",
                             text = glue::glue("Ha ocurrido un error al inertar en la base de datos: ${ex}. Inténtelo nuevamente, y si el error persiste, comuníquese con soporte técnico."))
      return(ex)
    },
    warning=function(ex) {
      return(ex)
    }
  )    
  return(out)
}
disableBd <- function(pool,nombre, condition, showMessage = T){
  out <- tryCatch(
    {
      state <- glue::glue("UPDATE {nombre} SET activo = '0' WHERE {condition}")
      DBI::dbExecute(conn = pool, statement = state)
      if(showMessage){
        shinyalert::shinyalert(title = "¡Eliminación exitosa!",
                               text = glue::glue("Se ha eliminado la encuesta de intención de voto correctamente.")) 
      }
    },
    error=function(ex) {
      shinyalert::shinyalert(title = "¡Ha ocurrido algo!",
                             text = glue::glue("Ha ocurrido un error al eliminar en la base de datos: ${ex}. Inténtelo nuevamente, y si el error persiste, comuníquese con soporte técnico."))
      return(ex)
    },
    warning=function(ex) {
      return(ex)
    }
  )    
  return(out)
}