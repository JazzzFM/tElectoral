leerBd <- function(pool,nombre){
  out <- tryCatch(
    {
      tbl(pool,nombre) %>% filter(activo == 1) %>% collect()
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

insertBd <- function(pool,nombre, bd){
  out <- tryCatch(
    {
      DBI::dbWriteTable(pool,nombre,value = bd, append = T)
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