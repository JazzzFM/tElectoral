leerBd <- function(pool,nombre){
  tbl(pool,nombre) %>% filter(activo == 1) %>% collect()
}

insertBd <- function(pool,nombre, bd){
  DBI::dbWriteTable(pool,nombre,value = bd, append = T)
}
