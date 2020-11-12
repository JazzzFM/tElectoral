config <- config::get(file = app_sys("app/data/config.yml"))
pool <- pool::dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = config$database,
  host = config$server,
  username = config$uid,
  password = config$pwd,
  port = config$port
)

onStop(function() {
  pool::poolClose(pool)
})

eventosBd <- "tElectoralTest_eventos"
girasBd <- "tElectoralTest_giras"
evaluacionEventosBd <- "tElectoralTest_evaluacionEventos"
