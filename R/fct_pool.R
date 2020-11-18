# config <- config::get(file = app_sys("app/data/config.yml"))
# pool <- pool::dbPool(
#   drv = RMariaDB::MariaDB(),
#   dbname = config$database,
#   host = config$server,
#   username = config$uid,
#   password = config$pwd,
#   port = config$port
# )

pool <- pool::dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = "db_gp",
  host = 'mysql.cduzrgqgkfht.us-east-2.rds.amazonaws.com',
  username = 'root',
  password = '9Blw33caY',
  port = 3306
)

onStop(function() {
  pool::poolClose(pool)
})
#gira
eventosBd <- "tElectoralTest_eventos"
girasBd <- "tElectoralTest_giras"
evaluacionEventosBd <- "tElectoralTest_evaluacionEventos"
#investigacion
formGeneralBd <- "tElectoralTest_investigacion_formularioGeneral"
formDisMuestralBd <- "tElectoralTest_investigacion_disenoMuestral"
