## code to prepare `AWS` dataset goes here
config <- config::get(file = system.file("app/data/config.yml",package = "tElectoral"))
pool <- pool::dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = config$database,
  host = config$server,
  username = config$uid,
  password = config$pwd,
  port = config$port
)

DBI::dbExecute(pool, "CREATE TABLE tElectoralTest_crear_giras (
  idGira INT AUTO_INCREMENT PRIMARY KEY,
  nombre VARCHAR(100),
  lugar VARCHAR(100),
  direccion VARCHAR(200),
  descripcion VARCHAR(500),
  contacto VARCHAR(100),
  telefono VARCHAR(20),
  correo VARCHAR(100),
  fechaEvento DATETIME,
  inicioEvento DATETIME,
  finEvento DATETIME,
  fechaAlta DATETIME,
  fechaEdicion DATETIME,
  usuarioCrea VARCHAR(100),
  usuarioEdicion VARCHAR(100),
  activo TINYINT
);" )

# DBI::dbRemoveTable(pool,"tElectoralTest_crear_giras")
# tbl(pool,"tElectoralTest_crear_giras") %>% select(idGira) %>% filter(idGira == 1)
# sandbox -----------------------------------------------------------------


DBI::dbExecute(pool, "CREATE TABLE tElectoral_prueba_responsables (
  idResponsable INT AUTO_INCREMENT PRIMARY KEY,
  responsable TEXT,
  activa TINYINT,
  usElimina TEXT
);" )

# DBI::dbRemoveTable(pool,"tElectoral_prueba_lugares")
DBI::dbExecute(pool,"CREATE TABLE tElectoral_prueba_lugares (
  idLugar INT AUTO_INCREMENT PRIMARY KEY,
  lugar TEXT,
  activa TINYINT,
  usElimina TEXT
);")

DBI::dbExecute(pool,"CREATE TABLE tElectoral_prueba_giras (
  idGira INT AUTO_INCREMENT PRIMARY KEY,
  idResponsable INT,
  idLugarInicio INT,
  idLugarFin INT,
  fechaInicio DATETIME,
  fechaFIN DATETIME,
  descripcion TEXT,
  activa TINYINT,
  usElimina TEXT
);")

DBI::dbExecute(pool,"CREATE TABLE tElectoral_prueba_eventos (
  idEvento INT AUTO_INCREMENT PRIMARY KEY,
  idGira INT,
  idLugar INT,
  idResponsable INT,
  fechaInicio DATETIME,
  fechaFIN DATETIME,
  descripcion TEXT,
  activa TINYINT,
  usElimina TEXT
);")
# usethis::use_data(AWS, overwrite = TRUE)
