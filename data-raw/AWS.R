## code to prepare `AWS` dataset goes here
config <- config::get(file = "inst/app/data/config.yml")

pool <- pool::dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = config$database,
  host = config$server,
  username = config$uid,
  password = config$pwd,
  port = config$port
)

DBI::dbExecute(pool, "CREATE TABLE tElectoralTest_eventos (
  idEvento INT AUTO_INCREMENT PRIMARY KEY,
  idGira INT,
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

# DBI::dbRemoveTable(pool,"tElectoralTest_giras")
DBI::dbExecute(pool, "CREATE TABLE tElectoralTest_giras (
  idGira INT AUTO_INCREMENT PRIMARY KEY,
  Responsable VARCHAR(100),
  Descripcion TEXT,
  LugarInicio VARCHAR(100),
  FechaInicio DATETIME,
  LugarFinal VARCHAR(100),
  FechaFinal DATETIME,
  fechaAlta DATETIME,
  fechaEdicion DATETIME,
  usuarioCrea VARCHAR(100),
  usuarioEdicion VARCHAR(100),
  activo TINYINT
);" )


DBI::dbExecute(pool, "CREATE TABLE tElectoralTest_evaluacionEventos (
  idEvento INT,
  asistentes VARCHAR(50),
  actitud VARCHAR(50),
  duracion VARCHAR(50),
  calidadTecno VARCHAR(50),
  expectativas INT,
  fechaAlta DATETIME,
  fechaEdicion DATETIME,
  usuarioCrea VARCHAR(100),
  usuarioEdicion VARCHAR(100),
  activo TINYINT
);" )

# tbl(pool,"tElectoralTest_eventos") %>% select(idGira) %>% filter(idGira == 1)
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

#pruebas
DBI::dbExecute(pool, "CREATE TABLE tElectoralTest_investigacion_formularioGeneral (
  idFormGeneral INT AUTO_INCREMENT PRIMARY KEY,
  nombre VARCHAR(100),
  casaEncuestadora VARCHAR(150),
  poblacionObjetivo VARCHAR(175),
  fechaInicio DATETIME,
  fechaFin DATETIME,
  fechaAlta DATETIME,
  fechaEdicion DATETIME,
  usuarioCrea VARCHAR(100),
  usuarioEdicion VARCHAR(100),
  activo TINYINT
);" )

DBI::dbExecute(pool, "CREATE TABLE tElectoralTest_investigacion_disenoMuestral (
  idDMuestral INT AUTO_INCREMENT PRIMARY KEY,
  idFormGeneral INT NOT NULL,
  modoLevantamiento VARCHAR(50),
  marcoMuestral VARCHAR(150),
  numeroEntrevistas INT,
  aleatoria VARCHAR(10),
  poliEtapa VARCHAR(10),
  nivelpoliEtapa INT NULL,
  estratificada VARCHAR(10),
  nivielEstratificada INT NULL,
  conglomerados VARCHAR(10),
  nivielConglomerados INT NULL,
  unidadMuestral VARCHAR(200),
  nivelConfianza VARCHAR(200),
  margenError VARCHAR(200),
  observaciones VARCHAR(500) NULL,
  fechaAlta DATETIME,
  fechaEdicion DATETIME,
  usuarioCrea VARCHAR(100),
  usuarioEdicion VARCHAR(100),
  activo TINYINT,
  CONSTRAINT FK_DisMuestral_FormGeneral FOREIGN KEY (idFormGeneral) REFERENCES tElectoralTest_investigacion_formularioGeneral(idFormGeneral)
);" )
#DBI::dbRemoveTable(pool,"tElectoralTest_investigacion_disenoMuestral")

# usethis::use_data(AWS, overwrite = TRUE)
