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
  numEventos INT,
  visitasPrioritarias INT,
  incidentes INT,
  asistentes VARCHAR(50),
  duracion VARCHAR(50),
  actitud VARCHAR(50),
  actitudOtro VARCHAR(50),
  calidadTecno VARCHAR(50),
  expectativas INT,
  fechaAlta DATETIME,
  fechaEdicion DATETIME,
  usuarioCrea VARCHAR(100),
  usuarioEdicion VARCHAR(100),
  activo TINYINT
);" )
#DBI::dbRemoveTable(pool,"tElectoralTest_evaluacionEventos")

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

## Investigacion
#Candidatos y Colores Oficiales

DBI::dbExecute(pool, "CREATE TABLE IF NOT EXISTS tElectoralTest_partidoCandidato (
  idPartido INTEGER UNSIGNED PRIMARY KEY AUTO_INCREMENT,
  nombrePartido VARCHAR(50) NOT NULL,
  nombreCandidato VARCHAR(50) NOT NULL  
  );")

# DBI::dbExecute(pool, "INSERT INTO `tElectoralTest_partidoCandidato` VALUES (1, 'PRI', 'Juan'),
# (2, 'PAN', 'Juan'),
# (3, 'PRD', 'Pedro'),
# (4, 'MORENA', 'Angel'),
# (5, 'PT', 'Gerardo'),
# (6, 'PVEM', 'Emilio'),
# (7, 'MC', 'Jesús'),
# (8, 'PES', 'Esteban'),
# (9, 'INDEPENDIENTE', 'Oscar');")
# tbl(pool,"tElectoralTest_partidoCandidato") %>% collect()
#DBI::dbRemoveTable(pool, "partidoCandidato")


DBI::dbExecute(pool, "CREATE TABLE IF NOT EXISTS tElectoralTest_coloresOficiales(
  idColor INTEGER UNSIGNED PRIMARY KEY AUTO_INCREMENT,
  idPartido INTEGER UNSIGNED NOT NULL REFERENCES partidoCandidato(idPartido),
  colorHex VARCHAR(50),
  colorRgb VARCHAR(50),
  opacity VARCHAR(50)
);")

# DBI::dbExecute(pool, "INSERT INTO `tElectoralTest_coloresOficiales` VALUES (1, 1,'#EB0E0E','rgb(235, 14, 14)',''),
# (2, 2,'#17418A','rgb(23, 65, 138)',''),
# (3, 3,'#FAB855','rgb(250, 184, 85)',''),
# (4, 4,'#751438','rgb(117, 20, 56)',''),
# (5, 5,'#D63131','rgb(214, 49, 49)',''),
# (6, 6,'#199121','rgb(25, 145, 33)',''),
# (7, 7,'#ED6B40','rgb(237, 107, 64)',''),
# (8, 8,'#54218A','rgb(84, 33, 138)',''),
# (9, 9,'#AD9B9A','rgb(173, 155, 154)','');")
# tbl(pool,"tElectoralTest_coloresOficiales") %>% collect()
#DBI::dbRemoveTable(pool,"tElectoralTest_coloresOficiales")

# DBI::dbGetQuery(pool,"SELECT p.idPartido, p.nombrePartido, p.nombreCandidato, c.colorHex
#                       FROM tElectoralTest_partidoCandidato as p
#                       JOIN tElectoralTest_coloresOficiales c
#                       ON p.idPartido = c.idPartido;")

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
#DBI::dbRemoveTable(pool,"tElectoralTest_investigacion_formularioGeneral")

DBI::dbExecute(pool, "CREATE TABLE tElectoralTest_investigacion_intencionVoto (
  idIntencionVoto INT AUTO_INCREMENT PRIMARY KEY,
  idFormGeneral INT NOT NULL,
  tipoIntencionVoto VARCHAR(25),
  fechaEncuesta DATETIME, 
  pregunta VARCHAR(250),
  siNoExplicacion VARCHAR(250),
  fechaAlta DATETIME,
  fechaEdicion DATETIME,
  usuarioCrea VARCHAR(100),
  usuarioEdicion VARCHAR(100),
  activo TINYINT,
  CONSTRAINT FK_intencionVoto_FormGeneral FOREIGN KEY (idFormGeneral) REFERENCES tElectoralTest_investigacion_formularioGeneral(idFormGeneral)
);" )
#DBI::dbRemoveTable(pool,"tElectoralTest_investigacion_intencionVoto")

DBI::dbExecute(pool, "CREATE TABLE IF NOT EXISTS tElectoralTest_investigacion_intencionVotoRegistro (
  idIntencionVotoRegistro INT AUTO_INCREMENT PRIMARY KEY,
  idIntencionVoto INT NOT NULL,
  candidato VARCHAR(25),
  partido VARCHAR(250),
  resultado VARCHAR(250),
  fechaAlta DATETIME,
  fechaEdicion DATETIME,
  usuarioCrea VARCHAR(100),
  usuarioEdicion VARCHAR(100),
  activo TINYINT,
  CONSTRAINT FK_intencionVotoRegistro_intencionVoto FOREIGN KEY (idIntencionVoto) REFERENCES tElectoralTest_investigacion_intencionVoto(idIntencionVoto)
);")
#DBI::dbRemoveTable(pool,"tElectoralTest_investigacion_intencionVotoRegistro")

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
