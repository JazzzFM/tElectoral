## code to prepare `DB_Mich` dataset goes here

# Elecciones Michoacán
library(tidyverse)
library(magrittr)

# Leer bases --------------------------------------------------------------
MICH_DPL_2011 <- read_csv("/home/devel/GerenciaPoder/Michoacán/DB/michoacan_diplocal_2011_casilla.csv")
MICH_DPL_2015 <- read_csv("/home/devel/GerenciaPoder/Michoacán/DB/michoacan_diplocal_2015_seccion.csv")
MICH_GOB_2011 <- read_csv("/home/devel/GerenciaPoder/Michoacán/DB/michoacan_gob_2011_casilla.csv")
MICH_GOB_2015 <- read_csv("/home/devel/GerenciaPoder/Michoacán/DB/michoacan_gob_2015_casilla.csv")
MICH_MUN_2011 <- read_csv("/home/devel/GerenciaPoder/Michoacán/DB/michoacan_municipal_2011_casilla.csv")
MICH_MUN_2015 <- read_csv("/home/devel/GerenciaPoder/Michoacán/DB/michoacan_municipal_2018_casilla.csv")
DPFED_2018 <- read_csv("/home/devel/GerenciaPoder/Michoacán/DB/Diputados_Fed_2018_casilla.csv")
PRES_2018 <- read_csv("/home/devel/GerenciaPoder/Michoacán/DB/Presidencia_2018_casilla.csv")
SENA_2018 <- read_csv("/home/devel/GerenciaPoder/Michoacán/DB/Senador_2018_casilla.csv")
cabecera_mun <- read_csv("/home/devel/GerenciaPoder/Michoacán/DB/cabecera_mun.csv")

BD_1 <- MICH_DPL_2011 %>% 
  group_by(SECCION,NOMBRE_MUNICIPIO) %>% 
  summarise(across(c(where(is.double),-DISTRITO,-NOMBRE_DISTRITO, -MUNICIPIO), sum, .names ="DPL_{col}_11", na.rm=TRUE)) %>% 
  ungroup() 
         
BD_2 <- MICH_DPL_2015 %>% 
  group_by(SECCION,NOMBRE_MUNICIPIO) %>% 
  summarise(across(c(where(is.double),-DISTRITO,-MUNICIPIO,-CIRCUNSCRIPCION, -ESTADO, -NOMBRE_ESTADO), sum, .names ="DPL_{col}_15", na.rm=TRUE)) %>% 
  ungroup()

DPL_MICH <- BD_1 %>% 
  full_join(y = BD_2, by = c("SECCION", "NOMBRE_MUNICIPIO") )

BD_3 <- MICH_GOB_2011 %>% 
  group_by(SECCION, NOMBRE_MUNICIPIO) %>% 
  summarise(across(c(where(is.double), -DISTRITO,-NOMBRE_DISTRITO,-MUNICIPIO,-CASILLA), sum, .names ="GOB_{col}_11", na.rm=TRUE))

BD_4 <- MICH_GOB_2015 %>% 
  group_by(SECCION) %>% 
  summarise(across(c(where(is.double), -DISTRITO,-CASILLA), sum, .names ="GOB_{col}_15", na.rm=TRUE))

GOB_MICH <- BD_3 %>% 
  full_join(y = BD_4, by="SECCION")

DPL_GOB_MICH <- DPL_MICH %>% 
  full_join(y = GOB_MICH, by = c("SECCION", "NOMBRE_MUNICIPIO"))

BD_5 <- MICH_MUN_2011 %>% 
  group_by(SECCION, NOMBRE_MUNICIPIO) %>% 
  summarise(across(c(where(is.double), -DISTRITO,-NOMBRE_DISTRITO,-MUNICIPIO,-CASILLA, -ASIGNACION), sum, .names ="MUN_{col}_11", na.rm=TRUE))

BD_6 <- MICH_MUN_2015 %>% 
  group_by(SECCION, NOMBRE_MUNICIPIO) %>% 
  summarise(across(c(where(is.double), -MUNICIPIO,-CASILLA), sum, .names ="MUN_{col}_15", na.rm=TRUE))

MUN_MICH <- BD_5 %>% 
  full_join(y = BD_6, by = c("SECCION", "NOMBRE_MUNICIPIO"))

DB_Mich_pre <- DPL_GOB_MICH %>% 
  full_join(y = MUN_MICH, by = c("SECCION", "NOMBRE_MUNICIPIO"))


# Se agregan las Bases de Datos Diputados Federales, Presidencia y Senador 2018

MICH_DPF_2018 <- DPFED_2018 %>% 
  filter(NOMBRE_ESTADO=="MICHOACÁN")

BD_7 <- MICH_DPF_2018 %>% 
  group_by(SECCION) %>% 
  summarise(across(c(where(is.double),-EXT_CONTIGUA,-FECHA_HORA,-NUM_ACTA_IMPRESO,-CLAVE_CASILLA,-CLAVE_ACTA, -ESTADO, -NOMBRE_ESTADO, -DISTRITO, -NOMBRE_DISTRITO, -ID_CASILLA, -TIPO_CASILLA), sum, .names ="DPFed_{col}_18", na.rm=TRUE)) %>% 
  ungroup() 

MICH_PRES_2018 <- PRES_2018 %>% 
  filter(NOMBRE_ESTADO=="MICHOACÁN")

BD_8 <- MICH_PRES_2018 %>% 
  group_by(SECCION) %>% 
  summarise(across(c(where(is.double),-MECANISMOS_TRASLADO,-NUM_ACTA_IMPRESO,-FECHA_HORA,-CLAVE_CASILLA,-CLAVE_ACTA, -ESTADO, -NOMBRE_ESTADO, -DISTRITO, -NOMBRE_DISTRITO, -CASILLA_ZONA,-CASILLA,-TIPO_CASILLA, -EXT_CONTIGUA, -OBSERVACIONES), sum, .names ="PRES_{col}_18", na.rm=TRUE)) %>% 
  ungroup() 

FED_PRES <- BD_7 %>% 
  full_join(y = BD_8, by="SECCION")

MICH_SENA_2018 <- SENA_2018 %>% 
  filter(NOMBRE_ESTADO=="MICHOACÁN")

BD_9 <- MICH_SENA_2018 %>% 
  group_by(SECCION) %>% 
  summarise(across(c(where(is.double),-CASILLA_1,-MECANISMOS_TRASLADO,-NUM_ACTA_IMPRESO,-FECHA_HORA,-CLAVE_CASILLA,-CLAVE_ACTA, -ESTADO, -NOMBRE_ESTADO, -DISTRITO, -NOMBRE_DISTRITO,-CASILLA,-TIPO_CASILLA, -EXT_CONTIGUA, -OBSERVACIONES), sum, .names ="SEN_{col}_18", na.rm=TRUE)) %>% 
  ungroup() 

FED_PRES_SEN <- FED_PRES %>% 
  full_join(y = BD_9, by="SECCION")

##########################################################################################


DB_10 <- DB_Mich_pre %>% 
  full_join(y=FED_PRES_SEN, by="SECCION" )

DB_Mich <- DB_10 %>% 
  group_by(NOMBRE_MUNICIPIO) %>% 
  summarise(across(c(where(is.double),-SECCION), sum, na.rm=TRUE)) %>% 
  ungroup()

DB_Mich <- DB_Mich %>% select(-contains("_TOTAL_")) %>% 
  rowwise() %>% 
  mutate(TOTAL_VOTOS=sum(c_across(-starts_with("NOMBRE_MUNICIPIO")))) %>% 
  ungroup() %>% 
  mutate(TOTAL_VOTOS=TOTAL_VOTOS/sum(TOTAL_VOTOS))

DB_Mich <- cabecera_mun %>% 
  full_join(y=DB_Mich,by=c("MUNICIPIO" = "NOMBRE_MUNICIPIO"))

usethis::use_data(DB_Mich, overwrite = TRUE)
