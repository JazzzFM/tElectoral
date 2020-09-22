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

BD_1 <- MICH_DPL_2011 %>% 
  group_by(SECCION,NOMBRE_MUNICIPIO) %>% 
  summarise(across(c(where(is.double),-DISTRITO,-NOMBRE_DISTRITO), sum, .names ="DPL_{col}_11", na.rm=TRUE)) %>% 
  ungroup()

BD_2 <- MICH_DPL_2015 %>% 
  group_by(SECCION,NOMBRE_MUNICIPIO) %>% 
  summarise(across(c(where(is.double),-DISTRITO,-MUNICIPIO,-CIRCUNSCRIPCION, -ESTADO, -NOMBRE_ESTADO), sum, .names ="DPL_{col}_15", na.rm=TRUE)) %>% 
  ungroup()

DPL_MICH <- BD_1 %>% 
  full_join(y = BD_2, by="SECCION")


BD_3 <- MICH_GOB_2011 %>% 
  group_by(SECCION, NOMBRE_MUNICIPIO) %>% 
  summarise(across(c(where(is.double), -DISTRITO,-NOMBRE_DISTRITO,-MUNICIPIO,-CASILLA), sum, .names ="GOB_{col}_11", na.rm=TRUE))

BD_4 <- MICH_GOB_2015 %>% 
  group_by(SECCION) %>% 
  summarise(across(c(where(is.double), -DISTRITO,-CASILLA), sum, .names ="GOB_{col}_15", na.rm=TRUE))

GOB_MICH <- BD_3 %>% 
  full_join(y = BD_4, by="SECCION")

DPL_GOB_MICH <- DPL_MICH %>% 
  full_join(y = GOB_MICH, by="SECCION")


BD_5 <- MICH_MUN_2011 %>% 
  group_by(SECCION, NOMBRE_MUNICIPIO) %>% 
  summarise(across(c(where(is.double), -DISTRITO,-NOMBRE_DISTRITO,-MUNICIPIO,-CASILLA, -ASIGNACION), sum, .names ="MUN_{col}_11", na.rm=TRUE))

BD_6 <- MICH_MUN_2015 %>% 
  group_by(SECCION, NOMBRE_MUNICIPIO) %>% 
  summarise(across(c(where(is.double), -MUNICIPIO,-CASILLA), sum, .names ="MUN_{col}_15", na.rm=TRUE))

MUN_MICH <- BD_5 %>% 
  full_join(y = BD_6, by="SECCION")

DB_Mich <- DPL_GOB_MICH %>% 
  full_join(y = MUN_MICH, by="SECCION")
  
usethis::use_data(DB_Mich, overwrite = TRUE)
