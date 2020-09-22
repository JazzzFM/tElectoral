## code to prepare `DB_Mich` dataset goes here

# Elecciones Michoacán
library(tidyverse)
library(magrittr)
library(here)
# Leer bases --------------------------------------------------------------

MICH_DPL_2011 <- read_csv(here("DB/michoacan_diplocal_2011_casilla.csv"))
MICH_DPL_2015 <- read_csv(here("DB/michoacan_diplocal_2015_seccion.csv"))
MICH_GOB_2011 <- read_csv(here("DB/michoacan_gob_2011_casilla.csv"))
MICH_GOB_2015 <- read_csv(here("DB/michoacan_gob_2015_casilla.csv"))
MICH_MUN_2011 <- read_csv(here("DB/michoacan_municipal_2011_casilla.csv"))
MICH_MUN_2015 <- read_csv(here("DB/michoacan_municipal_2018_casilla.csv"))



BD_1 <- MICH_DPL_2011 %>% 
  group_by(NOMBRE_MUNICIPIO,SECCION) %>% 
  summarise(across(c(where(is.double),-DISTRITO,-NOMBRE_DISTRITO), sum, .names ="DPL_{col}_11", na.rm=TRUE)) %>% 
  ungroup()

BD_2 <- MICH_DPL_2015 %>% 
  group_by(NOMBRE_MUNICIPIO,SECCION) %>% 
  summarise(across(c(where(is.double),-DISTRITO,-MUNICIPIO,-CIRCUNSCRIPCION, -ESTADO, -NOMBRE_ESTADO), sum, .names ="DPL_{col}_15", na.rm=TRUE)) %>% 
  ungroup()

BD_3 <- NL_DPL_2018 %>% 
  group_by(SECCION) %>% 
  summarise(across(where(is.double), sum, .names ="DPL_{col}_18", na.rm=TRUE))

DPL1_2 <- BD_1 %>% 
  left_join(y = BD_2, by="SECCION")

DPL <- DPL1_2 %>% 
  left_join(y = BD_3, by="SECCION")


GOB <- NL_GOB_2015 %>% 
  group_by(MUNICIPIO,SECCION) %>% 
  summarise(across(where(is.double), sum, .names ="GOB_{col}_15", na.rm=TRUE)) %>% 
  ungroup()


DPLYGOB <- DPL %>% 
  full_join(y =GOB, by="SECCION") %>% 
  full_join(y=NL_ )

# full join se van a juntar ambas secciones aunque en una no esté otra


BD_4 <- NL_MUN_2012 %>% 
  group_by(SECCION) %>% 
  summarise(across(where(is.double), sum, .names ="MUN_{col}_12", na.rm=F))

BD_5 <- NL_MUN_2015 %>% 
  group_by(SECCION) %>% 
  summarise(across(where(is.double), sum, .names ="MUN_{col}_15", na.rm=FALSE))

BD_6 <- NL_MUN_2018 %>% 
  group_by(SECCION) %>% 
  summarise(across(where(is.double), sum, .names ="MUN_{col}_18", na.rm=FALSE))

BD_7 <- NL_MUN_2018E %>% 
  group_by(SECCION) %>% 
  summarise(across(where(is.double), sum, .names ="MUNext_{col}_18", na.rm=FALSE))

MUN4_5 <- BD_4 %>% 
  left_join(y = BD_5, by="SECCION")

MUN6_7 <- BD_6 %>% 
  left_join(y = BD_7, by="SECCION")

MUN <- MUN4_5 %>% 
  left_join(y = MUN6_7, by="SECCION")


DB_NL <- DPLYGOB %>% 
  left_join(y =MUN, by="SECCION")

DB_Mich <- #La base de datos final 
  
usethis::use_data(DB_Mich, overwrite = TRUE)
