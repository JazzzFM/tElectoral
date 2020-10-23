## code to prepare `DB_MichEncuesta` dataset goes here
DB_MichEncuesta <- read_csv("~/Desktop/Michoacán_intención_2021.csv")
usethis::use_data(DB_MichEncuesta, overwrite = TRUE)
