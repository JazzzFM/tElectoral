validarSoloNumeros <- function(x) {
  grepl("\\<[0-9]+\\>", as.character(x), ignore.case=TRUE)
}