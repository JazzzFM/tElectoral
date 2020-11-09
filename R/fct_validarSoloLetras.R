validarSoloLetras <- function(x) {
  grepl("\\<[A-Z]+\\>", as.character(x), ignore.case=TRUE)
}