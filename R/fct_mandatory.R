mandatory <- function(input, variables) {
  out <- variables %>% purrr::map(~(!is.null(input[[.x]])) && ((input[[.x]] %>% stringr::str_squish()) != "") && (as.character(input[[.x]]) != 0)
  ) %>% all
  return(out)
}