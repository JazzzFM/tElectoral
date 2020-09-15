mandatory <- function(input, variables) {
  out <- variables %>% map(~(!is.null(input[[.x]])) && ((input[[.x]] %>% stringr::str_squish()) != "") && 
                             (as.character(input[[.x]]) != 0) 
  ) %>% all
  return(out)
}

mandatoryTime <- function(input, variables){
  out <- variables %>% map(~
                             (strftime(input[[.x]],"%R") != "00:00") 
  ) %>% all
  return(out)
}
