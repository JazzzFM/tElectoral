barras <- function(bd){
  bd +ggplot2(aes(x = x, y= y)) + geom_bar(stat = "identity")
}