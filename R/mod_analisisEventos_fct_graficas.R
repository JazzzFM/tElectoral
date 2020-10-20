barras <- function(bd){
  bd +ggplot2(aes(x = x, y= y)) + geom_bar(stat = "identity")
}

## gauge
# bd <- tibble(x = sample(0:10, size = 20, replace = T))
# aux <- bd %>% summarise(promedio = round(mean(x, na.rm = T), 1)) %>% 
#   mutate(color= case_when(promedio>= 6 ~"#2E8087", T ~"#C93446"))
# aux %>%    ggplot() +
#   annotate(x=1, xend=1, y=0, yend=10,size=10*1.1, color = aux$color,
#            geom = "segment", alpha=.5)+
#   geom_segment(aes(x = 1, y = 0, xend = 1, yend = promedio),
#                color = aux$color,
#                lineend = "round", linejoin = "round",
#                size =  10, arrow = arrow(length = unit(0, "inches"))  ) +
#   geom_text(aes(label=round(promedio,digits = 1)),color = aux$color,
#             x=-5, y=5,size=60/.pt, fontface="bold")+
#   coord_polar(theta = "y") +
#   scale_x_continuous(limits = c(-5,2)) +
#   scale_y_continuous(limits = c(0, 10))+
#   theme_minimal()+
#   theme(panel.grid = element_blank(),
#         axis.text = element_blank(),
#         axis.title = element_blank()
#         )

