# Hexagonal plots

# geom_hex 

# install.packages('ggstar')
library(ggstar)

theme_hex = function(){
  theme(
    plot.title = element_text(colour = "#000000", face = "bold", size = 13), # Here in bold
    plot.subtitle = element_text(colour = "#000000", size = 13),
    plot.caption = element_text(colour = "#000000", size = 13),
    plot.title.position = "plot",
    panel.background = element_rect(fill = "#FFFFFF"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(colour = "#000000", size = 13, face = 'bold'),
    strip.background = element_blank(),
    legend.title = element_text(colour = "#000000", size = 10, face = "bold"),
    legend.background = element_rect(fill = "#ffffff"),
    legend.key = element_rect(fill = "#ffffff", colour = "#ffffff"),
    legend.text = element_text(colour = "#000000", size = 10),
    legend.position = 'top',
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
    )
}

output_directory <- getwd()

df <- data.frame(x = c(1.5,2.5,3.5,1,2,3,1.5,2.5,3.5),
                 y = c(rep(1.5,3), rep(2, 3), rep(2.5,3)),
                 value = c('8','11%','140','7','26','77%','19%','69%','5%'),
                 indicator_line_1 = c('Per 1,000\nhouseholds\nexperiencing\nhomelessness\n', 'Older people\nliving in\npoverty\n\n','Killed or seriously injured\non roads per\nbillion vehicle miles\n\n', 'Air pollution:\nfine particulate matter\nconcentrations of total\nPM2.5\n', 'Violent offences\nper 1,000 population\n\n\n', 'Working age\nin employment\n\n\n','Children live\nin poverty\n\n\n','Good level of\ndevelopment at\nage 4-5\n\n', 'Pupil absence\n\n\n\n'),
                 compared_england = c('high', 'low', 'low','high', 'same','low','high', 'not_applicable', 'high'))

plot_1 <- ggplot(df,
       aes(x = x,
           y = y)) +
  geom_star(starshape = 6,
            size = 70,
            aes(fill = compared_england)) +
  scale_x_continuous(limits = c(0.5,4)) +
  scale_y_continuous(limits = c(1,3)) +
  scale_fill_manual(values = c('high' = '#C00000', 'low' = '#92D050', 'same' = '#FFC000', 'not_applicable' = 'purple')) +
  geom_text(aes(x - .4,
                y + .1,
                label = value),
            hjust = 0,
            vjust = 0,
            fontface = 'bold',
            size = 7) +
  geom_text(aes(x - .4,
                y - .2,
                label = indicator_line_1),
            hjust = 0,
            vjust = 0) +
  # coord_equal()+
  theme_hex() +
  theme(legend.position = 'none')

svg(filename = paste0(output_directory, '/test_hex_1.svg'),
    width = 9,
    height = 9)  
print(plot_1)
dev.off()
