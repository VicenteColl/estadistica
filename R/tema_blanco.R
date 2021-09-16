tema_blanco <- theme(
  panel.background = element_rect(fill = "transparent"), # bg of the panel
  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
  panel.grid.major = element_blank(), # get rid of major grid
  panel.grid.minor = element_blank(), # get rid of minor grid
  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
  legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
  legend.key = element_rect(fill = "transparent", colour = NA), # get rid of key legend fill, and of the surrounding
  axis.line.x = element_line(colour = "black"), # adding a black line for x and y axis
  axis.line.y = element_blank()
)
