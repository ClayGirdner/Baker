library(ggplot2)

theme_pff <- theme_minimal() + theme(plot.title = element_text(face = "bold"),
                                     axis.title = element_text(face = "bold"),
                                     plot.background = element_rect(fill = "#F0F0F0", color = "#F0F0F0"),
                                     panel.background = element_rect(fill = "#F0F0F0", color = "#F0F0F0"),
                                     panel.border = element_blank(),
                                     panel.grid.major = element_line(colour = "#E3E3E3"),
                                     panel.grid.minor = element_blank(),
                                     legend.position = "none")