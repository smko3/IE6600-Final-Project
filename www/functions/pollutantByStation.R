#' @title: pollutantByStation

library(dplyr)
library(ggplot2)


df <- read.csv("df.csv")

# switch function 
pollutantByStation <- function(colN) {
  
  boxPlot <- ggplot(df, aes(x = station, y= !!sym(colN), fill = station)) + 
    geom_boxplot() +
    xlab("Station") +
    ylab("Concentration (micrograms/m^3)") +
    theme(panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
          panel.grid.minor = element_line(color = "grey90", linetype = "dotted"),
          axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1)) +
    ggtitle(paste("Concentration of", colN, "in Different Station"))
  
  print(boxPlot)
} 