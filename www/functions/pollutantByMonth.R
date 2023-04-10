#' @title: pollutantByMonth

library(dplyr)
library(ggplot2)


df <- read.csv("df.csv")


pollutantByMonth <- function(colN) {
  
  df_mean_colN <- df %>%
    group_by(month) %>%
    summarise(mean_colN = mean(!!sym(colN), na.rm = TRUE))
  
  linePlot <- ggplot(df_mean_colN, aes(x = month, y = mean_colN)) +
    geom_line() +
    scale_x_continuous(breaks=c(3,6,9,12), labels=c("Spring", "Summer", "Fall", "Winter")) +
    xlab("Season") +
    ylab("Concentration (micrograms/m^3)") +
    ggtitle(paste("Average Concentration of", colN, "by Month"))
  
  print(linePlot)
} 

