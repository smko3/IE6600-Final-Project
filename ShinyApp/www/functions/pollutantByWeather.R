#' @title: pollutantByWeather

library(dplyr)
library(ggplot2)

df <- read.csv("df.csv")

df$weather_condition <- ifelse(df$TEMP < 0, "Freezing",
                               ifelse(df$TEMP >= 0 & df$TEMP < 10, "Cold",
                                      ifelse(df$TEMP >= 10 & df$TEMP < 20 & df$RAIN > 0, "Rainy",
                                             ifelse(df$WSPM > 20, "Windy",
                                                    ifelse(df$TEMP >= 20 & df$TEMP < 30, "Warm",
                                                           ifelse(df$TEMP >= 30, "Hot", "Clear"))))))


pollutantByWeather <- function(colN) {
  
  vioPlot <- ggplot(df, aes(x = weather_condition, y = !!sym(colN), fill = weather_condition)) + 
    geom_violin() +
    xlab("Weather Condition") +
    ylab("Concentration (micrograms/m^3)") +
    ggtitle(paste("Concentration of ", colN, "Under Different Weather Condition"))
  
  print(vioPlot)
} 