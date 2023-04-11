#' @title: switchMap

library(dplyr)
library(ggplot2)
library(raster)
library(maptools)
library(sf)

df_geo <- read.csv("df_geo.csv")

# Extract Beijing map data
china_map <- readRDS("gadm36_CHN_3_sf.rds")

beijing <- china_map[china_map$NAME_1 == "Beijing", ]

# Beijing map base
beijingMap <- ggplot() +
  geom_sf(data = beijing, fill = "lightgrey", color = "black") +
  labs(x = "Longitude", y = "Latitude")

# switch function 
switchMap <- function(colN, Year, Month, Day, Hour) {
  # Filter data based on input colname and time/date
  
    filtered_data <- df_geo %>% 
    filter(year == Year, month == Month, day == Day, hour == Hour)
  
    col_sym <- as.symbol(colN)
    col_data <- filtered_data[[col_sym]]
  
    # Create plot
   mapPlot <- beijingMap +
      geom_point(data = filtered_data, aes(x = Longitude, y = Latitude, size = log10(col_data), color = station)) +
      scale_size_continuous(name = paste0("log(", colN, ")"), range = c(0.01, 12), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)) +
      labs(y = "Latitude", x = "Longitude") +
      xlim(116, 117) + 
      ylim(39.5, 40.5) +
      coord_sf()
  
   print(mapPlot)
} 

