
library(tibble)
library(scales) # For converting RGB to HEX
library(tidyverse)

# Define the data
koppen_geiger <- tibble(
  Numeric = 1:30,
  Class = c(
    "Af", "Am", "Aw", "BWh", "BWk", "BSh", "BSk", "Csa", "Csb", "Csc",
    "Cwa", "Cwb", "Cwc", "Cfa", "Cfb", "Cfc", "Dsa", "Dsb", "Dsc", "Dsd",
    "Dwa", "Dwb", "Dwc", "Dwd", "Dfa", "Dfb", "Dfc", "Dfd", "ET", "EF"
  ),
  Description = c(
    "Tropical, rainforest", "Tropical, monsoon", "Tropical, savannah", "Arid, desert, hot",
    "Arid, desert, cold", "Arid, steppe, hot", "Arid, steppe, cold", 
    "Temperate, dry summer, hot summer", "Temperate, dry summer, warm summer", 
    "Temperate, dry summer, cold summer", "Temperate, dry winter, hot summer", 
    "Temperate, dry winter, warm summer", "Temperate, dry winter, cold summer", 
    "Temperate, no dry season, hot summer", "Temperate, no dry season, warm summer", 
    "Temperate, no dry season, cold summer", "Cold, dry summer, hot summer", 
    "Cold, dry summer, warm summer", "Cold, dry summer, cold summer", 
    "Cold, dry summer, very cold winter", "Cold, dry winter, hot summer", 
    "Cold, dry winter, warm summer", "Cold, dry winter, cold summer", 
    "Cold, dry winter, very cold winter", "Cold, no dry season, hot summer", 
    "Cold, no dry season, warm summer", "Cold, no dry season, cold summer", 
    "Cold, no dry season, very cold winter", "Polar, tundra", "Polar, frost"
  ),
  RGB = list(
    c(0, 0, 255), c(0, 120, 255), c(70, 170, 250), c(255, 0, 0), 
    c(255, 150, 150), c(245, 165, 0), c(255, 220, 100), 
    c(255, 255, 0), c(200, 200, 0), c(150, 150, 0), 
    c(150, 255, 150), c(100, 200, 100), c(50, 150, 50), 
    c(200, 255, 80), c(100, 255, 80), c(50, 200, 0), 
    c(255, 0, 255), c(200, 0, 200), c(150, 50, 150), 
    c(150, 100, 150), c(170, 175, 255), c(90, 120, 220), 
    c(75, 80, 180), c(50, 0, 135), c(0, 255, 255), 
    c(55, 200, 255), c(0, 125, 125), c(0, 70, 95), 
    c(178, 178, 178), c(102, 102, 102)
  )
)

# Add HTML colors
koppen_geiger <- koppen_geiger %>% 
  mutate(HTML_Color = map_chr(RGB, ~ rgb(.x[1]/255, .x[2]/255, .x[3]/255)))

# View the tibble
clrs <- koppen_geiger |> dplyr::select(Numeric, Class, Description, HTML_Color)

dir.create('./tbl')
write.csv(clrs, './tbl/colors.csv', row.names = FALSE)
