

library(raster)
library(rgdal)
library(tidyverse)
library(gtools)
library(sf)
library(devtools)
install_github('marcosci/layer')
library(layer)
library(geodata)

crnt <- raster('../raster/RF_5Prob_current.asc')
ft30 <- raster('../raster/RF5prob_2030.asc')
ft50 <- raster('../raster/RF5prob_2050.asc')
hndr <- gadm(country = 'HND', level = 1, path = './shp')
hndr <- as(hndr, 'Spatial')
crnt <- raster::crop(crnt, hndr) %>% raster::mask(., hndr)
ft30 <- raster::crop(ft30, hndr) %>% raster::mask(., hndr)
ft50 <- raster::crop(ft50, hndr) %>% raster::mask(., hndr)
srtm <- elevation_30s(country = 'HND', path = tempdir())

# Tild maps
tilt_crnt <- tilt_map(crnt)
tilt_ft30 <- tilt_map(ft30, x_shift = 2, y_shift = 2)
tilt_ft50 <- tilt_map(ft50, x_shift = 4, y_shift = 4)

map_list <- list(tilt_crnt, tilt_ft30, tilt_ft50)

plot_tiltedmaps(map_list, palette =c("tofino", "rocket", "mako"), direction = c(-1, 1, 1)) 

