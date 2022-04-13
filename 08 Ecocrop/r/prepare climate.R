
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, geodata, glue, tidyverse)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
country <- 'NIC'
limt <- geodata::gadm(country = country, level = 0, path = '../tmpr')

# Download climate
prec <- geodata::worldclim_country(country = country, var = 'prec', res = 0.5, path = '../tmpr')
prec <- terra::crop(prec, limt) %>% terra::mask(., limt)

tmax <- geodata::worldclim_country(country = country, var = 'tmax', res = 0.5, path = '../tmpr')
tmax <- terra::crop(tmax, limt) %>% terra::mask(., limt)

tmin <- geodata::worldclim_country(country = country, var = 'tmin', res = 0.5, path = '../tmpr')
tmin <- terra::crop(tmin, limt) %>% terra::mask(., limt)

# Calculate the t mean ----------------------------------------------------
tavg <- (tmax + tmin) / 2

# Multiply by 10 ----------------------------------------------------------
tmax <- tmax * 10 
tmin <- tmin * 10 
tavg <- tavg * 10

# Write the results as ascii  ---------------------------------------------
dir_create('../raster/current')

map(1:12, function(i){
  
  cat(month.abb[i], '\t')
  terra::writeRaster(x = tmax[[i]], filename = glue('../raster/current/tmax_{i}.asc')) 
  terra::writeRaster(x = tmin[[i]], filename = glue('../raster/current/tmin_{i}.asc')) 
  terra::writeRaster(x = tavg[[i]], filename = glue('../raster/current/tmean_{i}.asc')) 
  terra::writeRaster(x = prec[[i]], filename = glue('../raster/current/prec_{i}.asc')) 
  cat('Done!\n')
  
})

root <- 'D:/data/WORLDCLIM/Version20'
fles <- dir_ls(root)
fles <- grep('.tif$', fles, value = T)
fles <- as.character(fles)
tmax <- grep('tmax', fles, value = T)
tmin <- grep('tmin', fles, value = T)
tavg <- grep('tavg', fles, value = T)
prec <- grep('prec', fles, value = T)

tmax <- terra::rast(tmax)
tmax <- tmax %>% terra::crop(., limt) %>% terra::mask(., limt)
tmin <- terra::rast(tmin) %>% terra::crop(., limt) %>% terra::mask(., limt)
prec <- terra::rast(prec) %>% terra::crop(., limt) %>% terra::mask(., limt)
tavg <- terra::rast(tavg) %>% terra::crop(., limt) %>% terra::mask(., limt)

tavg <- tavg * 10
tmax <- tmax * 10
tmin <- tmin * 10


map(1:12, function(i){
  
  cat(month.abb[i], '\t')
  terra::writeRaster(x = tmax[[i]], filename = glue('../raster/current/tmax_{i}.asc')) 
  terra::writeRaster(x = tmin[[i]], filename = glue('../raster/current/tmin_{i}.asc')) 
  terra::writeRaster(x = tavg[[i]], filename = glue('../raster/current/tmean_{i}.asc')) 
  terra::writeRaster(x = prec[[i]], filename = glue('../raster/current/prec_{i}.asc')) 
  cat('Done!\n')
  
})
