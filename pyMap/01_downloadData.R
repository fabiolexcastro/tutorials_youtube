

## Load libraries --------------------------------------
require(pacman)
p_load(
  terra, geodata, sf, fs, tidyverse, gtools, stringr, glue
)

g <- gc(
  reset = T
)
rm(
  list = ls()
)
options(
  scipen = 999,
  warn = -1
)

## To download ---------------------------------------

## Vector data
col1 <- geodata::gadm(country = 'COL', level = 1, path = './tmpr')
wrld <- geodata::world(resolution = 1, path = './tmpr')

## Raster data
prec <- geodata::worldclim_country(country = 'COL', var = 'prec', path = './tmpr', version = '2.1')
tmin <- geodata::worldclim_country(country = 'COL', var = 'tmin', path = './tmpr', version = '2.1')
tmax <- geodata::worldclim_country(country = 'COL', var = 'tmax', path = './tmpr', version = '2.1')

## To add the data - Yearly -------------------------
prec <- sum(prec)
tmin <- mean(tmin)
tmax <- mean(tmax)

## To write the raster -----------------------------
dout <- 'H:/Mi unidad/YouTube/Season 13/python_map/tif'
dir.create(dout)

terra::writeRaster(
  x = prec, 
  filename = paste0(dout, '/', 'prec.tif')
)

terra::writeRaster(
  x = tmin,
  filename = paste0(dout, '/', 'tmin.tif'),
  overwrite = TRUE
)

terra::writeRaster(
  x = tmax, 
  filename = paste0(dout, '/', 'tmax.tif'),
  overwrite = TRUE
)

## To write the shapefile -----------------------------

dir.create('./gpkg')

terra::writeVector(
  x = wrld, 
  filename = './gpkg/world.gpkg', 
  overwrite = TRUE
)

terra::writeVector(
  x = col1, 
  filename = './gpkg/col1.gpkg',
  overwrite = TRUE
)
