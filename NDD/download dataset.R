
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, rmapshaper, tidyverse, climateR, glue, RColorBrewer)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
dpto <- terra::vect('D:/data/spatial/igac/Municipios202305.gpkg')
path <- 'D:/data/spatial/climate/CHIRPS'
fles <- dir_ls(path, regexp = '.tif$') %>% as.character()

# Filtering the study zone ------------------------------------------------
llns <- dpto[dpto$Depto %in% c('Meta', 'Casanare', 'Arauca', 'Vichada')]
limt <- st_as_sf(llns)
limt <- st_transform(limt, crs = st_crs(4326))
limt <- ms_dissolve(limt)
limt <- vect(limt)

dir.create('./gpkg')
terra::writeVector(x = limt, filename = './gpkg/limt.gpkg')
terra::writeVector(x = llns, filename = './gpkg/llanos.gpkg')

# How to use the getCHIRPS function ---------------------------------------
chrp <- climateR::getCHIRPS(AOI = limt, varname = 'precip', timeRes = 'daily', startDate = '1980-01-01', endDate = '1981-12-31')
chrp <- chrp$precip

# To download -------------------------------------------------------------
down.chirps <- function(yr){
  
  cat('To process: ', yr, '\t')
  str <- glue('{yr}-01-01')
  end <- glue('{yr}-12-31')
  chr <- climateR::getCHIRPS(AOI = limt, varname = 'precip', timeRes = 'daily', startDate = str, endDate = end)
  chr <- chr$precip
  terra::writeRaster(x = chr, filename = glue('./tif/chirps/down/chirps_{yr}.tif'), overwrite = TRUE)
  cat('Done!\n')

}

# To apply the function 
map(.x = 1981:2020, .f = down.chirps)

# End ---------------------------------------------------------------------


