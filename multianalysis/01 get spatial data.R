


# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, glue, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Study zone --------------------------------------------------------------
col1 <- vect('D:/data/spatial/igac/dptos.gpkg')
limt <- col1[col1$DPTO_CNMBR == 'CUNDINAMARCA',]

terra::writeVector(limt, 'shape_input_v1/cundinamarca.gpkg')

# Download ----------------------------------------------------------------

# Accesibility
accs <- travel_time(to = 'city', size = 6, up = TRUE, quiet = T, path = tempdir())
accs <- terra::crop(accs, limt)
accs <- terra::mask(accs, limt)
terra::writeRaster(x = accs, filename = 'raster_input_v1/accesibility.tif', overwrite = TRUE)

# Temperature 
tavg <- worldclim_country(country = 'COL', var = 'tavg', path = tempdir())
tavg <- terra::crop(tavg, limt)
tavg <- terra::mask(tavg, limt)
terra::writeRaster(x = tavg, filename = 'raster_input_v1/tavg.tif', overwrite = TRUE)

# Altitude 
srtm <- elevation_30s(country = 'COL', path = tempdir())
srtm <- terra::crop(srtm, limt)
srtm <- terra::mask(srtm, limt)
terra::writeRaster(x = srtm, filename = 'raster_input_v1/srtm.tif', overwrite = T)

# Protected areas # Source: https://www.protectedplanet.net/country/COL
prot <- terra::vect('raw/protected/WDPA_WDOECM_Apr2023_Public_COL_shp_2/WDPA_WDOECM_Apr2023_Public_COL_shp-polygons.shp')
prot <- terra::crop(prot, limt)
terra::writeVector(x = prot, filename = 'shape_input_v1/protected.gpkg')

# Rivers
rvr1 <- terra::vect('shape_input_v1/rios_doble.gpkg')
rvr2 <- terra::vect('shape_input_v1/rios_sencillos.gpkg')

