
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, elevatr, glue, trend, broom, rmapshaper, RColorBrewer, cptcity, gtools, rgeos, climateR, RSAGA)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
mpio <- st_read('gpkg/mpio.gpkg')
dpto <- st_read('gpkg/dpto.gpkg')
qndo <- filter(dpto, DPTO_CNMBR == 'QUINDIO')

# A simple plot -----------------------------------------------------------
ggplot() + 
  geom_sf(data = dpto, fill = NA, col = 'grey40') + 
  geom_sf(data = qndo, fill = 'red', col = 'red') + 
  theme_void() +
  coord_sf()

# Download ----------------------------------------------------------------
tmax <- getTerraClim(AOI = qndo, param = 'tmax', startDate = '1980-01-01', endDate = '2020-12-31')
tmin <- getTerraClim(AOI = qndo, param = 'tmin', startDate = '1980-01-01', endDate = '2020-12-31')

tmax <- tmax$terraclim_tmax
tmin <- tmin$terraclim_tmin

tavg <- (tmax + tmin) / 2
tavg <- rast(tavg)

# Download SRTM  ----------------------------------------------------------
srtm <- get_elev_raster(locations = qndo, z = 8)
srtm <- srtm * 1
srtm <- rast(srtm)

# Extract by mask 
srtm <- terra::crop(srtm, vect(qndo))
writeRaster(srtm, 'tif/srtm/srtm_8.tif')

# To write temperature files ----------------------------------------------
purrr::map(.x = 1:nlyr(tavg), .f = function(i){
  cat(i, '\n')
  name <- names(tavg[[i]])
  terra::writeRaster(x = tavg[[i]], filename = glue('tif/terraclimate/tavg_{name}.tif'), overwrite = TRUE)
  cat('Done!\n')
})

# Yearly to monthly -------------------------------------------------------
dir_create('tif/terraclimate/5km/yearly')
fles <- dir_ls('tif/terraclimate', regexp = '.tif$') %>% as.character() %>% mixedsort()
fles <- grep('tavg', fles, value = T)
year <- 1980L:2020L

purrr::map(.x = 1:length(year), .f = function(i){
  cat(i, '\n')
  rst <- grep(year[i], fles, value = T) %>% terra::rast() %>% mean()
  terra::writeRaster(x = rst, filename = glue('tif/terraclimate/5km/yearly/tavg_{year[i]}.tif'), overwrite = TRUE)
  cat('Done!\n')
})

# Go and download SAGA GIS v8.0.0 -----------------------------------------
fles <- dir_ls('tif/terraclimate/5km/yearly') %>% as.character %>% mixedsort()
envr <- rsaga.env('C:/saga-8.0.0_x64')

purrr::map(.x = 1:length(fles), .f = function(i){
  
  finp <- fles[i]
  fout <- glue('tif/terraclimate/zoom8/yearly/{basename(finp)}')
  
  cat('Processing layer #', i, '\n')
  rsl <- rsaga.geoprocessor(
    lib = 'statistics_regression',
    module = 'GWR for Grid Downscaling',
    param = list(PREDICTORS = 'tif/srtm/srtm_8.tif',
                 REGRESSION = fout,
                 DEPENDENT = finp),
    env = envr)
  
  plot(rast(fout))
  
  cat('Done!\n')
  
})


