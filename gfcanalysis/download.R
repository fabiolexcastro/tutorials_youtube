
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, gfcanalysis, tidyverse)
 
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
dpto <- st_read('D:/data/spatial/igac/dptos.gpkg')
mpio <- st_read('D:/data/spatial/igac/mpios.gpkg')
plot(st_geometry(dpto))
cqta <- dplyr::filter(dpto, DPTO_CNMBR == 'CAQUETÁ')
cqta.mpio <- dplyr::filter(mpio, DPTO_CNMBR == 'CAQUETÁ')

# Check the tiles ---------------------------------------------------------
tles <- calc_gfc_tiles(cqta)
dout <- 'tif/raw'
dir_create(dout)
download_tiles(tles, dout)

# Now - Extract by mask  --------------------------------------------------
fles <- dir_ls(dout)

# Tree cover
fles.tree <- grep('treecover', fles, value = T)
tree <- map(fles.tree, rast)
tree <- map(1:length(tree), function(i)terra::crop(tree[[i]], vect(cqta)) %>% terra::mask(., vect(cqta)))
tree <- sprc(tree)
tree <- mosaic(tree)

# Loss year
fles.loss <- grep('lossyear', fles, value = T)
loss <- map(fles.loss, rast)
loss <- map(1:length(loss), function(i)terra::crop(loss[[i]], vect(cqta)) %>% terra::mask(., vect(cqta)))
loss <- sprc(loss)
loss <- mosaic(loss)

# Gain 
fles.gain <- grep('gain', fles, value = T)
gain <- map(fles.gain, rast)
gain <- map(1:length(gain), function(i)terra::crop(gain[[i]], vect(cqta)) %>% terra::mask(., vect(cqta)))
gain <- sprc(gain)
gain <- mosaic(gain)

# To write these rasters --------------------------------------------------
dout <- 'tif/caqueta'
dir_create(dout)
terra::writeRaster(x = tree, filename = glue('{dout}/treecover.tif'), overwrite = T)
terra::writeRaster(x = gain, filename = glue('{dout}/gain.tif'), overwrite = T)
terra::writeRaster(x = loss, filename = glue('{dout}/loss.tif'), overwrite = T)
 
# Write caqueta -----------------------------------------------------------
st_write(cqta.mpio, 'gpk/caqueta_mpios.gpkg')
st_write(cqta, 'gpk/caqueta_dpto.gpkg')
st_write(dpto, 'gpk/dptos.gpkg')
