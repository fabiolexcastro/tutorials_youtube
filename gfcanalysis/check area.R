
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, gfcanalysis, tidyverse, xlsx, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
dir_ls('gpk')

mpio <- vect('gpk/caqueta_mpios.gpkg')
dpto <- vect('gpk/dptos.gpkg')
cqta <- dpto[dpto$DPTO_CNMBR == 'CAQUETÁ',]

dir_ls('tif/caqueta')

gain <- terra::rast('tif/caqueta/gain.tif')
tree <- terra::rast('tif/caqueta/treecover.tif')
loss <- terra::rast('tif/caqueta/loss.tif')

names(gain) <- 'gain'
names(tree) <- 'tree'
names(loss) <- 'loss'

stck <- c(tree, gain, loss)
plot(stck)

extr <- extract_gfc(aoi = st_as_sf(cqta), data_folder = 'tif/raw')
thrs <- threshold_gfc(gfc = extr)
thrs <- rast(thrs)

# To count the loss area --------------------------------------------------
tble <- purrr::map(.x = 1:nrow(mpio), .f = function(i){
  
  mpo <- mpio[i,]; cat(mpo$MPIO_CNMBR, '\t')
  
  nme <- mpo$MPIO_CNMBR
  nme <- iconv(nme, to = "ASCII//TRANSLIT")
  
  stc <- terra::crop(thrs, mpo)
  stc <- terra::mask(stc, mpo)
  
  # Forest area (2000)
  frs <- stc[[1]]
  frs.frq <- as_tibble(freq(frs))
  frs.frq <- mutate(frs.frq, class = c('No bosque', 'Bosque'))
  frs.frq <- dplyr::select(frs.frq, class, pixels = count)
  
  # To calculate the area (pixel)
  msk <- terra::project(frs, '+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs')
  has <- (res(msk)[1] * res(msk)[2])/10000
  
  # To add the area for the table
  frs.frq <- mutate(frs.frq, forest_hectares = pixels * has)
  frs.frq <- mutate(frs.frq, municipio = nme)
  frs.frq <- relocate(frs.frq, municipio, class, pixels, forest_hectares)
  
  # Loss area
  lss <- stc[[2]]
  lss <- as_tibble(freq(lss))
  lss <- filter(lss, value != 0)
  lss <- mutate(lss, year = 2000 + value)
  lss <- mutate(lss, loss_hectares = count * has)
  lss <- mutate(lss, municipio = nme)
  lss <- dplyr::select(lss, municipio, year, loss_hectares)
  
  # Gain area
  gnn <- stc[[4]]
  gnn <- as_tibble(freq(gnn))
  gnn <- mutate(gnn, class = c('nada', 'ganancia'))
  gnn <- mutate(gnn, hectares = count * has)
  gnn <- mutate(gnn, municipio = nme)
  gnn <- dplyr::select(gnn, municipio, class, hectares)
  
  cat('Done!\n')
  
  # Return
  return(list(frs.frq, lss, gnn))
  
})

# Compile the tables ------------------------------------------------------
tble.frst <- map(tble, 1)
tble.loss <- map(tble, 2)
tble.gain <- map(tble, 3)

tble.frst <- bind_rows(tble.frst)
tble.loss <- bind_rows(tble.loss)
tble.gain <- bind_rows(tble.gain)

tble.gain <- filter(tble.gain, class == 'ganancia')
tble.frst <- dplyr::select(tble.frst, -pixels)

# Write the results -------------------------------------------------------
dir_create('tble')
xlsx::write.xlsx(as.data.frame(tble.frst), 'tble/forest_caqueta.xlsx', sheet = 'Forest', append = FALSE, row.names = F)
xlsx::write.xlsx(as.data.frame(tble.loss), 'tble/forest_caqueta.xlsx', sheet = 'Loss', append = TRUE, row.names = F)
xlsx::write.xlsx(as.data.frame(tble.gain), 'tble/forest_caqueta.xlsx', sheet = 'Gain', append = TRUE, row.names = F)






