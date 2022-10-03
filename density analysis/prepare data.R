

# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, sf, fs, glue, tidyverse, gfcanalysis)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
bsin <- st_read('D:/data/spatial/igac/basins_col.gpkg')
bsin <- st_transform(bsin, crs = st_crs(4326))

bsin$NOMZH %>% unique()
head(bsin)

# Check the tiles
cttm <- bsin %>% filter(NOMSZH == 'Bajo Catatumbo')
tles <- calc_gfc_tiles(aoi = cttm)

# To write the basin shapefile
st_write(cttm, 'gpgk/catatumbo.gpkg')

# To download Hansen ------------------------------------------------------
hnsn <- download_tiles(
  tiles = tles,
  output_folder = 'tmpr',
  images = c("treecover2000", "lossyear", "gain", "datamask"),
  dataset = "GFC-2021-v1.9"
)

fles <- dir_ls('tmpr') %>% grep('v1.9', ., value = T) %>% as.character()
tree <- grep('treecover', fles, value = T)
tree <- terra::rast(tree)
tree <- terra::crop(tree, vect(cttm))
tree <- terra::mask(tree, vect(cttm))

loss <- grep('loss', fles, value = T)
loss <- terra::rast(loss)
loss <- terra::crop(loss, vect(cttm))
loss <- terra::mask(loss, vect(cttm))

loss[which.lyr(loss == 0)] <- NA

# Raster to table 
tble <- terra::as.data.frame(loss, xy = T)
tble <- as_tibble(tble)
colnames(tble) <- c('x', 'y', 'year')
tble <- filter(tble, year == 21)
tble <- mutate(tble, value = 1, year = 2000 + year)

write.csv(tble, './tble/loss_2021.csv', row.names = FALSE)

# All BD  -----------------------------------------------------------------
tree
loss
lyrs <- c(tree, loss)
names(lyrs) <- c('treecover', 'lossyear')
alld <- terra::as.data.frame(lyrs, xy = TRUE) %>% 
  as_tibble() %>% 
  mutate(gid = 1:nrow(.)) %>% 
  gather(var, value, -gid, -x, -y) %>% 
  mutate(var = factor(var, levels = c('treecover', 'lossyear')))

write.csv(alld, './tble/treecover_loss.csv', row.names = FALSE)
