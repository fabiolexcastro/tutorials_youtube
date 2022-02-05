
# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(raster, terra, ggpubr, gridExtra, sf, tidyverse, gtools, fs, glue, geodata, fields)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------

# dir_create('./raster/world')
# dir_create('./shp')

# Download administrative data
pan <- geodata::gadm(country = 'PAN', level = 1, version = 3.6, path = './shp')

# Download baseline data 
crn.30 <- geodata::worldclim_country(country = 'PAN', var = 'tmin', path = './raster/world')
crn.30 <- terra::crop(crn.30, pan)
crn.30 <- terra::mask(crn.30, pan)

crn.25 <- geodata::worldclim_global(var = 'tmin', res = 2.5, path = './raster/world')
crn.25 <- terra::crop(crn.25, pan)
crn.25 <- terra::mask(crn.25, pan)

ftr.25 <- geodata::cmip6_world(model = 'BCC-CSM2-MR', ssp = '370', time = '2041-2060', var = 'tmin', res = 2.5, path = './raster/world')
ftr.25 <- terra::crop(ftr.25, pan)
ftr.25 <- terra::mask(ftr.25, pan)

par(mfrow = c(1, 3))
plot(crn.30[[1]], main = 'Current 1 km')
plot(crn.25[[1]], main = 'Current 5 km')
plot(ftr.25[[1]], main = 'Future 5 km')

# Calculate the average ---------------------------------------------------
crn.30 <- terra::mean(crn.30)
crn.25 <- terra::mean(crn.25)
ftr.25 <- terra::mean(ftr.25)

# Create the mask ---------------------------------------------------------
msk <- raster(crn.30) * 0 

# Calculate the anomaly ---------------------------------------------------
dfr <- ftr.25 - crn.25
tbl <- as.data.frame(dfr, xy = TRUE)
tps <- fields::Tps(x = tbl[,c(1, 2)], Y = tbl[,3])
trp <- raster::interpolate(msk, tps)
trp <- raster::mask(trp,  as(pan, 'Spatial'))
ftr.30 <- crn.30 + rast(trp)

# A simple map
par(mfrow = c(1, 2))
plot(ftr.25)
plot(ftr.30)

plot(crn.30)
plot(ftr.30)

# To make the map ---------------------------------------------------------

tbl.30 <- raster::stack(raster(crn.30), raster(ftr.30))
tbl.30 <- rasterToPoints(tbl.30)
tbl.30 <- as_tibble(tbl.30)
colnames(tbl.30) <- c('lon', 'lat', 'Current', 'Future')
tbl.30 <- gather(tbl.30, period, value, -lon, -lat)
tbl.30 <- mutate(tbl.30, period = factor(period, levels = c('Current', 'Future')))

tbl.25 <- raster::stack(raster(crn.25), raster(ftr.25), raster(dfr))
tbl.25 <- rasterToPoints(tbl.25)
tbl.25 <- as_tibble(tbl.25)
colnames(tbl.25) <- c('lon', 'lat', 'Current', 'Future', 'Delta')
tbl.25 <- gather(tbl.25, period, value, -lon, -lat)
tbl.25 <- mutate(tbl.25, period = factor(period, levels = c('Current', 'Future', 'Delta')))

tbl.tr <- rasterToPoints(trp)
tbl.tr <- as_tibble(tbl.tr)
tbl.tr <- mutate(tbl.tr, period = 'Interpolated')

# Colors
library(cptcity)
find_cpt(name = 'temperature')
image(matrix(1:100),col = cpt(pal = "jjg_misc_temperature"),axes = FALSE)
image(matrix(1:100),col = cpt(pal = "kst_03_red_temperature"),axes = FALSE)

# Maps
g25.prd <- ggplot() + 
  geom_tile(data = filter(tbl.25, period %in% c('Current', 'Future')), aes(x = lon, y = lat, fill = value)) + 
  facet_wrap(.~period) + 
  scale_fill_gradientn(colors = cpt(pal = 'jjg_misc_temperature')) +
  geom_sf(data = st_as_sf(pan), fill = NA) + 
  coord_sf() + 
  theme_void() +
  theme(legend.position = 'bottom', 
        legend.key.width = unit(1.3, 'cm')) + 
  labs(x = 'Longitude', y = 'Latitude', fill = '°C')

g25.dlt <- ggplot() + 
  geom_tile(data = filter(tbl.25, period == 'Delta'), aes(x = lon, y = lat, fill = value)) +
  facet_wrap(.~period) + 
  scale_fill_gradientn(colors = rev(cpt(pal = 'kst_03_red_temperature'))) +
  geom_sf(data = st_as_sf(pan), fill = NA) + 
  coord_sf() + 
  theme_void() +
  theme(legend.position = 'bottom', 
        legend.key.width = unit(1.3, 'cm')) + 
  labs(x = 'Longitude', y = 'Latitude', fill = '°C')

# Arrange
g25 <- gridExtra::grid.arrange(g25.prd, g25.dlt, layout_matrix = matrix(c(1, 1, 1, 1, 2, 2), byrow = TRUE, ncol = 6, nrow = 1))

gan <- g25.dlt + 
  geom_point(data = tbl, aes(x = x, y = y), size = 0.1)

gin <- ggplot() + 
  geom_tile(data = tbl.tr, aes(x = x, y = y, fill = layer)) +
  facet_wrap(.~period) + 
  scale_fill_gradientn(colors = rev(cpt(pal = 'kst_03_red_temperature'))) +
  geom_sf(data = st_as_sf(pan), fill = NA) + 
  coord_sf() + 
  theme_void() +
  theme(legend.position = 'bottom', 
        legend.key.width = unit(1.3, 'cm')) + 
  labs(x = 'Longitude', y = 'Latitude', fill = '°C')

gfn <- ggplot() + 
  geom_tile(data = filter(tbl.30, period == 'Future'), aes(x = lon, y = lat, fill = value)) +
  facet_wrap(.~period) + 
  scale_fill_gradientn(colors = cpt(pal = 'jjg_misc_temperature')) +
  geom_sf(data = st_as_sf(pan), fill = NA) + 
  coord_sf() + 
  theme_void() +
  theme(legend.position = 'bottom', 
        legend.key.width = unit(1.3, 'cm')) + 
  labs(x = 'Longitude', y = 'Latitude', fill = '°C')

# Arrange
g30 <- gridExtra::grid.arrange(gan, gin, gfn, layout_matrix = matrix(c(1, 1, 2, 2, 3, 3), byrow = TRUE, ncol = 6, nrow = 1))

# Final Arrange
g30 <- ggpubr::ggarrange(g25, g30, ncol = 1, nrow = 2)
ggsave(plot = g30, filename = './png/final_map.jpg', units = 'in', width = 10, height = 6, dpi = 300)
