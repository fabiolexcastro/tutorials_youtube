
# Load libraries -----------------------------------------------------------
require(pacman)
suppressMessages(pacman::p_load(sf, rgdal, ggpubr, gridExtra, cptcity, ggthemes, rnaturalearthdata, rnaturalearth, hrbrthemes, climateStability, RColorBrewer, geodata, tmap, fs, raster, rgdal, gstat, tidyverse, sp))

g <- gc(reset = T)
rm(list = ls())
.rs.restartR() 
options(warn = -1, scipen = 999) 

# Load data ---------------------------------------------------------------
gtml <- geodata::gadm(country = 'GTM', level = 1, path = '../tmpr')
gtml <- as(gtml, 'Spatial')
dir_ls('../tbl/')
data <- suppressMessages(read_csv('../tbl/pseudo_values_all_tmax.csv'))

# World shape
wrld <- ne_countries(scale = 50, returnclass = 'sf')

# Filtering for January
data <- filter(data, mes_nme == 'Ene')
frml <- as.formula(base ~ lon + lat)

# Table to shapefile 
coordinates(data) <- ~ lon + lat
raster::crs(data) <- raster::crs(gtml)

# Variogram
varg <- variogram(frml, data, cloud = F)

# Modelo de variograma pasando los valores de pixel, umbral y rango 
fit0 <- fit.variogram(varg, fit.ranges = T, fit.sills = T, vgm(psill = 4, model = 'Sph',nugget = 0)) # 100 es bien
plot(varg, fit0)

# fit0 <- fit.variogram(varg, fit.ranges = T, fit.sills = T, vgm(psill = 4, model = 'Sph'))
# plot(varg, fit0)

data@bbox <- gtml@bbox

# Take the grid
grd1 <- as.data.frame(spsample(data, "regular", n=50000))
names(grd1) <- c("x1", "x2")
coordinates(grd1) <- c("x1", "x2")
gridded(grd1)  <- TRUE # crear el objeto SpatialPixel 
fullgrid(grd1) <- TRUE # crear el objeto SpatialGrid

raster::crs(grd1) <- raster::crs(data)
plot(grd1)
plot(data, add = TRUE, col = 'red', pch = 16)

# To make the interpolation
dat.krg0 <- krige(base ~ 1, data, grd1, fit0)

# Raster files
r0 <- raster(dat.krg0, layer = 'var1.pred')
r0 <- raster::mask(r0, gtml)
plot(r0)
plot(data, add = T, col = 'red', pch = 16)

# Varianza
r1 <- raster(dat.krg0, layer = 'var1.var')
r1 <- raster::mask(r1, gtml)
plot(r1); plot(data, add = T, pch = 16, col = 'red')

# Incertidumbre 
r2 <- sqrt(raster(dat.krg0, layer =  'var1.var')) * 1.96
r2 <- raster::mask(r2, gtml)
plot(r2); plot(data, add = T, pch = 16, col = 'red')

r2 <- rescale0to1(r2)
plot(r2)

# Add all layers into only one raster -------------------------------------
stk <- addLayer(r0, r2)
names(stk) <- c('Temperatura', 'Incertidumbre')
tbl <- rasterToPoints(stk, spatial = FALSE)
tbl <- as_tibble(tbl)

# Color
find_cpt('temperature')
find_cpt('reds')
image(matrix(1:100),col = cpt(pal = "jjg_misc_temperature"),axes = FALSE)

gprd <- ggplot() +
  geom_tile(data = tbl, aes(x = x, y = y, fill = Temperatura)) + 
  scale_fill_gradientn(colors = cpt(pal = 'jjg_misc_temperature')) + 
  geom_sf(data = st_as_sf(gtml), fill = NA, col = 'grey50', lwd = 0.4) +
  geom_sf(data = wrld, fill = NA, col = 'grey70', lwd = 0.2) +
  geom_sf_text(data = wrld, aes(label = admin )) +
  coord_sf(xlim = extent(gtml)[1:2], ylim = extent(gtml)[3:4]) + 
  ggthemes::theme_pander() +
  ggtitle(label = 'Superficie interpolada (kriging) de\ntemperatura máxima') +
  labs(x = 'Longitud', y = 'Latitud', fill = 'Temperatura (°C)') + 
  theme(legend.position = 'bottom', 
        legend.key.width = unit(5, 'line'),
        plot.title = element_text(size = 16, hjust = 0.5, face = 'bold'),
        legend.title = element_text(face = 'bold'),
        legend.key.height = unit(0.6, 'line')) 

ginc <- ggplot() +
  geom_tile(data = tbl, aes(x = x, y = y, fill = Incertidumbre)) + 
  scale_fill_gradientn(colors = rev(cpt(pal = 'ocal_reds'))) + 
  geom_sf(data = st_as_sf(gtml), fill = NA, col = 'grey50', lwd = 0.4) +
  geom_sf(data = wrld, fill = NA, col = 'grey70', lwd = 0.2) +
  geom_sf_text(data = wrld, aes(label = admin )) +
  coord_sf(xlim = extent(gtml)[1:2], ylim = extent(gtml)[3:4]) + 
  ggthemes::theme_pander() +
  ggtitle(label = 'Incertidumbre de los\ndatos interpolados') +
  labs(x = 'Longitud', y = 'Latitud', fill = 'Temperatura (°C)') + 
  theme(legend.position = 'bottom', 
        legend.key.width = unit(5, 'line'),
        plot.title = element_text(size = 16, hjust = 0.5, face = 'bold'),
        legend.title = element_text(face = 'bold'),
        legend.key.height = unit(0.6, 'line')) 

gall <- ggarrange(gprd, ginc, ncol = 2, nrow = 1)
ggsave(plot = gall, filename = '../png/maps/map_final.png', units = 'in', width = 16, height = 15, dpi = 300)

