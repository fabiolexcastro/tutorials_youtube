
# Load libraries
require(pacman)
pacman::p_load(raster, ggnewscale, rgdal, rgeos, stringr, RColorBrewer, sf, tidyverse, gtools, ggnewscale)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data
limt <- raster::getData(name = 'GADM', country = 'CRI', level = 0)
lim2 <- raster::getData(name = 'GADM', country = 'CRI', level = 1) 
coor <- coordinates(limt) %>% as.data.frame %>% as_tibble()
wrld <- sf::st_read('../shp/all_countries.shp')
wrld <- wrld %>% filter(CONTINENT %in% c('North America', 'South America'))

# Download SRTM 
srt1 <- raster::getData(name = 'SRTM', lon = as.numeric(coor[1,1]), lat = as.numeric(coor[1,2]))
srt2 <- raster::getData(name = 'SRTM', lon = as.numeric(coor[1,1]), lat = as.numeric(coor[1,2]) + 1)
srt3 <- raster::getData(name = 'SRTM', lon = as.numeric(coor[1,1]) - 1, lat = as.numeric(coor[1,2]) + 1)
srt4 <- raster::getData(name = 'SRTM', lon = -86, lat = 11)
srt5 <- raster::getData(name = 'SRTM', lon = -85.5, lat = 9.7)
srt6 <- list(srt1, srt2, srt3, srt4, srt5)
srt6$fun <- mean
srtm <- do.call(mosaic, srt6)
plot(srtm)

# Extract by mask 
srtm <- raster::crop(srtm, limt)
srtm <- raster::mask(srtm, limt)
srtm <- raster::crop(srtm, c(-86, -82, 8, 11.21))

# Get the hillshade
trrn <- raster::terrain(srtm, opt = c('slope', 'aspect'))
hlls <- raster::hillShade(slope = trrn[[1]], aspect = trrn[[2]], angle = 40, direction = 270)
extn <- c(extent(hlls)[1:2], 8, extent(hlls)[4])
trrn <- raster::crop(trrn, extn)
hlls <- raster::crop(hlls, extn)
srtm <- raster::crop(srtm, extn)

# To make the map
trrn.tble <- raster::rasterToPoints(trrn, spatial = FALSE) %>% 
  as_tibble %>% 
  set_names(c('x', 'y', 'slope', 'aspect'))
srtm.tble <- raster::rasterToPoints(srtm, spatial = FALSE) %>% 
  as_tibble %>% 
  set_names(c('x', 'y', 'value'))
hlls.tble <- raster::rasterToPoints(hlls, spatial = FALSE) %>% 
  as_tibble %>% 
  set_names(c('x', 'y', 'value'))


map <- ggplot()+ 
  geom_sf(data = st_as_sf(lim2), fill = NA) +
  geom_sf(data = wrld, fill = NA, col = 'grey10') +
  geom_tile(data = srtm.tble, aes(x = x, y = y, fill = value), alpha = 0.75) + 
  scale_fill_gradientn(colors = terrain.colors(n = 15)) + 
  labs(x = 'Longitude', y = 'Latitude', fill = 'Altitud (m.s.n.m)', caption = 'SRTM') +
  new_scale_fill() +
  geom_tile(data = hlls.tble, aes(x = x, y = y, fill = value), alpha = 0.5) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 8, name = 'Greys'), guide = 'none',
                       name = 'Altitud (m.s.n.m)') +
  geom_sf_text(data = st_as_sf(lim2), aes(label = NAME_1), size = 1.8) +
  ggtitle(label = 'Modelo de Elevación Digital para Costa Rica') + 
  coord_sf(xlim = c(-86, -82.8), ylim = c(8, 11.3)) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.key.width = unit(3, 'line'), 
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        plot.title = element_text(size = 12, face = 'bold', hjust = 0.5)) 

ggsave(plot = map, filename = '../png/map_dem.png', units = 'in', width = 8, height = 8, dpi = 300)
