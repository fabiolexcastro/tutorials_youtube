
# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(raster, terra, sf, fs, viridis, ggpubr, lubridate, tidyverse, glue, geodata, gtools, rgeos, meteo, sp, spacetime, gstat, plyr, xts, snowfall, doParallel,  CAST, ranger)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
dir_ls('tbl')
tble <- read_csv('tbl/mean_boyaca_2020.csv')
dpts <- st_read('gpkg/dptos.gpkg')
byca <- st_read('gpkg/boyaca.gpkg')

# Stations  
stts <- read_csv('tbl/stations_boyaca.csv')
stts <- dplyr::select(stts, Code, Municipality, Lon, Lat)

# Download srtm 
srtm <- geodata::elevation_30s(country = 'COL', path = 'tmpr')
srtm <- terra::crop(srtm, vect(byca))
srtm <- terra::mask(srtm, vect(byca))

# Join between the table and the station long / lati ---------------------
tble <- inner_join(tble, stts, by = c('code' = 'Code'))
tble <- dplyr::select(tble, Municipality, code, Lon, Lat, year, value)

# To extract the srtm values ----------------------------------------------
tble <- as_tibble(cbind(tble, srtm = terra::extract(srtm, tble[,c('Lon', 'Lat')])[,2]))
tble <- filter(tble, value <= 1000)

plot(tble$value, tble$srtm)
plot(tble$srtm, tble$value)

# To make a map -----------------------------------------------------------
gmap <- ggplot() + 
  geom_point(data = tble, aes(x = Lon, y = Lat, col = value)) + 
  scale_color_viridis() +
  geom_sf(data = byca, fill = NA, col = 'grey50') +
  geom_sf(data = dpts, fill = NA, col = 'grey60') + 
  labs(x = 'Lon', y = 'Lat', col = '\u00B0C') +
  coord_sf(xlim = ext(byca)[1:2], ylim = ext(byca)[3:4]) +
  ggtitle(label = 'Comportamiento de la temperatura\npara las estaciones climáticas del IDEAM', 
          subtitle = 'Año 2020') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        axis.text.x = element_text(size = 8), 
        plot.title = element_text(size = 18, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = 'bold', hjust = 0.5),
        axis.text.y = element_text(angle = 90, vjust = 0.5, size = 8),
        legend.key.height = unit(2.4, 'line'))  

ggsave(plot = gmap, filename = 'png/map_points_temperature_2020.png', units = 'in', width = 9, height = 7, dpi = 300)

# Check the database -----------------------------------------------------
gpnt <- ggplot(data = tble, aes(x = value, y = srtm, col = value)) + 
  geom_smooth(method = 'lm', se = FALSE, col = 'grey40', size = 0.25) + 
  geom_point() +
  scale_color_viridis() +
  stat_cor(method = "pearson", label.x = 7.5, label.y = 500) +
  labs(x = 'Temperatura promedio \u00B0C', y = 'DEM (m.s.n.m.)') +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'none', 
        panel.grid.minor = element_blank(), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold')) +
  guides(col = guide_legend(show = FALSE))

ggsave(plot = gpnt, filename = 'png/graph_temp_srtm.png', units = 'in', width = 8, height = 7, dpi = 300)

# Write the database
write.csv(tble, 'tbl/database.csv', row.names = FALSE)


