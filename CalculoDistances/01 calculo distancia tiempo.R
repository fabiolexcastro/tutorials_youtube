
# Cargamos librerias 
require(pacman)
pacman::p_load(raster, rgdal, rgeos, ggrepel, 
               RColorBrewer, stringr, sf, tidyverse,
               gtools, mapsf, fs)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Cargamos datos
road <- sf::st_read('D:/data/IGAC/VIAS/roads.shp')
dpto <- sf::st_read('D:/data/IGAC/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')
dpto

# Filtramos datos 
vlle <- dplyr::filter(dpto, DPTO_CNMBR == 'VALLE DEL CAUCA')
mf_map(x = vlle)

# Filtramos las vias para el Valle del Cauca 
st_crs(road)
st_crs(vlle)

vlle <- st_transform(x = vlle, crs = st_crs(road))
road_vlle <- sf::st_intersection(road, vlle)
road_vlle

mf_map(x = road_vlle, add = TRUE)

# Revisando los tipos de vias, y filtramos las de los tres primeros orden
# road_vlle <- road_vlle %>% filter(TIPO_VIA %in% 1:3)
unique(road_vlle$TIPO_VIA)

# Caragmos la libreria para calculo del tiempo entre un lugar a y un lugar b 
# install.packages('osrm')
library(osrm)

coord <- data.frame(lugar = c('Cali', 'Cartago'), lon = c(-76.5282, -75.9073), lat = c(3.4461, 4.7693))
coord <- st_as_sf(x = coord, coords = c('lon', 'lat'), crs = st_crs(4326))
coord_tble <- st_coordinates(coord) %>% as_tibble %>% mutate(lugar = c('Cali', 'Cartago'))
route <- osrmRoute(src = coord[1,], dst = coord[2,], returnclass = 'sf')
plot(st_geometry(route), add = TRUE, col = 'red')

fs::dir_create('../data/shp')
st_write(route, '../data/shp/route_cali_cartago_v2.shp')
route

# Ahora manos a la obra para realizar el mapa
route <- st_transform(x = route, crs = st_crs(road_vlle))
road_vlle <- road_vlle %>% mutate(TIPO_VIA = factor(TIPO_VIA, levels = as.character(1:8)))

road_map <- ggplot() + 
  geom_sf(data = dpto, fill = '#FAFAFA') + 
  geom_sf(data = vlle, fill = '#D8D8D8') + 
  geom_sf(data = road_vlle, aes(col = TIPO_VIA)) + 
  scale_color_manual(values = RColorBrewer::brewer.pal(n = 8, name = 'Greys')) + 
  # guides(color = guide_legend(ncol = 2)) +
  guides(color = 'none') + 
  geom_sf(data = coord, col = 'red', size = 2) +
  geom_sf(data = route, col = 'black', fill = NA, size = 1.3) + 
  geom_sf_text(data = dpto %>% filter(DPTO_CNMBR != 'VALLE DEL CAUCA'), aes(label = str_to_sentence(DPTO_CNMBR))) + 
  geom_text_repel(data = coord_tble, aes(x = X, y = Y, label = lugar), fontface = 'bold') + 
  ggtitle(label = 'Ruta desde Cali a Cartago - Valle del Cauca', 
          subtitle = paste0('Tiempo estimado: ', route$duration %>% round(., 1), ' minutos / Distancia: ', route$distance, ' km')) +
  coord_sf(xlim = extent(vlle)[1:2], ylim = extent(vlle)[3:4]) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.1), 
        plot.title = element_text(size = 13, face = 'bold', hjust = 0.5), 
        plot.subtitle = element_text(size = 11, face = 'bold', hjust = 0.5), 
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  labs(x = 'Longitude', y = 'Latitude', color = 'Tipo vía', caption = 'Elaboró: Fabio Castro') 


fs::dir_create('../png')
ggsave(plot = road_map, filename = '../png/map_v1.png', units = 'in', width = 8.5, height = 8.5, dpi = 300)
