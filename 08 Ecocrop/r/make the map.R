

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, geodata, png, grid, ggspatial, showtext, extrafont, rmapshaper, remotes, ggthemes, RColorBrewer, tidyverse, gtools, RSAGA, sf, fs)
library(rnaturalearthdata)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Font --------------------------------------------------------------------
font_add_google(family = 'Fira Sans', name = 'Fira Sans Condensed')
showtext_auto()

# Theme -------------------------------------------------------------------
my_theme <- theme(axis.text.x = element_text(size = 61, family = 'Fira Sans', col = 'white'), 
                  axis.text.y = element_text(size = 61, family = 'Fira Sans', angle = 90, hjust = 0.5, col = 'white'),
                  axis.title.x = element_text(size = 80, family = 'Fira Sans', col = 'white'), 
                  axis.title.y = element_text(size = 80, family = 'Fira Sans', col = 'white'),
                  plot.title = element_text(size = 90, face = 'bold', hjust = 0.5, family = 'Fira Sans', col = 'white'),
                  plot.subtitle = element_text(size = 90, face = 'bold', hjust = 0.5, family = 'Fira Sans', col = 'white'),
                  legend.title = element_text(size = 70, family = 'Fira Sans', face = 'bold', col = 'white'),
                  legend.position = 'bottom',
                  legend.key.width = unit(4.8, 'line'),
                  legend.key.height = unit(0.8, 'line'),
                  legend.text = element_text(size = 70, col = 'white', family = 'Fira Sans'),
                  # panel.background = element_rect(fill = "white"),
                  plot.caption = element_text(size = 70, face = 'bold', family = 'Fira Sans', col = 'white'),
                  panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.1))

# Load data ---------------------------------------------------------------
rstr <- raster::raster('../ecocrop/baseline/coffee_Coffea.arabica_suitability.tif')
wrld <- st_read('D:/data/WORLD/all_countries.shp') %>% filter(CONTINENT == 'North America')
nica <- st_read('../shp/NIC_adm1.shp')

# Raster to table ---------------------------------------------------------
tble <- rasterToPoints(rstr, spatial = FALSE) %>% as_tibble %>% setNames(c('x', 'y', 'value'))
logo <- readPNG('../logo/resize.png')
logo <- rasterGrob(logo, x = unit(1.3, "npc"), y = unit(1.3, "npc"), width = unit(1.5, "npc"))

gmap <- ggplot() + 
  geom_tile(data = tble, aes(x = x, y = y, fill = value)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'RdYlGn'),
                       limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100)) +
  labs(x = 'Lon', y = 'Lat', fill = 'Idoneidad') + 
  geom_sf(data = wrld, fill = NA, col = 'grey70') +
  geom_sf(data = nica, fill = NA, col = 'white') +
  coord_sf(xlim = extent(nica)[1:2], ylim = extent(nica)[3:4]) +
  ggtitle(label = 'Idoneidad para el cultivo de café', 
          subtitle = 'A partir de requerimientos agroclimáticos') +
  theme_ft_rc(base_family = 'Fira Sans') +
  theme(axis.text.x = element_text(size = 41, family = 'Fira Sans'), 
        axis.text.y = element_text(size = 41, family = 'Fira Sans', angle = 90, hjust = 0.5),
        axis.title.x = element_text(size = 60, family = 'Fira Sans'), 
        axis.title.y = element_text(size = 60, family = 'Fira Sans'),
        plot.title = element_text(size = 70, face = 'bold', hjust = 0.5, family = 'Fira Sans'),
        plot.subtitle = element_text(size = 70, face = 'bold', hjust = 0.5, family = 'Fira Sans'),
        legend.title = element_text(size = 60, family = 'Fira Sans', face = 'bold'),
        legend.position = 'bottom',
        legend.key.width = unit(4.8, 'line'),
        legend.key.height = unit(0.8, 'line'),
        legend.text = element_text(size = 60, family = 'Fira Sans'),
        # panel.background = element_rect(fill = "white"),
        plot.caption = element_text(size = 60, face = 'bold', family = 'Fira Sans'),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.1)) +
  annotation_scale(location =  "br", width_hint = 0.5, text_cex = 2.5, text_family = 'Fira Sans', text_col = 'white') +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'Fira Sans', text_col = 'white', text_size = 25)) +
  annotate(geom = "text", x = -92, y = 13.7, hjust = 0, vjust = 1, 
           label = 'Elaboró: Fabio Castro / YouTube: Un Geógrafo en YouTube',
           size = 7, family = "Roboto", color = "black") +
  annotation_custom(logo, xmin = -89.2, xmax = -88.6, ymin = 17, ymax = 17.4)

dir_create('../png')
ggsave(plot = gmap, filename = '../png/map_ecocrop_nicaragua_cafe.png', units = 'in', width = 13, height = 11, dpi = 300)  
