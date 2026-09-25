
## Shake Map: https://earthquake.usgs.gov/earthquakes/eventpage/us6000tjl2/shakemap/intensity

# Load libraries ----------------------------------------------------------
require(pacman)
p_load(
  terra, fs, sf, geodata, tidyterra, patchwork, janitor, leaflet, tidyverse, gtools, stringr, glue, RColorBrewer, ggspatial
)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Functions ---------------------------------------------------------------
read_grid <- function(x){
  r <- rast(x)
  if(is.na(crs(r)) || crs(r) == '') crs(r) <- 'EPSG:4326'
  r
}

# Load data ---------------------------------------------------------------
data <- dir_ls('./', regexp = '.csv$') 
data <- read_csv(data, show_col_types = FALSE)
dpto <- st_read('./shp/MGN_DPTO_POLITICO.shp')
zone <- dpto[dpto$DPTO_CNMBR %in% c('VALLE DEL CAUCA', 'CAUCA', 'CHOCÓ', 'QUINDÍO', 'RISARALDA', 'CALDAS'),]

# Shake map ---------------------------------------------------------------
fles <- dir_ls('./tif/raster', regexp = '.flt$')
fles <- as.character(fles)

## MMI: Modified Mercalli Intensity Escala I-X [adimensional]
mmim <- read_grid('./tif/raster/mmi_mean.flt')  
mmim <- mmim * 1
mmis <- read_grid('./tif/raster/mmi_std.flt')

## Match vector CRS to the grid, then crop + mask to the affected zone
zone <- st_transform(zone, crs(mmim))
vzon <- vect(zone)
mmim <- mask(crop(mmim, vzon), vzon)
mmis <- mask(crop(mmis, vzon), vzon)

## Panel A: mean intensity with isoseismal contours
gmea <- ggplot() +
  geom_spatraster(data = mmim, aes(fill = mmi_mean)) +
  scale_fill_gradientn(
    colors = rev(brewer.pal(n = 9, name = 'RdYlGn')), na.value = 'transparent'
  ) +
  geom_spatraster_contour(data = mmim, breaks = 2:9, color = 'grey25', linewidth = 0.2) +
  geom_sf(data = zone, fill = NA, color = 'grey20', linewidth = 0.3) +
  geom_sf(data = dpto, fill = NA, color = 'grey10', linewidth = 0.3) +
  annotation_scale(
    location = 'br',
    height = unit(0.15, 'cm')) +
  annotation_north_arrow(
    location = 'tl', height = unit(1, 'cm'), width = unit(1, 'cm'),
    style = north_arrow_fancy_orienteering()
  ) +
  coord_sf(
    ylim = c(2, 8),
    xlim = c(-79, -74)
  ) +
  labs(title = 'Intensidad modelada (MMI)', subtitle = 'ShakeMap — media',
       caption = 'Fuente: USGS - ShakeMap', fill = 'Modified Mercalli Intensity') +
  theme_bw(base_size = 11, base_family = 'Segoe UI') +
  theme(
    # legend.position    = 'bottom',
    # legend.key.height  = unit(0.5, 'line'),
    # legend.key.width   = unit(2.4, 'line'),
    # plot.title         = element_text(face = 'bold', hjust = 0.5),
    # plot.subtitle      = element_text(hjust = 0.5, face = 'bold'),
    # plot.caption       = element_text(hjust = 0.5, size = 8, color = 'grey40'),
    # legend.title       = element_text(hjust = 0.5, face = 'bold'),
    # legend.title.position = 'top',
    # panel.grid         = element_line(linewidth = 0.15, color = 'grey85'),
    # panel.border       = element_rect(color = 'grey30', linewidth = 0.4),
    # plot.title.position = 'plot'
    plot.background = element_rect(fill = '#f4ecd8', color = NA),
    panel.background = element_rect(fill = '#f4ecd8', color = NA),
    panel.border = element_rect(color = '#5a4d41', linewidth = 1),
    panel.grid = element_line(color = '#e3dad0', linewidth = 0.3),
    plot.title = element_text(color = '#2c221e', face = 'bold', size = 16, hjust = 0.5),
    plot.subtitle = element_text(color = '#5a4d41', size = 11, hjust = 0.5, margin = margin(b = 10)),
    plot.caption = element_text(color = '#5a4d41', size = 9, hjust = 0.5),
    legend.position = 'bottom',
    # legend.direction = 'vertical',
    # legend.box = 'vertical',
    # legend.box.just = 'right',
    legend.key.width = unit(2.5, 'line'),
    legend.background = element_rect(fill = '#f4ecd8', color = '#5a4d41', linewidth = 0),
    # legend.key = element_rect(fill = NA, color = NA),
    legend.text = element_text(color = '#2c221e', size = 8),
    legend.title = element_text(color = '#2c221e', face = 'bold', size = 9),
    plot.margin = margin(20, 20, 20, 20), 
    axis.text.y = element_text(angle = 90, hjust = 0.5)
  )
gmea

ggsave(
  plot = gmea, 
  filename = './mapa_shakeMap_v1.png', units = 'in', width = 6.5, height = 8, dpi = 300, create.dir = T
)

##

gstd <- ggplot() +
  geom_spatraster(data = mmis, aes(fill = mmi_std)) +
  # scale_fill_viridis_c(option = 'mako', direction = -1, na.value = 'transparent') +
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'Spectral')[1:9], na.value = 'transparent') +
  geom_spatraster_contour(data = mmis, breaks = seq(0.2, 1.4, 0.2),
                          color = 'grey25', linewidth = 0.2) +
  geom_sf(data = zone, fill = NA, color = 'grey20', linewidth = 0.3) +
  geom_sf(data = dpto, fill = NA, color = 'grey10', linewidth = 0.3) +
  annotation_scale(
    location = 'br',
    height = unit(0.15, 'cm')) +
  annotation_north_arrow(
    location = 'tl', height = unit(1, 'cm'), width = unit(1, 'cm'),
    style = north_arrow_fancy_orienteering()
  ) +
  coord_sf(
    ylim = c(2, 8),
    xlim = c(-79, -74)
  ) +
  labs(title = 'Incertidumbre (σ)', subtitle = 'ShakeMap — desviación estándar',
       caption = 'Fuente: USGS - ShakeMap', fill = 'Desv. estándar (MMI)') +
  theme_bw(base_size = 11, base_family = 'Segoe UI') +
  theme(
    legend.position    = 'bottom',
    legend.key.height  = unit(0.5, 'line'),
    legend.key.width   = unit(2.4, 'line'),
    plot.title         = element_text(face = 'bold', hjust = 0.5),
    plot.subtitle      = element_text(hjust = 0.5, face = 'bold'),
    plot.caption       = element_text(hjust = 0.5, size = 8, color = 'grey40'),
    legend.title       = element_text(hjust = 0.5, face = 'bold'),
    legend.title.position = 'top',
    panel.grid         = element_line(linewidth = 0.15, color = 'grey85'),
    panel.border       = element_rect(color = 'grey30', linewidth = 0.4),
    plot.title.position = 'plot'
  )
gstd