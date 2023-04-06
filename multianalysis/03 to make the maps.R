
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, glue, ggpubr, cowplot, colourpicker, geodata, ggspatial, RColorBrewer, cptcity)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Windows fonts
windowsFonts(georg = windowsFont('Georgia'))

# Study zone --------------------------------------------------------------
col1 <- vect('G:/D/data/IGAC/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')
limt <- col1[col1$DPTO_CNMBR == 'CUNDINAMARCA',]

# Raster files ------------------------------------------------------------
rstr <- dir_ls('raster_input_v1') %>% as.character()
accs <- grep('accesibility', rstr, value = T) %>% terra::rast()
srtm <- grep('srtm', rstr, value = T) %>% terra::rast()
tavg <- grep('tavg', rstr, value = T) %>% terra::rast() %>% mean()

# Raster to table 
accs.tble <- as_tibble(terra::as.data.frame(accs, xy = TRUE)) 
colnames(accs.tble)[3] <- 'value'

srtm.tble <- as_tibble(terra::as.data.frame(srtm, xy = TRUE))
colnames(srtm.tble)[3] <- 'value'

tavg.tble <- as_tibble(terra::as.data.frame(tavg, xy = TRUE))
colnames(tavg.tble)[3] <- 'value'

# Shapefiles files --------------------------------------------------------
shpf <- dir_ls('shape_input_v1') %>% as.character()
prot <- grep('protected', shpf, value = T) %>% terra::vect()
rvr1 <- grep('doble', shpf, value = T) %>% terra::vect()
rvr2 <- grep('sencillos', shpf, value = T) %>% terra::vect()

# Output result -----------------------------------------------------------
rslt <- terra::rast('raster_output_v1/result_v1.tif')
rslt.tble <- as_tibble(terra::as.data.frame(rslt, xy = TRUE))
colnames(rslt.tble)[3] <- 'value'

# Maps --------------------------------------------------------------------

# Accesibility
g_accs <- ggplot() + 
  geom_tile(data = accs.tble, aes(x = x, y = y, fill = value)) + 
  # scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'YlOrRd')) +
  scale_fill_gradientn(colors = cpt(n = 10, 'fractint_blackmap_Wizzl015')) +
  geom_sf(data = st_as_sf(limt), fill = NA, col = 'grey40') + 
  geom_sf(data = st_as_sf(col1), fill = NA, col = 'grey80', lwd = 0.2) + 
  coord_sf(xlim = ext(limt)[1:2], ylim = ext(limt)[3:4]) + 
  labs(x = 'Lon', y = 'Lat', fill = 'Accesibilidad ciudad 50k habitantes (min)') +
  ggtitle(label = 'Accesibilidad a urbes de más de 50 mil habitantes') +
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        plot.title = element_text(face = 'bold', hjust = 0.5, color = 'grey40'),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7, angle = 90, hjust = 0.5),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        # legend.key.width = unit(3, 'line'), 
        legend.box = 'horizontal',
        text = element_text(family = 'georg'),
        legend.key.height = unit(0.3, 'cm')) + 
  guides(fill = guide_legend(title.position = 'top', title.hjust = 0.5)) +
  annotation_scale(location =  "bl", width_hint = 0.5, text_family = 'georg', text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'georg', text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 

find_cpt('km')
image(matrix(1:100), col = cpt("imagej_topography"))

# SRTM
g_srtm <- ggplot() + 
  geom_tile(data = srtm.tble, aes(x = x, y = y, fill = value)) + 
  scale_fill_gradientn(colors = cpt(n = 10, 'wkp_tubs_nrwc')) +
  geom_sf(data = st_as_sf(limt), fill = NA, col = 'grey40') + 
  geom_sf(data = st_as_sf(col1), fill = NA, col = 'grey80', lwd = 0.2) + 
  coord_sf(xlim = ext(limt)[1:2], ylim = ext(limt)[3:4]) + 
  labs(x = 'Lon', y = 'Lat', fill = 'Altitud\n(m.s.n.m)') +
  ggtitle(label = 'Modelo de elevación digital') +
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        legend.key.width = unit(2.5, 'line'),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7, angle = 90, hjust = 0.5),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        plot.title = element_text(face = 'bold', hjust = 0.5, color = 'grey40'),
        legend.box = 'horizontal',
        text = element_text(family = 'georg'),
        legend.key.height = unit(0.3, 'cm')) +
  # guides(fill = guide_legend(title.position = 'top', title.hjust = 0.5)) +
  annotation_scale(location =  "bl", width_hint = 0.5, text_family = 'georg', text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'georg', text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 

# T Avg
g_tavg <- ggplot() + 
  geom_tile(data = tavg.tble, aes(x = x, y = y, fill = value)) + 
  scale_fill_gradientn(colors = cpt(n = 10, 'arendal_temperature')) +
  geom_sf(data = st_as_sf(limt), fill = NA, col = 'grey40') + 
  geom_sf(data = st_as_sf(col1), fill = NA, col = 'grey80', lwd = 0.2) + 
  coord_sf(xlim = ext(limt)[1:2], ylim = ext(limt)[3:4]) + 
  labs(x = 'Lon', y = 'Lat', fill = 'Temperatura (°C)') +
  ggtitle(label = 'Temperatura promedio anual') +
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7, angle = 90, hjust = 0.5),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        plot.title = element_text(face = 'bold', hjust = 0.5, color = 'grey40'),
        legend.key.width = unit(2.5, 'line'),
        legend.box = 'horizontal',
        text = element_text(family = 'georg'),
        legend.key.height = unit(0.3, 'cm')) + 
  # guides(fill = guide_legend(title.position = 'top', title.hjust = 0.5)) +
  annotation_scale(location =  "bl", width_hint = 0.5, text_family = 'georg', text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'georg', text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 

# Join the three maps into only one 
g_rstr <- ggarrange(g_accs, g_srtm, g_tavg, ncol = 3, nrow = 1)
dir_create('png')
ggsave(plot = g_rstr, filename = 'png/raster_maps.png', units = 'in', width = 15, height = 7, dpi = 300)

# Protected areas
g_prot <- ggplot() + 
  geom_sf(data = st_as_sf(prot), col = '#457D20', fill = '#457D20') + 
  geom_sf(data = st_as_sf(limt), fill = NA, col = 'grey40') + 
  geom_sf(data = st_as_sf(col1), fill = NA, col = 'grey80', lwd = 0.2) + 
  coord_sf(xlim = ext(limt)[1:2], ylim = ext(limt)[3:4]) + 
  labs(x = 'Lon', y = 'Lat', fill = 'Temperatura (°C)') +
  ggtitle(label = 'Áreas protegidas en Cundinamarca') +
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7, angle = 90, hjust = 0.5),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        plot.title = element_text(face = 'bold', hjust = 0.5, color = 'grey40'),
        legend.key.width = unit(2.5, 'line'),
        legend.box = 'horizontal',
        text = element_text(family = 'georg'),
        legend.key.height = unit(0.3, 'cm')) + 
  annotation_scale(location =  "bl", width_hint = 0.5, text_family = 'georg', text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'georg', text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 

# Red hídrica
rvr1 <- st_as_sf(rvr1)
rvr2 <- st_as_sf(rvr2)

rvr2_sf <- st_cast(rvr2, "POINT")
coords <- st_coordinates(rvr2_sf)
coords <- as.data.frame(coords)

g_rvr <- ggplot() + 
  geom_sf(data = st_as_sf(limt), fill = NA, col = 'grey40') + 
  geom_sf(data = st_as_sf(col1), fill = NA, col = 'grey80', lwd = 0.2) + 
  geom_sf(data = rvr1, col = '#3880DE', fill = '#3880DE', lwd = 1, size = 0.0000002) +
  geom_sf(data = rvr2, col = '#3880DE', fill = '#3880DE', lwd = 5, size = 0.1) +
  # geom_line(data = st_coordinates(rvr1), aes(x = X, y = Y), col = '#3880DE', size = 0.1) +
  # geom_path(data = coords, aes(x = X, y = Y), col = '#3880DE', size = 0.000005) +
  coord_sf(xlim = ext(limt)[1:2], ylim = ext(limt)[3:4]) + 
  labs(x = 'Lon', y = 'Lat', fill = 'Temperatura (°C)') +
  ggtitle(label = 'Red hídrica en Cundinamarca') +
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7, angle = 90, hjust = 0.5),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        plot.title = element_text(face = 'bold', hjust = 0.5, color = 'grey40'),
        legend.key.width = unit(2.5, 'line'),
        legend.box = 'horizontal',
        text = element_text(family = 'georg'),
        legend.key.height = unit(0.3, 'cm')) + 
  annotation_scale(location =  "bl", width_hint = 0.5, text_family = 'georg', text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'georg', text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 

g_shpf <- ggarrange(g_prot, g_rvr, ncol = 2, nrow = 1)
dir_create('png')
ggsave(plot = g_shpf, filename = 'png/shapes_maps.png', units = 'in', width = 9, height = 6.3, dpi = 300)


# Join both maps 
g_alld <- ggarrange(g_rstr, g_shpf, ncol = 1, nrow = 2)
ggsave(plot = g_alld, filename = 'png/arrange_maps.png', units = 'in', width = 15, height = 15, dpi = 300)

# Now to make the final map -----------------------------------------------

g_fnal <- ggplot() + 
  geom_tile(data = rslt.tble, aes(x = x, y = y), fill = '#598F1E') + 
  geom_sf(data = st_as_sf(limt), fill = NA, col = 'grey40') + 
  geom_sf(data = st_as_sf(col1), fill = NA, col = 'grey80', lwd = 0.2) + 
  coord_sf(xlim = ext(limt)[1:2], ylim = ext(limt)[3:4]) + 
  labs(x = 'Lon', y = 'Lat', fill = '') +
  ggtitle(label = 'Zonas óptimas para recreación') +
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7, angle = 90, hjust = 0.5),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        plot.title = element_text(face = 'bold', hjust = 0.5, color = 'grey40'),
        legend.box = 'horizontal',
        text = element_text(family = 'georg'),
        legend.key.height = unit(0.3, 'cm')) + 
  annotation_scale(location =  "bl", width_hint = 0.5, text_family = 'georg', text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'georg', text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 

ggsave(plot = g_fnal, filename = 'png/final_map.png', units = 'in', width = 7.5, height = 7, dpi = 300)


