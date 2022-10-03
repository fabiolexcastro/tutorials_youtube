
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rmapshaper, MetBrewwer, cowplot, colourpicker, climateStability, ade4, elevatr, geodata, KernSmooth, spatstat, crayon, terra, sf, qs, classInt, tidyverse, ggspatial, hrbrthemes, RColorBrewer, geodata, fs, glue)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# My theme ----------------------------------------------------------------
my_theme <- function(){ theme(axis.text.x = element_text(size = 7, family = 'serif'), 
                  axis.text.y = element_text(size = 7, family = 'serif'), 
                  axis.title.x = element_text(size = 9, face = 'bold', family = 'serif'),
                  axis.title.y = element_text(size = 9, face = 'bold', family = 'serif'), 
                  plot.title = element_text(size = 16, face = 'bold', family = 'serif', hjust = 0.5), 
                  plot.subtitle = element_text(size = 14, face = 'bold', family = 'serif', hjust = 0.5), 
                  plot.caption = element_text(size = 8, family = 'serif'), 
                  legend.title = element_text(size = 9, family = 'serif', face = 'bold'),
                  legend.text = element_text(size = 10, family = 'serif'))}

# Load data ---------------------------------------------------------------
cttm <- st_read('./gpgk/catatumbo.gpkg')
tree <- terra::rast('./tmpr/Hansen_GFC-2019-v1.7_treecover2000_10N_080W.tif') %>% terra::crop(., vect(cttm)) %>% terra::mask(., vect(cttm))
loss <- read_csv('tble/loss_2021.csv')
krnl <- c(terra::rast('./rstr/kernel01.tif'), terra::rast('./rstr/kernel02.tif'))
names(krnl) <- c('kernel_01', 'kernel_02')
tble <- as.data.frame(krnl, xy = TRUE) %>% as_tibble()

mpio <- st_read('D:/data/spatial/igac/mpios.gpkg')
tree <- terra::as.data.frame(tree, xy = T) %>% as_tibble() %>% setNames(c('x', 'y', 'value'))

# Intersection ------------------------------------------------------------
cttm_mpio <- st_intersection(cttm, mpio)
cttm_mpio <- filter(mpio, MPIO_CNMBR %in% pull(cttm_mpio, MPIO_CNMBR))

# To make the maps --------------------------------------------------------

# Loss treecover
gloss <- ggplot() + 
  geom_sf(data = cttm_mpio, fill = NA, col = 'grey90', lwd = 0.4) + 
  geom_sf(data = cttm, fill = NA, col = 'grey20', lwd = 0.9) + 
  geom_point(data = loss, aes(x = x, y = y), col = '#6B4B21CF', size = 0.01) +
  geom_sf_text(data = cttm_mpio, aes(label = str_to_title(MPIO_CNMBR)), family = 'serif', col = 'grey50') + 
  ggtitle(label = 'Zonas con pérdida de cobertura boscosa', subtitle = 'Cuenca: Catatumbo') +
  coord_sf(xlim = ext(cttm)[1:2], ylim = ext(cttm)[3:4]) + 
  labs(x = 'Lon', y = 'Lat') + 
  theme_minimal() +
  my_theme()

# Treecover 
gtree <- ggplot() + 
  geom_tile(data = tree, aes(x = x, y = y, fill = value)) + 
  scale_fill_gradientn(colors = MetBrewer::met.brewer(name = 'Nattier', n = 7)) + 
  geom_sf(data = cttm, fill = NA, col ='grey20', lwd = 0.9) + 
  geom_sf(data = cttm_mpio, fill = NA, col = 'grey90', lwd = 0.4) + 
  coord_sf(xlim = ext(cttm)[1:2], ylim = ext(cttm)[3:4]) + 
  ggtitle(label = 'Cobertura boscosa') +
  labs(x = 'Lon', y = 'Lat', fill = 'Bosque (%)') +
  theme_minimal() + 
  my_theme() + 
  theme(legend.position = c(0.9, 0.3),
        legend.key.height = unit(1.5, 'line'))

# Density
gdnst <- ggplot() + 
  geom_tile(data = tble, aes(x = x, y = y, fill = kernel_02), alpha = 0.9) + 
  scale_fill_gradientn(colors = MetBrewer::met.brewer(name = 'Nattier', n = 7)) + 
  geom_sf(data = cttm, fill = NA, col ='grey20', lwd = 0.9) + 
  geom_sf(data = cttm_mpio, fill = NA, col = 'grey90', lwd = 0.4) + 
  coord_sf(xlim = ext(cttm)[1:2], ylim = ext(cttm)[3:4]) + 
  labs(x = 'Lon', y = 'Lat', fill = 'Bosque (%)') +
  ggtitle(label = 'Densidad de zonas con pérdidas de cobertura boscosa', subtitle = 'Cuenca: Catatumbo') +
  theme_minimal() + 
  my_theme() + 
  theme(legend.position = c(0.9, 0.3),
        legend.key.height = unit(1.5, 'line'))

# Join all the maps into only one 

gallm <- ggdraw() + 
  coord_equal(xlim = c(0, 14), ylim = c(0, 14), expand = FALSE) + 
  draw_plot(gdnst, width = 7, height = 12, x = 0, y = 0) + 
  draw_plot(gtree, width = 3, height = 3, x = 8, y = 0) +
  draw_plot(gloss, width = 3, height = 3, x = 8, y = 8)

ggsave(plot = gallm, filename = './png/all_maps.png', units = 'in', width = 15, height = 15, dpi = 300)

