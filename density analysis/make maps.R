
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, png, grid, rmapshaper, MetBrewer, cowplot, colourpicker, climateStability, ade4, elevatr, geodata, KernSmooth, spatstat, crayon, terra, sf, qs, classInt, tidyverse, ggspatial, hrbrthemes, RColorBrewer, geodata, fs, glue)

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
cttm <- dir_ls('./gpkg') %>% st_read()
tree <- terra::rast('./tmpr/Hansen_GFC-2021-v1.9_treecover2000_10N_080W.tif') %>% terra::crop(., vect(cttm)) %>% terra::mask(., vect(cttm))
loss <- read_csv('tble/loss_2021.csv')
krnl <- c(terra::rast('./rstr/kernel01.tif'), terra::rast('./rstr/kernel02.tif'))
names(krnl) <- c('kernel_01', 'kernel_02')
tble <- as.data.frame(krnl, xy = TRUE) %>% as_tibble()

mpio <- st_read('D:/data/IGAC/MGN2018_MPIO_POLITICO/MGN_MPIO_POLITICO.shp')
tree <- terra::as.data.frame(tree, xy = T) %>% as_tibble() %>% setNames(c('x', 'y', 'value'))

# Intersection ------------------------------------------------------------
cttm_mpio <- st_intersection(cttm, mpio)
cttm_mpio <- filter(mpio, MPIO_CNMBR %in% pull(cttm_mpio, MPIO_CNMBR))

nrts <- filter(mpio, DPTO_CNMBR == 'NORTE DE SANTANDER')

# To make the maps --------------------------------------------------------

# Loss treecover
gloss <- ggplot() + 
  geom_sf(data = nrts, fill = 'grey79', col = 'grey90', lwd = 0.8) + 
  geom_sf(data = cttm, fill = NA, col = 'grey40', lwd = 0.9) + 
  geom_point(data = loss, aes(x = x, y = y), col = '#6B4B21CF', size = 0.01) +
  ggtitle(label = 'Deforestación') +
  coord_sf(xlim = ext(cttm)[1:2], ylim = ext(cttm)[3:4]) + 
  labs(x = 'Lon', y = 'Lat') + 
  theme_minimal() +
  my_theme()

# Treecover 
gtree <- ggplot() + 
  geom_sf(data = nrts, fill = 'grey79', col = 'grey90', lwd = 0.4) + 
  geom_sf(data = cttm, fill = NA, col ='grey40', lwd = 0.9) + 
  geom_tile(data = tree, aes(x = x, y = y, fill = value)) + 
  scale_fill_gradientn(colors = MetBrewer::met.brewer(name = 'VanGogh3', n = 7)) + 
  geom_sf(data = nrts, fill = NA, col = 'grey90', lwd = 0.4) +
  coord_sf(xlim = ext(cttm)[1:2], ylim = ext(cttm)[3:4]) + 
  ggtitle(label = 'Cobertura boscosa') +
  labs(x = 'Lon', y = 'Lat', fill = 'Bosque (%)') +
  theme_minimal() + 
  my_theme() + 
  theme(legend.position = c(0.9, 0.3),
        legend.key.height = unit(2.5, 'line'))

# Density
gdnst <- ggplot() + 
  geom_sf(data = nrts, fill = 'grey79', col = 'grey90', lwd = 0.4) + 
  geom_tile(data = tble, aes(x = x, y = y, fill = kernel_02), alpha = 0.9) + 
  scale_fill_gradientn(colors = MetBrewer::met.brewer(name = 'Nattier', n = 7)) + 
  geom_sf(data = nrts, fill = NA, col = 'grey90', lwd = 0.4) +
  geom_sf(data = cttm, fill = NA, col ='grey20', lwd = 0.9) + 
  coord_sf(xlim = ext(cttm)[1:2], ylim = ext(cttm)[3:4]) + 
  geom_sf_text(data = cttm_mpio, aes(label = str_to_title(MPIO_CNMBR)), fontface = 'bold', family = 'serif', col = 'grey50') + 
  labs(x = 'Lon', y = 'Lat', fill = 'Densidad (z)') +
  ggtitle(label = 'Densidad de zonas con pérdidas de cobertura boscosa', subtitle = '') +
  theme_minimal() + 
  my_theme() + 
  theme(legend.position = c(0.9, 0.3),
        legend.key.height = unit(2.5, 'line'))

# Logo
logo <- './logo/universidad.png'
lexx <- './logo/lex.png'

# Join all the maps into only one 
gallm <- ggdraw() + 
  coord_equal(xlim = c(0, 14), ylim = c(0, 14), expand = FALSE) + 
  draw_plot(gdnst, width = 7, height = 14, x = 0, y = 0.5) + # y = -0.5
  draw_plot(gtree, width = 7, height = 6.8, x = 7.03, y = 0) + # 6 6 8 0
  draw_plot(gloss, width = 7, height = 6.8, x = 7.03, y = 6.85) + # 6 6 8 7
  draw_label("Zonas con pérdida de cobertura boscosa para la cuenca\nCatatumbo en el año 2021. 
             \nFuente: Datos tomados de Hansen\n(Universidad de Maryland)",
             size = 18, lineheight = 0.8, #hjust = 0, x = 0.05, y = 13.5, vjust = 0.9
             fontface = 'bold', fontfamily = 'serif', hjust = 0, x = 0.05, y = 13.5, vjust = 0.9) +
  draw_image(logo, x = 0.9, y = 1.2, width = 1.5, scale = 1, height = 1.5) +
  draw_image(lexx, x = 2.9, y = 1.2, width = 1.5, scale = 1, height = 1.5) + 
  draw_label('Fuente:', x = 1.3, y = 2.9, size = 14, fontface = 'bold', fontfamily = 'serif') +
  draw_label('Autor:', x = 3, y = 2.9, size = 14, fontface = 'bold', fontfamily = 'serif')

ggsave(plot = gallm, filename = './png/densityMap.png', units = 'in', width = 12, height = 12, dpi = 300)
  
