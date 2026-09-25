

## Source: https://www.sgc.gov.co/catalogo
## Shake Map: https://earthquake.usgs.gov/earthquakes/eventpage/us6000tjl2/shakemap/intensity

# Load libraries ----------------------------------------------------------
require(pacman)
p_load(
  geodta, terra, fs, sf, geodata, cowplot, janitor, leaflet, tidyverse, gtools, stringr, glue, RColorBrewer, ggspatial
)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Vector data -------------------------------------------------------------
wrld <- geodata::world(resolution = 1, path = './tmpr')
wrld <- st_as_sf(wrld)

# Load data ---------------------------------------------------------------
data <- dir_ls('./', regexp = '.csv$') 
data <- read_csv(data, show_col_types = FALSE)
dpto <- st_read('./shp/MGN_DPTO_POLITICO.shp')
zone <- dpto[dpto$DPTO_CNMBR %in% c('VALLE DEL CAUCA', 'CAUCA', 'CHOCÓ', 'QUINDÍO', 'RISARALDA', 'CALDAS'),]

# Shake map ---------------------------------------------------------------
fles <- dir_ls('./tif/raster', regexp = '.flt$')
fles <- as.character(fles)

## MMI: Modified Mercalli Intensity Escala I-X [adimensional]
mmia <- rast(grep('mmi_mean', fles, value = T))
mmia <- mmia * 1
rstr <- terra::rast('./tif/raster/pga_mean.flt')

## PGA - Peak Ground Acceleration, aceleración máxima del suelo, en %g (porcentaje de gravedad).
## PGV - Peak Ground Velocity, velocidad máxima del suelo, en cm/s. Correalciona con daños en estructuras flexibles y de periodo intermedio. 
## PSA 0.3 / 0.6 / 1.0 / 3.0 — Pseudo-Spectral Acceleration a esos períodos estructurales (en segundos), en %g. Es la respuesta de un oscilador de un grado de libertad a cada período: 
## aproximando período ≈ 0.1 × n.º de pisos, psa0p3 te habla de edificaciones bajas (~3 pisos), 
## psa1p0 de edificios medios (~10 pisos) y psa3p0 de estructuras altas o muy flexibles (puentes, 
## torres). Es el insumo de ingeniería sísmica.

# Filter by magnitude -----------------------------------------------------
sism <- data |> 
  clean_names() |> 
  rename(
    lati = latitude,
    long = longitude,
    dpth = depth,
    magn = magnitude
  ) |> 
  filter(magn > 3, !is.na(lati), !is.na(long))

# To sf points (WGS84) ----------------------------------------------------
pnts <- st_as_sf(x = sism, coords = c('long', 'lati'), crs = 4326)

# Epicenter map -----------------------------------------------------------

g_macro <- ggplot() +
  geom_sf(data = dpto, fill = '#dfd3c3', color = '#8c7b6c', linewidth = 0.1) +
  geom_sf(data = wrld, fill = NA, color = 'grey40') +
  geom_rect(aes(xmin = -78.2, xmax = -75.2, ymin = 3.0, ymax = 5.5), 
            fill = '#c0392b', alpha = 0.6, color = '#962d22', linewidth = 0.5) +
  coord_sf(xlim = c(-80, -66), ylim = c(-4.5, 13)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = '#f4ecd8', color = '#5a4d41', linewidth = 0.8),
    plot.background = element_rect(fill = NA, color = NA)
  )

gmap_poster <- ggplot() +
  geom_sf(data = dpto, fill = '#dfd3c3', color = '#8c7b6c', linewidth = 0.3) +
  geom_sf(data = pnts, aes(size = magn, color = dpth), alpha = 0.9) +
  scale_color_viridis_c(option = 'C', direction = -1, name = 'Prof. (km)') +
  scale_size_continuous(range = c(3, 8), name = 'Magnitud') +
  annotation_scale(location = 'bl', text_col = '#2c221e', line_col = '#2c221e', pad_x = unit(0.5, 'cm'), pad_y = unit(0.5, 'cm')) +
  annotation_north_arrow(location = 'tr', style = north_arrow_fancy_orienteering(
    text_col = '#2c221e', line_col = '#2c221e', fill = c('#2c221e', '#f4ecd8')
  )) +
  coord_sf(xlim = c(-78.2, -75.2), ylim = c(3.0, 5.5)) +
  labs(
    title = 'Sismicidad Agosto 2026 - Chocó',
    subtitle = 'Sur del Chocó y Valle del Cauca',
    caption = 'Fuente: Servicio Geológico Colombiano'
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.background = element_rect(fill = '#f4ecd8', color = NA),
    panel.background = element_rect(fill = '#f4ecd8', color = NA),
    panel.border = element_rect(color = '#5a4d41', linewidth = 1),
    panel.grid = element_line(color = '#e3dad0', linewidth = 0.3),
    plot.title = element_text(color = '#2c221e', face = 'bold', size = 16, hjust = 0.5),
    plot.subtitle = element_text(color = '#5a4d41', size = 11, hjust = 0.5, margin = margin(b = 10)),
    plot.caption = element_text(color = '#5a4d41', size = 9, hjust = 0.5),
    legend.position = c(0.93, 0.3),
    legend.direction = 'vertical',
    legend.box = 'vertical',
    legend.box.just = 'right',
    legend.background = element_rect(fill = '#f4ecd8', color = '#5a4d41', linewidth = 0.5),
    legend.key = element_rect(fill = NA, color = NA),
    legend.text = element_text(color = '#2c221e', size = 8),
    legend.title = element_text(color = '#2c221e', face = 'bold', size = 9),
    plot.margin = margin(20, 20, 20, 20), 
    axis.text.y = element_text(angle = 90, hjust = 0.5)
  )

mapa_final <- ggdraw(gmap_poster) +
  draw_plot(
    g_macro, 
    x = 0.1, 
    y = 0.62, 
    width = 0.23, 
    height = 0.23
  )

ggsave(
  plot = mapa_final, 
  filename = './mapa_zoom_v1.png', units = 'in', width = 9, height = 8, dpi = 300, create.dir = T
)

# Count per department (join by spatial containment) ----------------------
cnts <- st_join(pnts, dpto['DPTO_CNMBR']) |> 
  st_drop_geometry() |> 
  count(DPTO_CNMBR, name = 'nsis')

dpto <- left_join(dpto, cnts, by = 'DPTO_CNMBR') |> 
  mutate(nsis = replace_na(nsis, 0))

# gcho <- ggplot(dpto) +
#   geom_sf(aes(fill = nsis), color = 'white', linewidth = 0.2) +
#   scale_fill_distiller(palette = 'YlOrRd', direction = 1, name = 'N.º sismos') +
#   labs(title = 'Número de sismos por departamento') +
#   theme_minimal(base_size = 11)


