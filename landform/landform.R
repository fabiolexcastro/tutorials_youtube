

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(RSAGA, terra, fs, ggrepel, sf, OpenStreetMap, colourpicker, rnaturalearth, rnaturalearthdata, tidyverse, rgeos, geodata, elevatr, gtools, glue, elevatr)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Download ----------------------------------------------------------------
wrld <- ne_countries(returnclass = 'sf', scale = 50, type = 'countries')
pan0 <- geodata::gadm(country = 'PAN', level = 0, path = './tmpr')
pan1 <- geodata::gadm(country = 'PAN', level = 1, path = './tmpr')
srtm <- geodata::elevation_30s(country = 'PAN', path = './tmpr')

plot(srtm)

# Extract by mask ---------------------------------------------------------
srtm <- terra::crop(srtm, pan0)
srtm <- terra::mask(srtm, pan0)

dir_create('./tif')
terra::writeRaster(x = srtm, filename = './tif/srtm_raw.tif', overwrite = TRUE)

srtm <- rast('./tif/srtm_raw.tif')
srtm <- terra::project(srtm, '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +type=crs')

terra::writeRaster(x = srtm, filename = './tif/srtm_prj.tif', overwrite = TRUE)

srtm <- rast('./tif/srtm_prj.tif')

# Rsaga -------------------------------------------------------------------

# Rsaga Environment
envr <- rsaga.env('C:/SAGA/saga-9.2.0_x64/saga-9.2.0_x64') 

# Rsaga check the parameters
rsaga.get.usage(lib = 'ta_morphometry', env = envr, module = 'TPI Based Landform Classification')

# To execute TPI Based Landform Classification
rsl <- rsaga.geoprocessor(
  lib = 'ta_morphometry',
  module = 'TPI Based Landform Classification',
  param = list(DEM = 'F:/yt/rsaga/landform/tif/srtm_prj.tif',
               LANDFORMS = 'F:/yt/rsaga/landform/tif/landform.tif',
               RADIUS_A_MIN = 0,
               RADIUS_A_MAX = 100.000000,
               RADIUS_B_MIN= 0.000000,
               RADIUS_B_MAX= 1000.000000, 
               DW_WEIGHTING= 0),
  env = envr)

# To read the results
lndf <- terra::rast('./tif/landform.tif')
lndf <- terra::project(lndf, crs(pan0), method = 'near')

# Landform labels 
# Source: http://www.jennessent.com/downloads/tpi-poster-tnc_18x22.pdf
lbls <- tibble(value = 1:10, class = c('Streams', 'Midslope drainages', 'Upland drainages', 'Valleys', 'Plains', 'Open Slopes', 'Upper slopes', 'Local ridges', 'Midslope ridges', 'High ridges'))

# Coordinates  ------------------------------------------------------------
crds <- pan1 %>% terra::centroids() %>% terra::crds() %>% as_tibble() %>% mutate(name_1 = pan1$NAME_1)

# To make the map ---------------------------------------------------------
lndf.tble <- terra::as.data.frame(lndf, xy = T) %>% 
  as_tibble() %>% 
  setNames(c('x', 'y', 'value')) %>% 
  inner_join(., lbls, by = 'value')

# Open Street Maps
ext(pan0)
LAT1 =  7.20235900000011 ; LAT2 = 9.6473610000001
LON1 = -83.0518869819999 ; LON2 = -77.1292800909999
map <- openmap(c(LAT2,LON1), c(LAT1,LON2), zoom = NULL, type = c("osm", "stamen-toner", "stamen-terrain","stamen-watercolor", "esri","esri-topo", 'esri-physical', 'esri-shaded')[8], mergeTiles = TRUE)
map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

gmap <- autoplot(map.latlon) + 
  geom_sf(data = wrld, fill = NA, col = 'grey30', inherit.aes = FALSE) +
  geom_tile(data = lndf.tble, aes(x = x, y = y, fill = class)) + 
  scale_fill_manual(values = c('#BCBFA3', '#A5C2B2', '#BF793F', '#327D3C')) +
  geom_sf(data = st_as_sf(pan0), fill = NA, col = 'grey40', inherit.aes = FALSE) +
  geom_sf(data = st_as_sf(pan1), fill = NA, col = 'grey30', inherit.aes = FALSE) +
  geom_text_repel(data = crds, aes(x = x, y = y, label = name_1), family = 'Gill Sans MT', size = 2, bg.color = 'white', bg.r = 0.25) +
  ggtitle(label = 'Landform classification / Topograhic Position Index', 
          subtitle = 'Panama') +
  coord_sf(xlim = ext(pan0)[1:2], ylim = ext(pan0)[3:4]) +
  labs(x = 'Lon', y = 'Lat', fill = 'Landform', caption = 'CSI - SRTM') +
  theme_minimal() +
  theme(legend.position = 'bottom', 
        plot.title = element_text(face = 'bold', hjust = 0.5), 
        plot.subtitle = element_text(face = 'bold', hjust = 0.5), 
        text = element_text(family = 'Gill Sans MT'),
        axis.text.y = element_text(angle = 90, hjust = 0.5))  +
  guides(fill = guide_legend( 
    title.position = 'top',
    title.hjust = 0.5,
    nrow = 1
  ))

gmap

ggsave(plot = gmap, filename = './map_landform_pan.png', units = 'in', width = 12, height = 6, dpi = 300)

 