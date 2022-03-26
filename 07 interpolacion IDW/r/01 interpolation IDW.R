

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, geodata, tidyverse, gtools, RSAGA, sf, fs)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Theming -----------------------------------------------------------------
source('00 theme.R')

# Load data ---------------------------------------------------------------
tble <- read_csv('../data/tbl/tmax_ene_ave.csv')

# World -------------------------------------------------------------------
wrld <- st_read('D:/data/WORLD/all_countries.shp')
gtml <- filter(wrld, ENGLISH == 'Guatemala')
gtml <- vect(gtml)

# Download SRTM -----------------------------------------------------------
gtml <- geodata::gadm(country = 'GTM', level = 1, path = '../data/shp')
srtm <- geodata::elevation_30s(country = 'GTM', level = 1, path = '../data/tif/srtm')
gtm0 <- geodata::gadm(country = 'GTM', level = 0, path = '../data/shp')

# Get the values for the pnts ---------------------------------------------
tble <- mutate(tble, srtm = terra::extract(srtm, tble[,c('lon', 'lat')])[,2])

# Project -----------------------------------------------------------------
geog <- '+proj=longlat +datum=WGS84 +no_defs'
proj <- '+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs'
pnts <- tble
pnts <- terra::vect(pnts, c('lon', 'lat'))
terra::crs(pnts) <- geog
pnts <- terra::project(x = pnts, y = proj)
pnts <- pnts[!is.na(pnts$srtm),]
gtml <- terra::project(x = gtml, y = proj)

# IDW ---------------------------------------------------------------------
x.range <- terra::ext(gtml)[1:2]
y.range <- terra::ext(gtml)[3:4]
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 5000),
                   y = seq(from = y.range[1], to = y.range[2], by = 5000))
coordinates(grd) <- ~ x + y
gridded(grd) <- TRUE
raster::crs(grd) <- proj

pnts <- as(pnts, 'Spatial')

idw.d <- gstat::idw(base ~ 1, pnts, grd)
idw.d <- raster::raster(idw.d)
idw.d <- rast(idw.d)
idw.d <- terra::crop(idw.d, gtml)
idw.d <- terra::mask(idw.d, gtml)
plot(idw.d)
idw.d <- terra::project(idw.d, geog)

terra::writeRaster(x = idw.d, filename = '../data/tif/tasm/tas_idwd.tif', overwrite = T)
terra::writeRaster(x = srtm, filename = '../data/tif/srtm/srtm.tif', overwrite = T)

# GWR ---------------------------------------------------------------------
env <- rsaga.env(path = 'C:/saga-8.0.0_x64')
fle.srt <- '../data/tif/srtm/srtm.tif'
fle.idw <- '../data/tif/tasm/tas_idwd.tif'
fle.out <- '../data/tif/tasm/tas_idwd_gwr.tif'

rsl <- rsaga.geoprocessor(
  lib = 'statistics_regression',
  module = 'GWR for Grid Downscaling',
  param = list(PREDICTORS = fle.srt,
               REGRESSION = fle.out,
               DEPENDENT = fle.idw),
  env = env)

rst <- terra::rast(fle.out)
plot(rst)
tbl <- as.data.frame(rst, xy = TRUE) %>% as_tibble() %>% setNames(c('x', 'y', 'value'))

# Aspect and hillshade ----------------------------------------------------
srtm <- terra::rast(fle.srt)
srtm <- terra::project(srtm, proj)
trrn <- terra::terrain(x = srtm, v = c('slope', 'aspect'))
hlls <- hillShade(slope = raster(trrn[[1]]), aspect = raster(trrn[[2]])) %>% rast()
hlls <- terra::project(hlls, geog)
hlls.tble <- as.data.frame(hlls, xy = T) %>% as_tibble()
gtml_geog <- terra::project(gtml, geog) %>% st_as_sf()

# Labels ------------------------------------------------------------------
lbls <- gtml_geog %>% st_centroid %>% st_coordinates %>% as_tibble %>% mutate(name = gtml_geog$NAME_1)

# Macrolocalization -------------------------------------------------------
col_bb <- st_as_sfc(st_bbox(st_as_sf(gtml_geog)))
ext <- extent(wrld)

# Logo --------------------------------------------------------------------
logo <- readPNG('../logo/resize.png')
logo <- rasterGrob(logo, x = unit(1.3, "npc"), y = unit(1.3, "npc"), width = unit(1.5, "npc"))

glct <- ggplot() + 
  geom_sf(data = st_as_sf(gtm0), fill = NA, col = 'grey50') +
  geom_sf(data = wrld, fill = NA, col = 'grey60') + 
  geom_sf(data = col_bb, fill = NA, color = 'red', size = 0.5) + 
  theme_bw() +
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.4)) +
  labs(x = '', y = '') +
  coord_sf(xlim = c(-100, -80), ylim = c(5, 30))

gmap <- ggplot() + 
  geom_sf(data = wrld, fill = NA, col = 'grey70') +
  geom_tile(data = tbl, aes(x = x, y = y, fill = value), alpha = 0.75) + 
  scale_fill_gradientn(colors = cpt('jjg_misc_temperature'),
                       limits = round(c(min(tbl$value), max(tbl$value)), 1),
                       breaks = round(seq(min(tbl$value), max(tbl$value), (max(tbl$value)-min(tbl$value))/4),1)) +
  labs(x = 'Lon', y = 'Lat', fill = 'Temperatura (C)') + 
  new_scale_fill() +
  geom_tile(data = hlls.tble, aes(x = x, y = y, fill = layer), alpha = 0.1) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 8, name = 'Greys'), guide = 'none', name = '') +
  geom_sf(data = gtml_geog, fill = NA, col = 'grey60') +
  geom_text_repel(data = lbls, aes(x = X, y = Y, label = name, family = 'Roboto'), size = 6.9) +
  ggtitle(label = 'Temperatura máxima interpolada para Enero', 
          subtitle = 'Promedio multianual 1980-2020') +
  my_theme +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 2.5, text_family = 'Roboto') +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), # 0.2 # 0.3
                         style = north_arrow_fancy_orienteering(text_family = 'Roboto', text_size = 25)) +
  coord_sf(xlim = extent(gtml_geog)[1:2], ylim = extent(gtml_geog)[3:4]) +
  annotate(geom = "text", x = -92, y = 13.7, hjust = 0, vjust = 1, 
           label = 'Elaboró: Fabio Castro / YouTube: Un Geógrafo en YouTube',
           size = 7, family = "Roboto", color = "black") +
  annotation_custom(ggplotGrob(glct), xmin = -92.5, xmax = -91.5, ymin = 17, ymax = 17.8) +
  annotation_custom(logo, xmin = -89.2, xmax = -88.6, ymin = 17, ymax = 17.4)
  
ggsave(plot = gmap, filename = '../png/map_tasm_idw_gwr.png', units = 'in', width = 8, height = 8, dpi = 300)
