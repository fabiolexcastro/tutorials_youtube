
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, glue, distanceto, fasterize, ggspatial, raster, tidyverse, cptcity, glue)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
accs <- vect('D:/data/spatial/igac/vias.gpkg')
mpio <- vect('D:/data/spatial/igac/mpios.gpkg')
dpto <- vect('D:/data/spatial/igac/dptos.gpkg')

# Filter to Valle del Cauca -----------------------------------------------
vlle <- mpio[mpio$DPTO_CNMBR == 'VALLE DEL CAUCA',]

dir_create('gpkg')
writeVector(vlle, 'gpkg/valle_mpios.gpkg')

# Filtering roads ---------------------------------------------------------
accs <- accs[accs$TIPO_VIA %in% 1:5,]

# Crop to Valle del Cauca -------------------------------------------------
accs <- terra::crop(accs, vlle)
writeVector(accs, 'gpkg/accesibility.gpkg')

# To project --------------------------------------------------------------
proj <- '+proj=tmerc +lat_0=4.59620041666667 +lon_0=-74.0775079166667 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'
accs <- terra::project(accs, proj)
vlle <- terra::project(vlle, proj)
dpto <- terra::project(dpto, proj)

plot(vlle)
plot(accs, add = TRUE, col = 'red')

trgt <- c('EL ÁGUILA', 'ANSERMANUEVO', 'EL CAIRO', 'ARGELIA', 'CARTAGO', 'ULLOA', 'ALCALÁ', 'VERSALLES', 'TORO', 'OBANDO', 'LA UNIÓN', 'EL DOVIO', 'LA VICTORIA')
zone <- vlle[vlle$MPIO_CNMBR %in% trgt,]

accs <- terra::crop(accs, zone)

# To calc the euclidean distance ------------------------------------------
accs <- st_as_sf(accs)
zone <- st_as_sf(zone)
dpto <- st_as_sf(dpto)
accs <- mutate(accs, gid = 1)
accs <- accs %>% group_by(gid) %>% summarise()

rstr <- distance_raster(y = accs, cellsize = 100, extent = st_bbox(zone), check = FALSE)
rstr <- terra::rast(rstr)
rstr <- terra::crop(rstr, vect(zone))
rstr <- terra::mask(rstr, vect(zone))

dir_create('rstr')
writeRaster(x = rstr, filename = 'rstr/acces_northvalle_proj.tif', overwrite = TRUE)

# To make the map ---------------------------------------------------------
tble <- terra::as.data.frame(rstr, xy = T)
tble <- as_tibble(tble)
colnames(tble)[3] <- 'value'

find_cpt('GMT_hot')
image(matrix(1:100), col = cpt("gmt_GMT_hot"))

gmap <- ggplot() + 
  geom_tile(data = tble, aes(x = x, y = y, fill = value)) + 
  scale_fill_gradientn(colors = cpt(pal = 'gmt_GMT_hot')) +
  # geom_sf(data = st_as_sf(accs), fill = NA, col = 'white') +
  geom_sf(data = dpto, fill = NA, col = 'grey50') +
  geom_sf(data = zone, fill = NA, col = 'grey90') + 
  coord_sf(datum = sf::st_crs(3116), xlim = ext(zone)[1:2], ylim = ext(zone)[3:4]) + 
  ggtitle(label = 'Distancia euclideana para la red víal en el\nNorte del Valle del Cauca') +
  labs(x = '', y = '', fill = 'Distancia\nEuclideana (mt)') +
  theme_bw() +
  theme(legend.position = 'bottom', 
        plot.title = element_text(face = 'bold', hjust = 0.5, colour = 'grey25'),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        text = element_text(colour = 'grey30'),
        panel.grid.major = element_line(colour = 'grey30', linetype = 'dashed'), #, size = 0.1
        panel.grid.minor = element_line(colour = 'grey30', linetype = 'dashed'), #, size = 0.1
        legend.key.width = unit(5, 'line'), 
        legend.key.height = unit(0.4, 'line')) +
  annotation_scale(location =  "bl", width_hint = 0.5, text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.2) + #text_family = 'georg'
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99')))  #v text_family = 'georg'

dir_create('png')
ggsave(plot = gmap, filename = 'gmap/acces_index.png', units = 'in', width = 9, height = 9, dpi = 300)
