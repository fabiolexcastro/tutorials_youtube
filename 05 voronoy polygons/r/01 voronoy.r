

# Load libraries -------------------------------------------------------------
require(pacman)
pacman::p_load(raster, ggpubr, cowplot, rgdal, terra, elevatr, cptcity, RColorBrewer, rgeos, stringr, sf, tidyverse, fs, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

getwd()

# Load data ------------------------------------------------------------------
stts <- read_csv('../data/stations_boyaca.csv')
stts
tble <- read_csv('../data/complete_boyaca.csv')
tble

dpto <- terra::vect('D:/data/spatial/igac/dptos.gpkg')
byca <- dpto[dpto$DPTO_CNMBR == 'BOYACÁ',]
plot(dpto)
plot(byca, add = TRUE, border = 'red')

# Add year, month and day as a individual column
tble <- mutate(tble, year = str_sub(date, start = 1, end = 4))
tble <- mutate(tble, month = str_sub(date, start = 6, end = 7))
tble <- mutate(tble, day = str_sub(date, start = 9, end = 10))
tble

# Filtering for the year 2000
tble <- filter(tble, year == 2000)
tble

# Group by -------------------------------------------------------------------
smm <- tble %>% 
    group_by(code, year) %>% 
    summarise(value = mean(value, na.rm = TRUE)) %>% 
    ungroup()
smm

# Join the table with the stations ------------------------------------------
smm <- inner_join(smm, stts, by = c('code' = 'Code'))
smm <- dplyr::select(smm, code, Variable, year, value, Lon, Lat)
smm
smm$value
smm <- filter(smm, value <= 1000)
smm
vct <- terra::vect(x = smm, c('Lon', 'Lat'))
vct
plot(vct)

# To create a voronoi group -------------------------------------------------
vrn <- terra::voronoi(vct)
vrn <- terra::crop(vrn, byca)
plot(vrn)
vrn

vrn <- st_as_sf(vrn)
dpt <- st_as_sf(dpto)

# Basemap raster -----------------------------------------------------------
srt <- get_elev_raster(as(byca, 'Spatial'), z = 9)
slp <- terrain(srt, opt = 'slope')
asp <- terrain(srt, opt = 'aspect')
hll <- hillShade(slp, asp, angle = 40, direction = 270)
hll.tbl <- as_tibble(rasterToPoints(x = hll, spatial = FALSE))
head(hll.tbl)

# To make the map ----------------------------------------------------------
find_cpt("temperature")
image(matrix(1:100), col = cpt("jjg_misc_temperature"))
cpt('jjg_misc_temperature')

st_crs(vrn) <- st_crs(4326)

gmp <- ggplot() + 
    geom_tile(data = hll.tbl, aes(x = x, y = y, fill = layer), alpha = 0.7, show.legend = FALSE) +
    scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'Greys')) +
    ggnewscale::new_scale_fill() +
    geom_sf(data = vrn, aes(fill = value), alpha = 0.5) + 
    scale_fill_gradientn(colors = cpt('jjg_misc_temperature')) + 
    geom_sf(data = dpt, fill = NA, col = 'grey50') +
    coord_sf(xlim = ext(byca)[1:2], ylim = ext(byca)[3:4]) + 
    geom_point(data = smm, aes(x = Lon, y = Lat), pch = 16) +
    labs(x = 'Lon', y = 'Lat', fill = 'Temperatura (\u00B0C)') +
    ggtitle(label = 'Polígonos de Voronoi para las estaciones de temperatura promedio\nen Boyacá') +
    theme_minimal() + 
    theme(axis.text = element_text(family = 'serif'),
          axis.title = element_text(family = 'serif', face = 'bold'),
          plot.title = element_text(family = 'serif', face = 'bold', hjust = 0.5),
          legend.text = element_text(family = 'serif'), 
          legend.position = 'bottom',
          legend.key.width = unit(3, 'line'),
          legend.title = element_text(family = 'serif', face = 'bold'))

ggsave(plot = gmp, filename = '../png/map2.png', units = 'in', width = 8, height = 7, dpi = 300)
