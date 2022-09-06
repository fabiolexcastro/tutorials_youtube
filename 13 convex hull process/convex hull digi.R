


# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(rgbif, geodata, terra, sf, fs, glue, tidyverse, ggspatial, rnaturalearthdata, rnaturalearth)

g <- gc(reset = T)
options(scipen = 999, warn = -1)
rm(list = ls())

# Load data ---------------------------------------------------------------
spce <- 'Digitonthophagus gazella'
occr <- occ_data(scientificName = spce, limit = 2e5, hasCoordinate = T, hasGeospatialIssue = F)

occr <- occr[[2]]

# Countries ---------------------------------------------------------------
wrld <- ne_countries(scale = 50, returnclass = 'sf')
wrld <- dplyr::select(wrld, name, region_un, subregion, continent, su_a3)

# Get the countries for the presences -------------------------------------

plot(st_geometry(wrld))
points(occr$decimalLongitude, occr$decimalLatitude, pch = 16, col = 'red')

occr <- filter(occr, country == 'Colombia')

# Download Administrative Data for Colombia ------------------------------
col1 <- geodata::gadm(country = 'COL', level = 1, path = 'tmpr')
col1 <- st_as_sf(col1)

plot(st_geometry(col1), border = 'green')
points(occr$decimalLongitude, occr$decimalLatitude, pch = 16, col = 'red')

# Table to sf -------------------------------------------------------------
occr <- st_as_sf(x = occr, coords = c('decimalLongitude', 'decimalLatitude'), crs = st_crs(4326))
occr_col1 <- st_intersection(x = occr, y = col1)
colnames(occr_col1)

# Removing presences from San Andres Islas
occr_col1$NAME_1
occr_col1 <- filter(occr_col1, NAME_1 != 'San Andrés y Providencia')

# Checking the points
plot(st_geometry(col1), border = 'green')
plot(st_geometry(occr_col1), pch = 16, col = 'red', add = T)

# To calculate the convex hull --------------------------------------------
occr_col1 <- mutate(occr_col1, GID = 1)

hull <- occr_col1 %>% 
  dplyr::group_by(GID) %>% 
  dplyr::summarise() %>% 
  st_cast('POLYGON') %>% 
  st_convex_hull()

plot(st_geometry(col1), col = 'grey40')
plot(st_geometry(occr_col1), add = TRUE, col = 'red', pch = 16)
plot(st_geometry(hull), add = T, col = 'red')

# To make the map ---------------------------------------------------------
gmap <- ggplot() + 
  geom_sf(data = wrld, fill = NA, col = 'grey60', lwd = 0.3) +
  geom_sf(data = col1, fill = NA, col = 'grey55', lwd = 0.6)  + 
  geom_sf(data = occr_col1, col = 'brown', fill = NA) + 
  geom_sf(data = hull, col = 'springgreen2', fill = NA) + 
  coord_sf(xlim = ext(col1)[1:2], ylim = c(-5, 13)) +
  labs(x = 'Lon', y = 'Lat', fill = '', source = 'Presencias tomadas de GBIF, 2022') + 
  ggtitle(label = 'Convex Hull para las presencias', subtitle = spce) +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0.5), 
        plot.subtitle = element_text(size = 14, face = 'bold.italic', hjust = 0.5))  +
  annotation_north_arrow(style = north_arrow_fancy_orienteering(), location = 'tl') +
  annotation_scale(location = 'bl')

ggsave(plot = gmap, filename = 'png/mapa_convex_hull.png', units = 'in', width = 6, height = 8.5, dpi = 300)










