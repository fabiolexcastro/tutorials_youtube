

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, grid, rgdal, png, sf, hrbrthemes, ggspatial, ggthemes, rgeos, RColorBrewer, terra, sf, geodata, tidyverse, terra, devtools)

# Install analogues
install_github('CIAT-DAPA/analogues')
library(analogues)

# World -------------------------------------------------------------------
wrld <- st_read('D:/data/WORLD/all_countries.shp') %>% filter(CONTINENT == 'South America')

# Colombia
col <- shapefile('D:/data/IGAC/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')

# Coordinate  -------------------------------------------------------------

lon <- -77
lat <- 3.5
pnt <- tibble(lon = lon, lat = lat)

# Download climate
prec <- geodata::worldclim_country(country = 'COL', var = 'prec', path = '../tmpr')
tavg <- geodata::worldclim_country(country = 'COL', var = 'tavg', path = '../tmpr')

prec <- raster::stack(prec)
tavg <- raster::stack(tavg)

params <- createParameters(x = lon, y = lat, vars = c("prec","tmean"), weights = c(0.5,0.5),
                           ndivisions = c(12,12), growing.season=c(1,12), rotation = "tmean", threshold=1,
                           env.data.ref = list(prec,tavg), env.data.targ = list(prec,tavg),
                           outfile = "~/.", fname = NA, writefile = FALSE)

# Calculate similarity
sim_out <- calc_similarity(params)

# Now you can plot the result
plot(sim_out)

# Or save the result
writeRaster(sim_out, "../data/analogues_output.tif", overwrite = T)

rstr <- raster('../data/analogues_output.tif')
rstr <- sim_out

# Make the map ------------------------------------------------------------
tble <- rasterToPoints(rstr) %>% 
  as_tibble() %>% 
  setNames(c('lon', 'lat', 'value'))

gmap <- ggplot() + 
  geom_tile(data = tble, aes(x = lon, y = lat, fill = value)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'RdYlGn')) +
  geom_point(data = pnt, aes(x = lon, y = lat)) +
  labs(x = 'Lon', y = 'Lat', fill = 'Puntaje ') + 
  geom_sf(data = wrld, fill = NA, col = 'grey70') +
  geom_sf(data = st_as_sf(col), fill = NA, col = 'grey40') +
  coord_sf(xlim = extent(col)[1:2], ylim = extent(col)[3:4]) +
  ggtitle(label = 'Similaridad climática para la coordenada', 
          subtitle = '-77 - 3.5') +
  theme_pander() +
  theme(axis.text.x = element_text(family = 'sans'), 
        axis.text.y = element_text(family = 'sans', angle = 90, hjust = 0.5),
        axis.title.x = element_text(family = 'sans'), 
        axis.title.y = element_text(family = 'sans'),
        plot.title = element_text(face = 'bold', hjust = 0.5, family = 'sans'),
        plot.subtitle = element_text(face = 'bold', hjust = 0.5, family = 'sans'),
        legend.title = element_text(family = 'sans', face = 'bold'),
        legend.position = 'bottom',
        legend.key.width = unit(4.8, 'line'),
        legend.key.height = unit(0.8, 'line'),
        legend.text = element_text(family = 'sans'),
        plot.caption = element_text(face = 'bold', family = 'sans'),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.1)) +
  annotation_scale(location =  "br", width_hint = 0.5, text_family = 'sans', text_col = 'white') +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'sans', text_col = 'white')) +
  annotate(geom = "text", x = -82, y = 13.7, hjust = 0, vjust = 1, 
           label = 'Elaboró: Fabio Castro / YouTube: Un Geógrafo en YouTube',
           size = 3, family = "sans", color = "black") 

ggsave(plot = gmap, filename = '../png/map_similarity.png', 
       units = 'in', width = 7, height = 10, dpi = 300)


