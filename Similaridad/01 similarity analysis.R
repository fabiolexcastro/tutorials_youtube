

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, devtools, rgeos, stringr, ggspatial, RColorBrewer, tidyverse, ENMeval, glue, gtools, fs, rgbif, sf)

g <- gc(reset = TRUE)
rm(list = ls())

devtools::install_github("BlakeRMills/MetBrewer") 
library(MetBrewer)

# Load data ---------------------------------------------------------------
cntr <- st_read('../shp/all_countries.shp')
cntr
mexc <- filter(cntr, ENGLISH == 'Mexico')
mex1 <- st_as_sf(raster::getData(name = 'GADM', country = 'MEX', level = 1))
amer <- filter(cntr, CONTINENT == 'North America')

# Download RGBIF species -------------------------------------------------
spce <- 'Pinus durangensis'
occr <- occ_data(scientificName = spce, 
                 limit = 200000, 
                 hasCoordinate = TRUE, 
                 hasGeospatialIssue = FALSE)
occr <- occr$data

sftr <- st_as_sf(occr, coords = c('decimalLongitude', 'decimalLatitude'), crs = st_crs(4326))

# Select the main columns
unique(occr$country)
occr <- dplyr::select(occr, scientificName, country, decimalLatitude, decimalLongitude,occurrenceStatus)

# Filtering just Mexico
occr <- filter(occr, country == 'Mexico')
nrow(occr)

# A simple plot
plot(st_geometry(mexc))
points(occr$decimalLongitude, occr$decimalLatitude, pc = 16, col = 'red')

st_centroid(mexc) |> st_coordinates() |> as.data.frame()
mexc <- as(mexc, 'Spatial')

# Download climate data ---------------------------------------------------
bio1 <- raster::getData(name = 'worldclim', var = 'bio', res = 0.5, lon = -102, lat = 25)
bio1 <- raster::crop(bio1, mexc) |> raster::mask(mexc)
bio2 <- raster::getData(name = 'worldclim', var = 'bio', res = 0.5, lon = -102, lat = 35)
bio2 <- raster::crop(bio2, mexc) |> raster::mask(mexc)
bio3 <- raster::getData(name = 'worldclim', var = 'bio', res = 0.5, lon = -80, lat = 10)
bio3 <- raster::crop(bio3, mexc) |> raster::mask(mexc)
bios <- list(bio1, bio2, bio3)
bios$fun <- mean
bios$na.rm <- TRUE

# Mosaicking
bios <- do.call(mosaic, bios)

# Write these rasters
Map('writeRaster', x = unstack(bios), filename = glue('../tif/climate/bio_{1:19}.tif'), overwrite = TRUE)

# Make the similarity -----------------------------------------------------
refr <- raster::extract(bios, occr[,c('decimalLongitude', 'decimalLatitude')])
smml <- ENMeval::similarity(x = bios, ref = refr, full = TRUE)

plot(smml$mos)
levelplot(mess$mos, col.regions=brewer.pal(8, 'Set1'))
levelplot(mess$mod, col.regions=brewer.pal(8, 'Set1'))

# Most similarity ----------------------------------------------------------
moss <- smml$mos
plot(moss)

# Less similarity ---------------------------------------------------------
less <- smml$mod
plot(smml$mod)

tble <- raster::stack(less, moss) |> rasterToPoints() |> as_tibble() |> setNames(c('x', 'y', 'less', 'moss'))
lbls <- tibble(variable = glue('bio {1:19}'), value = 1:19)
tble <- gather(tble, var, value, -x, -y)
tble <- inner_join(tble, lbls, by = 'value')
tble <- mutate(tble, var = factor(var, levels = c('less', 'moss')))
tble <- mutate(tble, variable = factor(variable, levels = paste0('bio ', 1:19)))

#  To make the map --------------------------------------------------------

# Mayor Similaridad
gmss <- ggplot() + 
  geom_sf(data = amer, fill = "white", col = 'grey') + 
  geom_tile(data = filter(tble, var == 'moss'), aes(x = x, y = y, fill = variable)) +
  scale_fill_manual(values = met.brewer("Signac", 19)) +
  geom_sf(data = sftr, col = 'black', size = 1.2) +
  geom_sf(data = mex1, fill = NA, col = 'white') +
  geom_sf_text(data = amer, aes(label = SPANISH), size = 1.2) +
  ggtitle(label = 'Mayor similaridad para las variables bioclimáticas', 
          subtitle = spce) +
  theme_bw() + 
  theme(legend.position = c(0.25, 0.08), 
        plot.subtitle = element_text(face = 'italic', size = 12),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.1),
        panel.background = element_rect(fill = '#63799B'), 
        plot.title = element_text(size = 14, face = 'bold')) + 
  coord_sf(xlim = extent(mexc)[1:2], ylim = extent(mexc)[3:4]) + 
  labs(x = 'Longitud', y = 'Latitud', fill = '') +
  guides(fill = guide_legend(ncol = 7, nrow = 3)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), # 0.2 # 0.3
                         style = north_arrow_fancy_orienteering)

ggsave(plot = gmss, filename = '../png/moss.png', units = 'in', width = 12, height = 8.5, dpi = 300)

# Menor Similaridad
gdss <- ggplot() + 
  geom_sf(data = amer, fill = "white", col = 'grey') + 
  geom_tile(data = filter(tble, var == 'less'), aes(x = x, y = y, fill = variable)) +
  scale_fill_manual(values = met.brewer("Signac", 19)) +
  geom_sf(data = sftr, col = 'black', size = 1.2) +
  geom_sf(data = mex1, fill = NA, col = 'white') +
  geom_sf_text(data = amer, aes(label = SPANISH), size = 1.2) +
  ggtitle(label = 'Menor similaridad para las variables bioclimáticas', 
          subtitle = spce) +
  theme_bw() + 
  theme(legend.position = c(0.25, 0.08), 
        plot.subtitle = element_text(face = 'italic', size = 12),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.1),
        panel.background = element_rect(fill = '#63799B'), 
        plot.title = element_text(size = 14, face = 'bold')) + 
  coord_sf(xlim = extent(mexc)[1:2], ylim = extent(mexc)[3:4]) + 
  labs(x = 'Longitud', y = 'Latitud', fill = '') +
  guides(fill = guide_legend(ncol = 7, nrow = 3)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), # 0.2 # 0.3
                         style = north_arrow_fancy_orienteering)

ggsave(plot = gdss, filename = '../png/diss.png', units = 'in', width = 12, height = 8.5, dpi = 300)





