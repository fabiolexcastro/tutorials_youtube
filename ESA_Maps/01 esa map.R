

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, sf, fs, glue, tidyverse, stringr, ggspatial)

# Source: https://www.arcgis.com/apps/instant/media/index.html?appid=fc92d38533d440078f17678ebc20e8e2
# ESA WorldCover 10m v100
# The European Space Agency (ESA) WorldCover 10 m 2020 product provides a global land cover map for 2020
# at 10 m resolution based on Sentinel-1 and Sentinel-2 data. 
# The WorldCover product comes with 11 land cover classes and has been generated in the 
# framework of the ESA WorldCover project, part of the 5th Earth Observation Envelope 
# Programme (EOEP-5) of the European Space Agency.

# Load data ---------------------------------------------------------------
rst <- raster('../tif/18L_20200101-20210101.tif')
lim <- raster::getData(name = 'GADM', country = 'PER', level = 2)
lim <- spTransform(x = lim, CRSobj = crs(rst))
psc <- lim[lim@data$NAME_1 == 'Pasco',]
dnl <- lim[lim@data$NAME_2 == 'Daniel Alcides Carrión',]
nv3 <- raster::getData(name = 'GADM', country = 'PER', level = 3)
nv3 <- spTransform(x = nv3, CRSobj = crs(rst))
nv3 <- nv3[nv3@data$NAME_2 == 'Daniel Alcides Carrión',]

# Extract by mask ---------------------------------------------------------
rst <- raster::crop(rst, dnl)
rst <- raster::mask(rst, dnl)

tbl <- rasterToPoints(rst, spatial = FALSE)
tbl <- as_tibble(tbl)
names(tbl) <- c('x', 'y', 'value')
sort(unique(tbl$value))

lbl <- data.frame(values = factor(1:10, levels = 1:10), 
                  name = c('Bosque', 'Matorral', 'Pradera', 'Cultivo', 'Construcciones', 'Vegetacion escasa', 'Nieve', 'Agua', 'Humedal herbaceo', 'Musco y liquen'),
                  col = c('#006400', '#ffbb22', '#ffff4c', '#f096ff', '#fa0000', '#C7F1BA', '#f0f0f0', '#0064c8', '#0096a0', '#fae6a0'))

tbl <- mutate(tbl, value = factor(value, levels = 1:10))
tbl <- inner_join(tbl, lbl, by = c('value' = 'values'))
tbl <- mutate(tbl, name = factor(name, levels = c('Bosque', 'Matorral', 'Pradera', 'Cultivo', 'Construcciones', 'Vegetacion escasa', 'Nieve', 'Agua', 'Humedal herbaceo', 'Musco y liquen')))

# To make the maps --------------------------------------------------------
gmp <- ggplot() + 
  geom_tile(data = tbl, aes(x = x, y = y, fill = name)) + 
  scale_fill_manual(values = lbl$col) +
  geom_sf(data = st_as_sf(psc), fill = NA) + 
  geom_sf(data = st_as_sf(nv3), fill = NA) + 
  geom_sf_text(data = st_as_sf(nv3), aes(label = NAME_3), col = 'darkgrey') + 
  coord_sf(xlim = extent(dnl)[1:2], ylim = extent(dnl)[3:4]) + 
  labs(x = 'Longitud', y = 'Latitud', fill = 'Cobertura') +
  ggtitle(label = "Cobertura de la tierra para Daniel Alcides Carrión") +
  theme_bw() + 
  theme(legend.position = 'bottom',
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 22, face = 'bold')) + 
  guides(fill = guide_legend(ncol = 5, nrow = 2)) +
  annotation_scale(style = "bar", location = "br") + 
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering()) 

ggsave(plot = gmp, filename = '../png/map_daniel_alcides_V3.png', units = 'in', width = 12, height = 11, dpi = 300)
