
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, showtext, rgdal, rgeos, stringr, sf, rJava, readxl, tidyverse, fs, gtools, rgeoda)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Add font ----------------------------------------------------------------
font_add_google(family = 'Fira', name = 'Fira code')
font_add_google(family = 'Roboto', name = 'Roboto condensed' )
showtext_auto()

# Load data ---------------------------------------------------------------
shpf <- st_read('../shp/homicidios_barrios_2019.gpkg')
cmns <- st_read('../shp/comunas.gpkg')
cmns <- st_transform(cmns, crs = st_crs(4326))
mpio <- st_read('../shp/mpios_valle.gpkg')

# Moran analysis ----------------------------------------------------------
shpf <- mutate(shpf, gid = 1:nrow(shpf))
qnwg <- queen_weights(shpf, order = 1)
morn <- local_moran(qnwg, st_drop_geometry(shpf['y2019']))

mran_lbls <- lisa_labels(morn)
mran_clrs <- setNames(lisa_colors(morn), mran_lbls)

shpf_clst <- shpf %>%
  st_drop_geometry() %>%
  select(gid) %>%
  mutate(cluster_num = lisa_clusters(morn) + 1, # add 1 bc clusters are zero-indexed
         cluster = factor(mran_lbls[cluster_num], levels = mran_lbls)) %>%
  right_join(shpf, by = "gid") %>%
  st_as_sf() %>% 
  st_transform(x = ., crs = st_crs(4326))

# A simple map
ggplot(shpf_clst, aes(fill = cluster)) +
  geom_sf(color = "white", size = 0) +
  scale_fill_manual(values = mran_clrs, na.value = "green") +
  theme_dark()

mpios <- st_read('D:/data/IGAC/MGN2018_MPIO_POLITICO/MGN_MPIO_POLITICO.shp')
mpios <- filter(mpios, DPTO_CNMBR == 'VALLE DEL CAUCA')

# A nice map
ggm <- ggplot() +
  geom_sf(data = shpf_clst, aes(fill = cluster), color = 'white', size = 0) + 
  scale_fill_manual(values = mran_clrs, na.value = 'green') +
  geom_sf(data = cmns, col = 'grey10', fill = NA) +
  geom_sf_text(data = cmns, aes(label = COMUNA), size = 10, col = '#76766F', family = 'Roboto') +
  geom_sf(data = mpio, fill = NA, col = 'grey10') +
  theme_bw() + 
  coord_sf(xlim = extent(cmns)[1:2], ylim = extent(cmns)[3:4]) +
  ggtitle(label = 'Análisis LISA - Cantidad de homicidios en Cali', 
          subtitle = 'Año 2019') +
  labs(x = 'Longitude', y = 'Latitude', fill = 'Cluster', caption = 'Centro de Investigación y Documentación Socioeconómica (CIDSE)') +
  theme(axis.text.x = element_text(size = 19, family = 'Roboto'), 
        axis.text.y = element_text(size = 19, family = 'Roboto', angle = 90, hjust = 0.5),
        axis.title.x = element_text(size = 26, family = 'Roboto'), 
        axis.title.y = element_text(size = 26, family = 'Roboto'),
        legend.text = element_text(size = 19, family = 'Roboto'),
        plot.title = element_text(size = 30, face = 'bold', hjust = 0.5, family = 'Roboto'),
        plot.subtitle = element_text(size = 30, face = 'bold', hjust = 0.5, family = 'Roboto'),
        legend.title = element_text(size = 26, family = 'Roboto', face = 'bold'),
        legend.position = c(0.815, 0.2),
        panel.background = element_rect(fill = "white"),
        plot.caption = element_text(size = 10, face = 'bold', family = 'Roboto'),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.1))  

ggsave(plot = ggm, 
       filename = '../png/map_moran.png', 
       units = 'in', 
       width = 5, height = 7, dpi = 300)  








