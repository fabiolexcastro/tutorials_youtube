
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(showtext, tidyverse, sf, rJava, ggpubr, rmapshaper, ggspatial, colourpicker, terra, rgeoda, fs, MetBrewer, geodata)

g <- gc(reset = TRUE)
rm(list = ls())
option(scipen = 999, warn = -1)

# More about Spatial Data (Cali): http://ws-idesc.cali.gov.co:8081/geoserver/web/;jsessionid=C7A0573E922A863F7F310930468D28F0?wicket:bookmarkablePage=:org.geoserver.web.demo.MapPreviewPage

# Load data ---------------------------------------------------------------
shpf <- './shpf/homicidios_barrios.gpkg'
shpf <- st_read(shpf)

plot(st_geometry(shpf))                

crgn <- st_read('http://ws-idesc.cali.gov.co:8081/geoserver/idesc/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=idesc:mc_corregimientos&maxFeatures=50&outputFormat=json')
rios <- st_read('http://ws-idesc.cali.gov.co:8081/geoserver/pot_2014/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=pot_2014:bcs_hid_rios&maxFeatures=50&outputFormat=json')
cmns <- st_read('http://ws-idesc.cali.gov.co:8081/geoserver/idesc/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=idesc:mc_comunas&maxFeatures=50&outputFormat=json')
mpos <- st_read('D:/data/spatial/igac/mpios.gpkg') %>% filter(DPTO_CNMBR == 'VALLE DEL CAUCA') %>% filter(MPIO_CNMBR %in% c('PALMIRA', 'CANDELARIA', 'DAGUA', 'JAMUNDÍ', 'YUMBO')) %>% mutate(MPIO_CNMBR = str_to_title(MPIO_CNMBR))
expn <- st_read('http://ws-idesc.cali.gov.co:8081/geoserver/idesc/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=idesc:mc_division_suelo_expansion&maxFeatures=50&outputFormat=json')


# Projecting to WGS 1984
crgn <- st_transform(crgn, crs = st_crs(4326))
rios <- st_transform(rios, crs = st_crs(4326))
cmns <- st_transform(cmns, crs = st_crs(4326))
expn <- st_transform(expn, crs = st_crs(4326))

# To dissolve
expn <- expn %>% mutate(gid = 'Zona expansión') %>% ms_dissolve(., field = 'gid')

# To make the map ---------------------------------------------------------

# To check the colors
MetBrewer::display_all()

# Homicidios map
ghmc <- ggplot() + 
  geom_sf(data = shpf, aes(fill = total), col = 'grey59', lwd = 0.3) + 
  scale_fill_gradientn(colors = rev(met.brewer('Peru2', 9))) +
  geom_sf(data = cmns, fill = NA, col = 'white', lwd = 0.75) +
  geom_sf(data = expn, fill = '#C7C7C7', col = 'white', lwd = 0.3) +
  geom_sf(data = crgn, fill = 'grey70', col = '#E0E0E0', lwd = 0.3) + 
  geom_sf(data = mpos, fill = '#FFFFFF', col = 'grey90', lwd = 0.3) +
  geom_sf(data = rios, fill = '#3F73A3', col = '#3F73A3') + 
  geom_sf_text(data = crgn, aes(label = corregimie), family = 'serif', size = 2.5, col = 'grey40') +
  geom_sf_text(data = cmns, aes(label = comuna), family = 'serif', size = 4, face = 'bold', col = 'white') +
  geom_sf_text(data = expn, aes(label = gid), family = 'serif', size = 2.5, col = 'grey40') +
  geom_sf_text(data = mpos, aes(label = MPIO_CNMBR), family = 'serif', size = 2.5, col = 'grey40') +
  labs(x = 'Lon', y = 'Lat', fill = 'Homicidios', caption = 'Fuente: Policía Metropolitana de Cali') + 
  ggtitle(label = 'Cantidad de homicidios por barrio\ndesde 2015 hasta el 2019') + 
  coord_sf(xlim = ext(shpf)[1:2], ylim = ext(shpf)[3:4]) +
  theme_minimal() + 
  theme(legend.position = 'right', 
        legend.text = element_text(family = 'serif'), 
        legend.title = element_text(family = 'serif', face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = 0.5, family = 'serif'),
        plot.caption = element_text(family = 'serif'),
        axis.text.x = element_text(family = 'serif'), 
        axis.text.y = element_text(hjust = 0.5, angle = 90, family = 'serif'),
        axis.title.x = element_text(family = 'serif'),
        axis.title.y = element_text(family = 'serif'),
        legend.key.height = unit(2.5, 'line')) +
  annotation_scale(location =  "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'serif'))

ggsave(plot = ghmc, filename = './png/homicidios.png', units = 'in', width = 7, height = 8, dpi = 300)

names(clrs) <- nmes$clase

# Estrato moda map
gstr <- ggplot() + 
  geom_sf(data = shpf %>% filter(ESTRA_MODA %in% 1:6), aes(fill = ESTRA_MODA), col = 'grey59', lwd = 0.3) + 
  scale_fill_gradientn(colors = met.brewer(name = 'VanGogh3', n = 6), na.value = 'green') +
  geom_sf(data = cmns, fill = NA, col = 'white', lwd = 0.75) +
  geom_sf(data = expn, fill = '#C7C7C7', col = 'white', lwd = 0.3) +
  geom_sf(data = crgn, fill = 'grey70', col = '#E0E0E0', lwd = 0.3) + 
  geom_sf(data = mpos, fill = '#FFFFFF', col = 'grey90', lwd = 0.3) +
  geom_sf(data = rios, fill = '#3F73A3', col = '#3F73A3') + 
  geom_sf_text(data = crgn, aes(label = corregimie), family = 'serif', size = 2.5, col = 'grey40') +
  geom_sf_text(data = cmns, aes(label = comuna), family = 'serif', size = 4, face = 'bold', col = 'white') +
  geom_sf_text(data = expn, aes(label = gid), family = 'serif', size = 2.5, col = 'grey40') +
  geom_sf_text(data = mpos, aes(label = MPIO_CNMBR), family = 'serif', size = 2.5, col = 'grey40') +
  labs(x = 'Lon', y = 'Lat', fill = 'Estrato', caption = 'Fuente: Plan de Ordenamiento Territorial (2014)') + 
  ggtitle(label = 'Estrato moda\na nivel de barrio') + 
  coord_sf(xlim = ext(shpf)[1:2], ylim = ext(shpf)[3:4]) +
  theme_minimal() + 
  theme(legend.position = 'right', 
        legend.text = element_text(family = 'serif'), 
        legend.title = element_text(family = 'serif', face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = 0.5, family = 'serif'),
        plot.caption = element_text(family = 'serif'),
        axis.text.x = element_text(family = 'serif'), 
        axis.text.y = element_text(hjust = 0.5, angle = 90, family = 'serif'),
        axis.title.x = element_text(family = 'serif'),
        axis.title.y = element_text(family = 'serif'),
        legend.key.height = unit(2.5, 'line')) +
  annotation_scale(location =  "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'serif'))

ggsave(plot = gstr, filename = './png/estrato.png', units = 'in', width = 7, height = 8, dpi = 300)

# Join both maps into only one
gall <- ggarrange(gstr, ghmc, nrow = 1, ncol = 2)
ggsave(plot = gall, filename = './png/estrato_homicidios.png', units = 'in', width = 14, height = 8, dpi = 300)

# To make bimoran analysis ------------------------------------------------

shpf
qnwg <- queen_weights(shpf, order = 1)
morn <- local_bimoran(w = qnwg, df = st_drop_geometry(shpf[c('ESTRA_MODA', 'total')]))

lbls <- lisa_labels(morn)
clrs <- setNames(lisa_colors(morn), lbls)

shpf <- mutate(shpf, cluster_num = lisa_clusters(morn) + 1, cluster = factor(lbls[cluster_num], levels = lbls))
nmes <- tibble(cluster = lbls, clase = c('Sin significancia', 'Alto - Alto', 'Bajo - Bajo', 'Bajo - Alto', 'Alto - Bajo', 'Indefinido', 'Isolado'))

shpf <- inner_join(shpf, nmes, by = 'cluster')
shpf <- mutate(shpf, clase = factor(clase, levels = nmes$clase))

# A simple map
ggplot(shpf, aes(fill = cluster)) +
  geom_sf(color = "white", size = 0) +
  scale_fill_manual(values = clrs, na.value = "green") +
  theme_dark()

# A nive map
gbin <- ggplot() + 
  geom_sf(data = shpf %>% filter(ESTRA_MODA %in% 1:6), aes(fill = clase), col = 'grey59', lwd = 0.3) + 
  scale_fill_manual(values = clrs, na.value = 'green') +
  geom_sf(data = cmns, fill = NA, col = '#999999', lwd = 0.75) +
  geom_sf(data = expn, fill = '#C7C7C7', col = 'white', lwd = 0.3) +
  geom_sf(data = crgn, fill = 'grey70', col = '#E0E0E0', lwd = 0.3) + 
  geom_sf(data = mpos, fill = '#FFFFFF', col = 'grey90', lwd = 0.3) +
  geom_sf(data = rios, fill = '#3F73A3', col = '#3F73A3') + 
  geom_sf_text(data = crgn, aes(label = corregimie), family = 'serif', size = 2.5, col = 'grey40') +
  geom_sf_text(data = cmns, aes(label = comuna), family = 'serif', size = 4, face = 'bold', col = 'white') +
  geom_sf_text(data = expn, aes(label = gid), family = 'serif', size = 2.5, col = 'grey40') +
  geom_sf_text(data = mpos, aes(label = MPIO_CNMBR), family = 'serif', size = 2.5, col = 'grey40') +
  labs(x = 'Lon', y = 'Lat', fill = 'Estrato - Homicidios', caption = 'Fuente: Análisis Moran Bivariado, Castro 2022') + 
  ggtitle(label = 'Análisis Moran Bivariado entre el estrato moda\n y la cantidad de homicidios') + 
  coord_sf(xlim = ext(shpf)[1:2], ylim = ext(shpf)[3:4]) +
  theme_minimal() + 
  theme(legend.position = 'right', 
        legend.text = element_text(family = 'serif'), 
        legend.title = element_text(family = 'serif', face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = 0.5, family = 'serif'),
        plot.caption = element_text(family = 'serif'),
        axis.text.x = element_text(family = 'serif'), 
        axis.text.y = element_text(hjust = 0.5, angle = 90, family = 'serif'),
        axis.title.x = element_text(family = 'serif'),
        axis.title.y = element_text(family = 'serif')) +
  annotation_scale(location =  "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'serif'))

ggsave(plot = gbin, filename = './png/moran_bivariado.png', units = 'in', width = 7.3, height = 8, dpi = 300)
