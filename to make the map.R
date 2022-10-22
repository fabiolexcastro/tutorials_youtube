
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(sf, fs, glue, tidyverse, ggspatial, terra, osmdata, hrbrthemes, showtext, osrm, rmapshaper, colourpicker)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
fles <- dir_ls('./gpkg') %>% as.character()
amnt <- grep('amenities', fles, value = T) %>% st_read()
amnt
road <- grep('road', fles, value = T) %>% st_read()
rver <- grep('river', fles, value = T) %>% st_read()
cmns <- grep('comunas', fles, value = T) %>% st_read()
cali <- ms_dissolve(cmns)

# Filtering road
road %>% pull(highway) %>% table()
road <- filter(road, highway %in% c('primary', 'secondary', 'tertiary'))

# Dissolving
rver
rios <- unique(rver$name)
i <- 1
rver <- purrr::map(.x = 1:length(rios), .f = function(i){
  rvr <- rver %>% filter(name == rios[i]) %>% dplyr::select(name, geom)
  rvr <- st_combine(rvr)
  rvr <- st_as_sf(rvr)
  rvr <- mutate(rvr, name = rios[i])
})
rver <- do.call(what = 'rbind', args = rver)

# To make the map ---------------------------------------------------------

colourpicker::colourWidget()

import_econ_sans()
font_add_google("Roboto Condensed", "Roboto")

showtext_auto()

# Amenities
amnt
unique(amnt$amenity)
table(amnt$amenity)
bars <- filter(amnt, amenity == 'bar')

gmap <- ggplot() + 
  geom_sf(data = cali, fill = NA, col = 'grey20', lwd = 0.5) + 
  geom_sf(data = rver, col = '#2D5E8C', lwd = 0.9) +
  geom_sf(data = road, col = '#CFCF3E', lwd = 0.2) +
  geom_sf(data = cmns, fill = NA, col = 'grey40', lwd = 0.5) +
  geom_sf(data = bars, col = '#C24E4E') +
  labs(x = 'Lon', y = 'Lat') + 
  coord_sf(xlim = ext(cali)[1:2], ylim = ext(cali)[3:4]) +
  geom_sf_text(data = cmns, aes(label = comuna), size = 9.8, family = 'Roboto') +
  geom_sf_text(data = rver, aes(label = name), size = 6, family = 'Roboto') +
  # ggtitle(label = '   Principales vías de Santiago de Cali', subtitle = '    Uso de Open Street Data') +
  theme_void() + 
  theme(panel.background = element_rect(fill = '#919191', colour = '#919191'),
        plot.background = element_rect(colour = 'white'), #fill = '#919191', 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        plot.caption = element_text(family = 'Roboto', size = 16, face = 'bold'),
        plot.title = element_text(face = 'bold', family = 'Roboto'), 
        plot.subtitle = element_text(face = 'bold', family = 'Roboto')) + # axis.text.x = element_text(size = 5), axis.text.y = element_text(size = 5, angle = 90, hjust = 0.5))
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    width = unit(1.4, 'cm'),
    height = unit(1.3, 'cm'),
    pad_x = unit(0.14, "in"), pad_y = unit(0.14, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey50", "white"),
      line_col = "grey15", text_family = "Roboto" , text_col = 'grey50', text_size = 20))+
  ggspatial::annotation_scale(location = "bl", bar_cols = c("grey60", "white"), text_cex = 2.2, text_family = "Roboto", text_col="white") +
  annotate(geom = 'text', x = -76.550, y = 3.51, size = 14, label = 'Principales vías y bares en Santiago de Cali', family = 'Roboto') + 
  annotate(geom = 'text', x = -76.480, y = 3.32599, size = 6.5, label = 'Fuente: Open Street Maps - IDESC (Cali)', family = 'Roboto')
  
gmap
ggsave(plot = gmap, filename = './png/mapa_cali_v1.png', units = 'in', width = 5, height = 7.05, dpi = 300)
 