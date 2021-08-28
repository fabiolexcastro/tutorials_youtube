

# Load libraries
require(pacman)
pacman::p_load(raster, gridExtra, grid, lattice, rgdal, ggpubr, ggspatial, rgeos, stringr, ggrepel, hrbrthemes, ghibli, colorspace, gtools, sf, fs, glue, tidyverse)
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data
coord <- data.frame(lugar = c('Cali', 'Cartago'), lon = c(-76.5282, -75.9073), lat = c(3.4461, 4.7693))
coord <- coord[1,]

files <- fs::dir_ls('D:/data/WORLDCLIM/Version20', regexp = '.tif$')

# Read as a stack
tavrg <- grep('tavg', files, value = TRUE)
tavrg <- mixedsort(tavrg)
tavrg <- raster::stack(tavrg)
names(tavrg) <- glue('tmean_{1:12}') 

precp <- grep('30s_prec', files, value = TRUE)
precp <- mixedsort(precp)
precp <- raster::stack(precp)
names(precp) <- glue('prec_{1:12}') 

# Extract the values from the coordinates 
vl_ta <- raster::extract(tavrg, coord[,2:3])
vl_ta <- as.data.frame(vl_ta)
vl_ta <- gather(vl_ta, var, value)
vl_pp <- raster::extract(precp, coord[,2:3])
vl_pp <- as.data.frame(vl_pp)
vl_pp <- gather(vl_pp, var, value)

# Join the two tables
tb <- rbind(vl_ta, vl_pp)
tb <- separate(data = tb, col = var, into = c('variable', 'month'), sep = '_', remove = TRUE)
tb <- inner_join(tb, data.frame(month = as.character(1:12), month_abb = month.abb), by = 'month')
tb <- mutate(tb, month_abb = factor(month_abb, levels = month.abb))
tb <- as_tibble(tb)
tb <- spread(tb, variable, value)
tb$month_abb
tb <- tb %>% arrange(month_abb)

# To make the graph
clmt <- ggplot() + 
  geom_bar(data = tb, aes(x = month_abb, y = prec), fill = '#ECD89DFF',position = 'dodge', stat = 'identity') + 
  theme_ipsum_es() +
  ylab('Precipitación (mm)')

rlc <- mean(tb$prec) / mean(tb$tmean) * 2
rlc <- 10

clm2 <- clmt + 
  geom_line(data = tb, aes(x = month_abb, y = tmean * rlc, group = 1), col = '#94C5CCFF', size = 1.2) + 
  scale_y_continuous(sec.axis = sec_axis(~./rlc, name = 'Temperatura ºC')) +
  xlab('') + 
  scale_x_discrete(labels = c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')) +
  labs(caption = 'Fuente: Worldclim 2.0') + 
  theme(axis.text.x = element_text(size = 11, face = 'bold'),
        axis.text.y = element_text(size = 11, face = 'bold'),
        axis.title.y = element_text(size = 12, face = 'bold'))

# To make the map
cmns <- st_read('D:/data/CALI/shp base/Comunas.shp')
crgt <- st_read('D:/data/CALI/shp base/Corregimientos.shp')
dpts <- st_read('D:/data/IGAC/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')
mpio <- st_read('D:/data/IGAC/MGN2018_MPIO_POLITICO/MGN_MPIO_POLITICO.shp')
limt <- filter(mpio, DPTO_CNMBR %in% c('VALLE DEL CAUCA', 'CAUCA'))
cntr <- st_read('D:/academico/maestria/tesis/data/shp/bse/all_countries.shp')
cali <- filter(mpio, MPIO_CNMBR == 'CALI')
vlle <- filter(dpts, DPTO_CNMBR == 'VALLE DEL CAUCA')

cmns <- st_transform(x = cmns, crs = st_crs(mpio))
crgt <- st_transform(x = crgt, crs = st_crs(crgt))

gcali <- ggplot() + 
  geom_sf(data = limt, fill = '#B3B8B1FF', col = 'white') + 
  geom_sf(data = cmns, fill = '#8E938DFF', col = 'white') +
  geom_sf(data = crgt, fill = '#94A39CFF', col = 'white') + 
  coord_sf(xlim = extent(cali)[1:2], ylim = extent(cali)[3:4]) + 
  theme_bw() + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5, size = 9), 
        axis.text.x = element_text(size = 9)) + 
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book") +
  labs(x = '', y = '')

gmcro <- ggplot() + 
  geom_sf(data = dpts, fill = '#B3B8B1FF') + 
  geom_sf(data = cali, fill = '#94A39CFF', col = '#94A39CFF') + # '#833437FF', col = '#833437FF'
  geom_sf_text(data = dpts, aes(label = str_to_title(DPTO_CNMBR))) +
  coord_sf(xlim = extent(vlle)[1:2], ylim = extent(vlle)[3:4]) +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5, size = 9),
        axis.text.x = element_text(size = 9), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book") + 
  labs(x = '', y = '')

# Figures 
fig1 <- annotate_figure(clm2, top = text_grob('Climatograma para la ciudad de Santiago de Cali', color = 'black', face = 'bold', size = 13))
fig2 <- annotate_figure(gcali, top = text_grob('Santiago de Cali', color = 'black', face = 'bold', size = 13))
fig3 <- annotate_figure(gmcro, top = text_grob('Santiago de Cali en el Valle', color = 'black', face = 'bold', size = 13))

layout <- matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE)# layout <- matrix(c(1, 1, 2, 1, 1, 3, 1, 1, 4 ), nrow = 3, ncol = 3, byrow = TRUE)

all <- grid.arrange(fig1, fig2, fig3, layout_matrix = layout)
ggsave(plot = all, filename = '../png/climatograma_con_mapa.png', units = 'in', width = 11, height = 11, dpi = 300)


