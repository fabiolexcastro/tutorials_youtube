

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, paletteer, ggspatial, hrbrthemes, rmapshaper, tidyverse, climateR, glue, RColorBrewer)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
fles <- dir_ls('./tif/chirps/down', regexp = '.tif$') %>% as.character()
llns <- st_read('./gpkg/llanos.gpkg')
limt <- st_read('./gpkg/limt.gpkg')

# Function to use ---------------------------------------------------------
calc.ndd <- function(yr, mn){
  
  # yr <- 1981
  # mn <- 1
  
  cat('To process: ', yr, ' ', mn, '\n')
  fl <- grep(yr, fles, value = T)
  rs <- terra::rast(fl)
  mn <- ifelse(mn < 10, paste0('0', mn), as.character(mn))
  rs <- rs[[grep(glue('{yr}-{mn}'), terra::time(rs), value = F)]]
  nd <- terra::app(x = rs, fun = function(x){ndd <- sum(x < 1, na.rm = T); return(ndd)})
  return(nd)
  
}

# To apply the function  --------------------------------------------------
ndd.1981 <- map2(.x = rep(1981, 12), .y = 1:12, .f = calc.ndd)
ndd.1981 <- reduce(ndd.1981, c)
names(ndd.1981) <- glue('ndd_1981-{1:12}')

dir <- glue('./tif/chirps/index-ndd')
dir_create(dir)
terra::writeRaster(x = ndd.1981, filename = glue('{dir}/ndd-1981.tif'), overwrite = TRUE)

ndd.1981 <- terra::crop(ndd.1981, vect(limt)) 
ndd.1981 <- terra::mask(ndd.1981, vect(limt))

# To make the map  --------------------------------------------------------
tbl <- terra::as.data.frame(ndd.1981, xy = T, na.rm = T) %>% 
  as_tibble() %>% 
  gather(var, value, -x, -y) %>% 
  mutate(date = str_sub(var, start = 5, end = nchar(var)), 
         month = str_sub(date, start = 6, end = nchar(date))) %>% 
  inner_join(., tibble(month = as.character(1:12), month_abb = month.abb), by = 'month') %>% 
  mutate(month_abb = factor(month_abb, levels = month.abb))

# Mapping
gmap <- ggplot() + 
  geom_tile(data = tbl, aes(x = x, y = y, fill = value)) +
  paletteer::scale_fill_paletteer_c("grDevices::Broc", na.value = 'transparent') +
  facet_wrap(.~month_abb) + 
  geom_sf(data = limt, fill = NA, col = 'grey50') + 
  labs(x = 'Lon', y = 'Lat', fill = 'Number of dry days') +
  theme_ipsum_es() + 
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(size = 4), 
        axis.text.y = element_text(size = 4), 
        strip.text = element_text(face = 'bold'),
        axis.title = element_text(face = 'bold'), 
        text = element_text(family = 'Barlow')) +
  guides(fill = guide_legend( 
    direction = 'horizontal',
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )) 

dir.create('./png')
ggsave(plot = gmap, filename = './png/map_ndd-1981_yearly.png', units = 'in', width = 12, height = 7, dpi = 300)
