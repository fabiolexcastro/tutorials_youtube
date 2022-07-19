

# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, sf, tidyverse, gtools, RColorBrewer, rgeos, stringr, rnaturalearth, glue, fs, magick)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
fles <- 'D:/data/TERRACLIMATE' %>% dir_ls()
wrld <- ne_countries(scale = 50, returnclass = 'sf')
zone <- filter(wrld, sov_a3 %in% c('GTM', 'NIC', 'HND', 'SLV'))
year <- 1981:2020
zone <- vect(zone)

# To extract by mask  -----------------------------------------------------
rstr <- purrr::map(.x = 1:length(year), .f = function(i){
  
  cat(year[i], '\n')
  fls <- grep(year[i], fles, value = T)
  fls <- as.character(fls)
  ppt <- terra::rast(grep('ppt', fls, value = T))
  tmx <- terra::rast(grep('tmax', fls, value = T))
  tmn <- terra::rast(grep('tmin', fls, value = T))
  tmx <- terra::crop(tmx, zone) %>% terra::mask(., zone)
  tmn <- terra::crop(tmn, zone) %>% terra::mask(., zone)
  ppt <- terra::crop(ppt, zone) %>% terra::mask(., zone)
  return(list(ppt, tmx, tmn))
  
})

# To make the table -------------------------------------------------------
tbls <- purrr::map(.x = 1:length(rstr), .f = function(i){
  
  cat(i, '\n')
  rst <- rstr[[i]]
  yea <- year[i]
  ppt.tbl <- rst[[1]]
  tmx.tbl <- rst[[2]]
  tmn.tbl <- rst[[3]]
  
  ppt.tbl <- as.data.frame(ppt.tbl, xy = TRUE)
  tmx.tbl <- as.data.frame(tmx.tbl, xy = TRUE)
  tmn.tbl <- as.data.frame(tmn.tbl, xy = TRUE)
  
  ppt.tbl <- as_tibble(ppt.tbl)
  tmx.tbl <- as_tibble(tmx.tbl)
  tmn.tbl <- as_tibble(tmn.tbl)
  
  ppt.tbl <- mutate(ppt.tbl, year = yea)
  tmx.tbl <- mutate(tmx.tbl, year = yea)
  tmn.tbl <- mutate(tmn.tbl, year = yea)
  
  tbl <- list(ppt.tbl, tmx.tbl, tmn.tbl) %>% 
    purrr::reduce(., inner_join, by = c('x', 'y')) %>% 
    mutate(year = yea, gid = 1:nrow(.)) %>% 
    gather(var, value, -x, -y, -gid) %>% 
    separate(data = ., col = var, into = c('variable', 'month'), sep = '_') %>% 
    inner_join(., tibble(month = as.character(1:12), month_abb = month.abb)) %>% 
    mutate(month_abb = factor(month_abb, levels = month.abb)) %>% 
    dplyr::select(x, y, gid, variable, month_abb, value) %>% 
    spread(variable, value) %>% 
    mutate(year = yea)
  
  return(tbl)
  
})

dir_create('../qs')
tbls <- bind_rows(tbls)
qs::qsave(tbls, file = '../qs/tbls.qs')
tbls <- qs::qread('../qs/tbls.qs')

# To make the maps --------------------------------------------------------
summary(tbls$ppt)

purrr:::map(.x = 1:40, .f = function(i){
  
  yea <- year[i]
  ggp <- ggplot() + 
    geom_tile(data = tbls %>% filter(year == yea), aes(x = x, y = y, fill = ppt)) +
    facet_wrap(.~month_abb) + 
    scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'BrBG'),
                         limits = c(0, 1800)) +
    geom_sf(data = st_as_sf(zone), fill = NA, col = 'grey60', width = 0.5) + 
    geom_sf_text(data = st_as_sf(zone), aes(label = admin), size = 1.2) +
    coord_sf() + 
    theme_void() + 
    ggtitle(label = year[i]) +
    labs(fill = 'Prec (mm)') +
    theme(legend.position = 'bottom', 
          plot.title = element_text(face = 'bold', size = 16, hjust = 0.5),
          legend.title = element_text(face = 'bold', size = 14),
          legend.key.width = unit(3, 'line'),
          legend.key.height = unit(0.5, 'line'),
          strip.text.y = element_text(face = 'bold', size = 13, hjust = 0.5))
  
  ggsave(plot = ggp, 
         filename = glue('../png/maps/prec_{yea}.png'), units = 'in', width = 8, height = 8, dpi = 300)
  
  
})



# To make the GIF 
img <- dir_ls('../png/maps')
img <- map(img, image_read)
jnd <- image_join(img)
anm <- magick::image_animate(jnd, fps = 1)

dir_create('../gif')

# To write
image_write(image = anm, path = glue('../gif/prec.gif'))





