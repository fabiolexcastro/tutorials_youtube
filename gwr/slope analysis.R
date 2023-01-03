
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, elevatr, glue, trend, broom, rmapshaper, ggspatial, RColorBrewer, cptcity, gtools, rgeos, climateR, RSAGA)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
mpio <- st_read('gpkg/mpio.gpkg')
dpto <- st_read('gpkg/dpto.gpkg')
qndo <- filter(dpto, DPTO_CNMBR == 'QUINDIO')
qndo_mpio <- filter(mpio, DPTO_CNMBR == 'QUINDIO')

# Salento  ----------------------------------------------------------------
slnt <- filter(mpio, MPIO_CNMBR == 'SALENTO')
slnt <- vect(slnt)

# Climate  ----------------------------------------------------------------
fles <- dir_ls('tif/terraclimate/zoom8/yearly') %>% as.character() %>% mixedsort()
rstr <- terra::rast(fles)
rstr <- terra::crop(rstr, slnt)
rstr <- terra::mask(rstr, slnt)
tble <- terra::as.data.frame(rstr, xy = T) %>% as_tibble()
colnames(tble)
tble <- mutate(tble, gid = 1:nrow(tble))
tble <- gather(tble, var, value, -gid, -x, -y)
tble <- mutate(tble, year = parse_number(var))
gids <- unique(pull(tble, gid))

# To calculate the slope --------------------------------------------------
rslt <- purrr::map(.x = 1:length(gids), .f = function(i){
  cat(i, '\n')
  tbl <- filter(tble, gid == gids[i])
  vec <- pull(tbl, value)
  slp <- sens.slope(vec)
  slp <- tidy(slp) %>% mutate(gid = gids[i], estimates = slp$estimates)
  return(slp)
})

rslt <- bind_rows(rslt)
crds <- distinct(tble, x, y, gid)
rslt <- inner_join(rslt, crds, by = 'gid')
mtrx <- dplyr::select(rslt, x, y, estimates, p.value)
rstr <- terra::rast(mtrx, type = 'xyz')
hist(rslt$p.value)
plot(rstr[[1]])

addl <- rstr[[1]] * 41
rstr <- c(rstr, addl)
names(rstr) <- c('slope', 'pvalue', 'added')
tble <- as_tibble(terra::as.data.frame(rstr, xy = TRUE))

# To make the map ---------------------------------------------------------

find_cpt('temperature')
image(matrix(1:100), col = cpt("jjg_misc_temperature"))

gmap <- ggplot() + 
  geom_tile(data = tble, aes(x = x, y = y, fill = added)) + 
  scale_fill_gradientn(colors = cpt('jjg_misc_temperature')) +
  geom_sf(data = st_as_sf(slnt), fill = NA, col = 'grey50', lwd = 0.5) + 
  geom_sf(data = qndo_mpio, fill = NA, col = 'grey40', lwd = 0.5) +
  geom_sf(data = dpto, fill = NA, col = 'grey20', lwd = 0.5) +
  coord_sf(xlim = ext(slnt)[1:2], ylim = ext(slnt)[3:4]) +
  ggtitle(label = 'Aumento en la temperatura en Salento\npara los últimos 40 años (1980-2020)') +
  labs(fill = 'Aumento temperatura (°C)') +
  theme_void() + 
  theme(legend.position = 'bottom', 
        legend.text= element_text(color = 'grey50'),
        legend.title = element_text(color = 'grey50'),
        plot.title = element_text(hjust = 0.5, face = 'bold', color = 'grey50'),
        legend.key.width = unit(3, 'line')) +
  annotation_scale(location =  "br", width_hint = 0.5, bar_cols = c('grey30', 'grey90'), text_col = 'grey50') +
  annotation_north_arrow(location = 'tl', which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_col = 'grey50'))

ggsave(plot = gmap, filename = 'png/map_aumento_temp.png', units = 'in', width = 9, height = 7, dpi = 300)



