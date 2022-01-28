
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, geodata, tidyverse, 
               gtools, fs, glue, geodata, extrafont, ggthemes, ggspatial)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
pnts <- read_csv('../tbl/cocoa_hndr.csv')
crnt <- raster('../raster/RF_5Prob_current.asc')
ft30 <- raster('../raster/RF5prob_2030.asc')
ft50 <- raster('../raster/RF5prob_2050.asc')
prds <- c('Linea base', '2030s', '2050s')

# Tidy the names
pnts$NAME_1 <- iconv(pnts$NAME_1, to = 'latin1')

# Honduras limits ----------------------------------------------------------
hndr <- gadm(country = 'HND', level = 1, path = './shp')
hndr <- as(hndr, 'Spatial')
hndr <- hndr[,c('NAME_0', 'NAME_1')]

# Extract the values for the presences ------------------------------------
get_values <- function(rst){
   vls <- raster::extract(rst, pnts[,1:2])
   vls <- cbind(pnts, vls)
   vls <- as_tibble(vls) 
   return(vls)
}

vles <- map(.x = list(crnt, ft30, ft50), .f = get_values)
vles <- map(.x = 1:3, .f = function(k){
  vles[[k]] %>% 
    setNames(c('x', 'y', 'name_0', 'name_1', prds[k]))
})

vles <- vles %>% 
   purrr::reduce(inner_join) %>% 
   mutate(gid = 1:nrow(.)) %>% 
   setNames(c('x', 'y', 'name_0', 'name_1', 'baseline', 'y2030', 'y2050', 'gid')) %>%  
   gather(variable, value, -x, -y, -name_0, -name_1, -gid)

# Get the srtm raster -----------------------------------------------------
srtm <- elevation_30s(country = 'HND', path = tempdir())
srtm.vles <- terra::extract(srtm, as.data.frame(pnts[,1:2]))
srtm.vles <- as_tibble(srtm.vles)
colnames(srtm.vles) <- c('id', 'srtm')
vles <- full_join(srtm.vles, vles, by = c('id' = 'gid'))
vles <- drop_na(vles)
vles <- dplyr::select(vles, id, x, y, name_0, name_1, variable, value, srtm)

ggpl <- ggplot(data = vles, aes(x = value, y = srtm, col = variable)) + 
   geom_smooth(se = FALSE, method = 'loess') + 
   scale_color_manual(values = c('#719E70', '#70839E', '#D9737E'), 
                      labels = c('Línea base', '2030', '2050')) +
   ggtitle(label = 'Idoneidad versus altitud a distintos periodos de tiempo\n                        ') +
   theme_pander() + 
   theme(legend.position = 'bottom',
         plot.title = element_text(size = 14, face = 'bold', hjust = 0.5), 
         axis.text.y = element_text(angle = 90, hjust = 0.5), 
         legend.title = element_text(size = 12, face = 'bold')) +
   labs(x = 'Puntaje de idoneidad', y = 'Altitud (m.s.n.m)', col = 'Periodo') 

ggsave(plot = ggpl, 
       filename = './png/srtm_vs_suit.png', 
       units = 'in', width = 8, height = 7, dpi = 300)


