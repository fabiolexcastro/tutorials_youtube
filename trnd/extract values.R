
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, glue, exactextractr)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

# Vector data
bsin.zone <- vect('./gpkg/bsin.gpkg')

# Raster data
path <- dir_ls('./tif/climate/cmip6_daily')

# Baseline data -----------------------------------------------------------
fles.bsln <- dir_ls(dir_ls(dir_ls(grep('historical', path, value = T))))
fles.bsln <- grep('.nc$', fles.bsln, value = T)
fles.bsln <- as.character(fles.bsln)

# Future data -------------------------------------------------------------
fles.ftre <- dir_ls(dir_ls(dir_ls(grep('future', path, value = T))), regexp = '.nc$')
fles.ftre <- grep('.nc$', fles.ftre, value = T)
fles.ftre <- as.character(fles.ftre)

# Extract values ----------------------------------------------------------

# Function to use 
extr.vles <- function(fles){
  
  # fles <- fles.bsln
  
  cat('... Starting ...\n')
  rslt <- map(.x = fles, .f = function(f){
    
    cat('To process: ', basename(f), '\n')
    nme <- basename(f)
    yea <- str_sub(nme, start = nchar(nme) - 6, end = nchar(nme) -3)
    rst <- rast(f)
    rst <- terra::crop(rst, bsin.zone)
    rst <- terra::mask(rst, bsin.zone)
    vle <- terra::as.data.frame(rst, xy = F)
    vle <- as_tibble(t(vle))
    vle <- mean(pull(vle)) - 275.15
    vle <- tibble(year = yea, value = vle)
    return(vle)
    
  })
  
  rslt <- bind_rows(rslt)
  return(rslt)
  
}

# To apply the function
vles.bsln <- extr.vles(fles = fles.bsln) 
vles.ftre <- extr.vles(fles = fles.ftre)

vles.bsln <- mutate(vles.bsln, Periodo = 'Línea base')
vles.ftre <- mutate(vles.ftre, Periodo = 'Futuro')
vles <- rbind(vles.bsln, vles.ftre)
vles <- mutate(vles, year = as.numeric(year))
vles <- mutate(vles, Periodo = factor(Periodo, levels = c('Línea base', 'Futuro')))

plot(vles.bsln$value, type = 'l')
plot(vles$value, type = 'l')

# To make the graph -------------------------------------------------------


gline <- ggplot(data = vles, aes(x = year, y = value, col = Periodo)) + 
  geom_line() + 
  scale_x_continuous(labels = seq(1980, 2100, 10), breaks = seq(1980, 2100, 10)) +
  scale_y_continuous(labels = 23:30, breaks = 23:30) +
  scale_color_manual(values = c('#0B3B0B', '#DAA801')) +
  ggtitle(label = 'Comportamiento de la temperatura máxima histórica y futura', 
          subtitle = 'SSP 370 - GCM: ACCESS-CM2') +
  labs(x = 'Año', y = 'Temperatura (°C)', col = '') +
  theme_light() + 
  theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), 
        plot.subtitle = element_text(face = 'bold', size = 14, hjust = 0.5),
        legend.key.width = unit(3, 'line'), 
        legend.key.height = unit(1.2, 'line'),
        text = element_text(family = 'Segoe UI'), 
        legend.position = 'bottom') 

ggsave(plot = gline, filename = './png/graph_trend.png', units = 'in', width = 9, height = 7, dpi = 300)
