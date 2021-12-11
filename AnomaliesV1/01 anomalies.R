
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, tidyverse, hrbrthemes, sf, fs, ggpubr, gridExtra)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------

gtml <- getData(name = 'GADM', country = 'GTM', level = 2)
gtml <- st_as_sf(gtml)
asnc <- filter(gtml, NAME_2 == 'Asunción Mita')
gtm1 <- getData(name = 'GADM', country = 'GTM', level = 1)
gtm1 <- st_as_sf(gtm1)
wrld <- st_read('D:/academico/maestria/tesis/data/shp/bse/all_countries.shp')
wrld <- filter(wrld, CONTINENT == 'North America')

tble <- read_csv('../tbl/asuncion_mita.csv')
tble <- mutate(tble, estacion = iconv(estacion, to = 'latin1'))

# To calculate the anomalies ----------------------------------------------
ppt.avg <- mean(tble$ppt)
tmx.avg <- mean(tble$tmax)
tmn.avg <- mean(tble$tmin)

tble <- mutate(tble,
               ppt.anm = ppt - ppt.avg, 
               tmx.anm = tmax - tmx.avg,
               tmn.anm = tmin - tmn.avg)

# To make the graphic -----------------------------------------------------
gtmx <- ggplot(data = tble, aes(x = Anio)) +
  geom_line(aes(y = tmax, group = 1), col = 'blue', size = 1.2) + 
  geom_hline(yintercept = tmx.avg, lineytype = 'dashed', col = 'darkblue', size = 1.2) +
  ggtitle(label = 'Anomalías para la temperatura máxima en\nAsunción Mita') +
  theme_ipsum_es() + 
  theme(axis.text = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold', size = 12), 
        axis.title.y = element_text(face = 'bold', size = 12)) + 
  labs(x = 'Año', y = 'Temperatura máxima')

gtmn <- ggplot(data = tble, aes(x = Anio)) +
  geom_line(aes(y = tmin, group = 1), col = 'blue', size = 1.2) + 
  geom_hline(yintercept = tmn.avg, lineytype = 'dashed', col = 'darkblue', size = 1.2) +
  ggtitle(label = 'Anomalías para la temperatura mínima en\nAsunción Mita') +
  theme_ipsum_es() + 
  theme(axis.text = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold', size = 12), 
        axis.title.y = element_text(face = 'bold', size = 12)) + 
  labs(x = 'Año', y = 'Temperatura mínima')

gppt <- ggplot(data = tble, aes(x = Anio)) +
  geom_col(aes(y = ppt, group = 1), fill = 'grey', size = 1.2) + 
  geom_hline(yintercept = ppt.avg, lineytype = 'dashed', col = 'darkblue', size = 1.2) +
  ggtitle(label = 'Anomalías para la precipitación en\nAsunción Mita') +
  theme_ipsum_es() + 
  theme(axis.text = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold', size = 12), 
        axis.title.y = element_text(face = 'bold', size = 12)) + 
  labs(x = 'Año', y = 'Precipitación')

gmap <- ggplot() + 
  geom_sf(data = wrld, fill = '#E4E4E4', col = '#F7F7F7') +
  geom_sf(data = gtm1, fill = '#939393', col = '#F7F7F7') + 
  geom_sf(data = asnc, fill = '#901515', col = '#F7F7F7') +
  theme_ipsum_es() +
  ggtitle(label = 'Ubicación geográfica de\nAsunción Mita') +
  # geom_sf_text(data = asnc, aes(label = NAME_2)) +
  coord_sf(xlim = extent(gtm1)[1:2], ylim = extent(gtm1)[3:4])


gall <- ggarrange(gtmx, gtmn, gppt, gmap, ncol = 2, nrow = 2)

ggsave(plot = gall,
       filename = '../png/grap_all.jpg', units = 'in', width = 12, height = 11, dpi = 300)




