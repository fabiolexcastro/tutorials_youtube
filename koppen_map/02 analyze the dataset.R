

# Source:
# https://www.nature.com/articles/s41597-023-02549-6?fbclid=IwY2xjawHoF0ZleHRuA2FlbQIxMAABHaYNEt_zSeOG5bY4qhilO0dZitgR2l5MGlnSo1xm4pGNtYW8_rsOnYbrwg_aem_DBZq8Hy-myTfaLqFsMG1Yw

## High-resolution (1km) Koppen-Geiger maps for 1901-2099 baseon on constrained CMIP6 projections

# Load libraries
require(pacman)
p_load(terra, fs, sf, tidyverse, geodata, rnaturalearthdata, rnaturalearth, gtools, glue)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data 
wrld <- ne_countries(returnclass = 'sf', scale = 50)
col0 <- filter(wrld, sov_a3 == 'COL')
#col0 <- gadm(country = 'COL', level = 0, path = './tmpr')
clrs <- read_csv('./tbl/colors.csv', show_col_types = FALSE)
dirs <- dir_ls('../koppen_geiger_tif', type = 'directory')
dirs <- as.character(dirs)

## Baseline
bsln <- grep('1991_2020', dirs, value = T) |> 
  dir_ls() |> 
  grep('008333', x = _, value = T) |> 
  as.character() |> 
  rast()

## Future 
ftre <- grep('2041_2070', dirs, value = T) |> 
  dir_ls() |> 
  as.character() |> 
  grep(paste0(c('ssp245', 'ssp585'), collapse = '|'), x = _, value = T) |> 
  dir_ls() |> 
  grep('0083333', x = _, value = T) |> 
  as.character() |> 
  rast() |> 
  setNames(c('ssp245', 'ssp585'))


# Make just one stack 
stck <- c(bsln, ftre)
names(stck) <- c('Baseline', 'ssp245', 'ssp585')

# To extract by mask 
stck <- terra::crop(stck, col0)
stck <- terra::mask(stck, col0)

# Raster to table 
tble <- terra::as.data.frame(stck, xy = TRUE) |> 
  as_tibble() |> 
  gather(period, value, -c(x, y)) |> 
  inner_join(x = _, y = clrs, by = c('value' = 'Numeric'))

# Tidy the colors
colr <- pull(clrs, HTML_Color)
names(colr) <- pull(clrs, Class)

# To draw the map 
gmap <- ggplot() + 
  geom_tile(data = tble, aes(x = x, y = y, fill = Class)) +
  facet_wrap(~period) +
  scale_fill_manual(values = colr) +
  geom_sf(data = col0, fill = NA, col = 'grey30') +
  geom_sf(data = wrld, fill = NA, col = 'grey30') +
  ggtitle(label = 'Köppen-Geiger - Clasificación climática') +
  labs(x = 'Lon', y = 'Lat', fill = '', caption = 'Beck, H.E., T.R. McVicar, N. Vergopolan, A. Berg, N.J. Lutsko, A. Dufour, Z. Zeng, X. Jiang, A.I.J.M. van Dijk, D.G. MirallesHigh-resolution (1 km) Köppen-Geiger maps for 1901–2099 based on constrained CMIP6 projectionsScientific Data 10, 724, doi:10.1038/s41597-023–02549‑6 (2023)') +
  scale_y_continuous(breaks = seq(-5, 15, 2), labels = function(breaks) ifelse(breaks < 0, paste0(abs(breaks), "°S"), paste0(breaks, "°N"))) +
  coord_sf(xlim = ext(stck)[1:2], ylim = ext(stck)[3:4]) +
  theme_minimal() + 
  theme(
    legend.position = 'bottom', 
    strip.text = element_text(face = 'bold', hjust = 0.5, size = 14),
    plot.title = element_text(face = 'bold', hjust = 0.5, size = 16), 
    axis.text.y = element_text(face = 'bold', hjust = 0.5, angle = 90)
  ) +
  guides(fill=guide_legend(nrow = 1, byrow = TRUE))
gmap

ggsave(plot = gmap, filename = './png/mapa_koppen.jpg', units = 'in', width = 9, height = 6, dpi = 300, create.dir = TRUE)
