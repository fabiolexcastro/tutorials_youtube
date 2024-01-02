

# ---------------------------------------------------------------------------

# GHSL - Global Human Settlement Layer
# To download the dataset
# Data source: https://ghsl.jrc.ec.europa.eu/download.php?ds=pop

### Colombia 
## Tiles: 
# R9 - C11
# R8 - C11
# R9 - C12
# R10 - C11
# R10 - C12

# Author: Fabio Castro - Llanos / Milos Makes Maps
# December 29th 2023

# ---------------------------------------------------------------------------

### Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, ggrepel, ggspatial, fs, sf, tidyverse, gtools, stringr, glue, rnaturalearthdata, rnaturalearth)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

### Vector data -------------------------------------------------------------
wrld <- ne_countries(returnclass = 'sf', scale = 50)
limt <- wrld[wrld$sov_a3 == 'COL',]
dpto <- st_read('D:/data/spatial/igac/dptos.gpkg')

### To download the dataset -------------------------------------------------

url_1 <- 'http://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R9_C11.zip'
url_2 <- 'http://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R8_C11.zip'
url_3 <- 'http://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R9_C12.zip'
url_4 <- 'http://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R10_C11.zip'
url_5 <- 'http://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R10_C12.zip'
urls  <- c(url_1, url_2, url_3, url_4, url_5)
dir   <- './data/tif_raw'
dir.create(dir, recursive = TRUE)

# To download and unzip
for(i in 1:length(urls)){ 
  
  cat('Download file: ', i, '\n')
  download.file(
    url = urls[i],
    path = dir,
    destfile = paste0(dir, '/', basename(urls[i]))
  )
  
  unzip(
    paste0(dir, '/', basename(urls[i])), 
    exdir = dir
  )
  
}

### To read the dataset -----------------------------------------------------

fles <- as.character(dir_ls(dir, regexp = '.tif$'))
rstr <- map(fles, rast)
rstr <- sprc(rstr)
rstr <- mosaic(rstr)

# To project the shapefile 
limt <- st_transform(limt, crs(rstr))
limt.geo <- st_transform(limt, '+proj=longlat +datum=WGS84 +no_defs +type=crs')

# To extract by mask 
rstr <- terra::crop(rstr, vect(limt))
rstr <- terra::mask(rstr, vect(limt))
rstr <- terra::project(rstr, '+proj=longlat +datum=WGS84 +no_defs +type=crs')

### Raster to table ---------------------------------------------------------
tble <- terra::as.data.frame(rstr, xy = T)
tble <- as_tibble(tble)
colnames(tble) <- c('lon', 'lat', 'val')

# Continous to binary
tble <- mutate(tble, val = if_else(val > 0, 'Sí', 'No'))
tble <- mutate(tble, val = factor(val, levels = c('Sí', 'No')))

# The colors
cols <- c("#0a1c29", "#edc241")

# Get the coordinates for each department ---------------------------------
crds <- dpto %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  mutate(dpto = dpto$DPTO_CNMBR, 
         dpto = str_to_title(dpto))

crds <- mutate(crds, dpto = gsub('Del', 'del', dpto))
crds <- mutate(crds, dpto = gsub('De', 'de', dpto))
crds <- mutate(crds, dpto = gsub('D.c.', 'D.C.', dpto))

# To make the map ------------------------------------------------------------

gmap <- 
  ggplot() + 
  geom_raster(
    data = tble,
    aes(
      x = lon,
      y = lat,
      fill = val
    )
  ) +
  scale_fill_manual(
    name = 'Personas',
    values = cols,
    na.value = '#0a1c29'
  ) +
  geom_sf(
    data = wrld, 
    fill = NA, 
    col = 'grey70'
  ) +
  geom_sf(
    data = limt, 
    fill = NA, 
    col = 'grey30'
  ) +
  geom_sf(
    data = dpto, 
    fill = NA, 
    col = 'grey20'
  ) +
  geom_text_repel(
    data = crds,
    aes(
      x = X, 
      y = Y, 
      label = dpto
    ),
    col = 'grey70',
    size = 2.2
  ) +
  coord_sf(
    xlim = ext(limt.geo)[1:2], 
    ylim = ext(limt.geo)[3:4],
  ) +
  ggtitle(
    label = 'GHSL - Global Human Settlement Layer (Colombia)'
  ) + 
  guides(
    fill = guide_legend(
      direction = 'horizontal', 
      keyheight = unit(5, 'mm'),
      keywidth = unit(15, 'mm'),
      title.position = 'top',
      title.hjust = 0.5,
      label.position = 'bottom',
      label.hjust = 0.5,
      nrow = 1, 
      byrow = T
    )
  ) + 
  theme_void() + 
  theme(
    legend.position = 'bottom',
    legend.title = element_text(
      size = 16, color = "grey10"
    ),
    legend.text = element_text(
      size = 14, color = "grey10"
    ),
    plot.caption = element_text(
      size = 10, color = "grey10",
      hjust = 0.5,# vjust = 20
    ),
    plot.title = element_text(
      hjust = 0.5, face = 'bold', size = 20
    ),
    plot.subtitle = element_text(
      hjust = 0.5, face = 'bold', size = 16
    ),
    plot.margin = unit(
      c(
        t = 0, b = 0,
        l = 1, r = 2
      ), "lines"
    )
  )  +
  labs(
    caption = "Fuente: Global Human Settlement Layer at 100 meters"
  ) +
  annotation_scale(
    location =  "br", 
    width_hint = 0.3, 
    text_col = 'grey60', 
    bar_cols = c('grey60', 'grey99'), 
    line_width = 0.15
    ) +
  annotation_north_arrow(
    location = "tr", 
    which_north = "true", 
    pad_x = unit(0.1, "in"), 
    pad_y = unit(0.2, "in"), 
    style = north_arrow_fancy_orienteering(
      text_col = 'grey40', 
      line_col = 'grey60', 
      fill = c('grey60', 'grey99')
      )
    ) 

ggsave(plot = gmap, filename = './png/maps/final-map_white.png', units = 'in', width = 9, height = 13, dpi = 300)



