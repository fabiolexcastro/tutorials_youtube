
## MapBiomas Download 
## Fabio Castro - Llanos  // Un Geógrafo en YouTube
## September 13 th 2025

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, ggspatial, RColorBrewer, gtools, glue, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Root URL ----------------------------------------------------------------
urls <- glue(
  'https://storage.googleapis.com/mapbiomas-public/initiatives/colombia/collection_2/lulc/mapbiomas_colombia_collection2_integration_v1/mapbiomas_colombia_collection2_integration_v1-classification_{1985:2024}.tif'
) |> as.character()

## Select some years
year <- seq(2000, 2020, 5)
urls <- urls[grep(paste0(year, collapse = '|'), urls, value = F)]

# To download -------------------------------------------------------------
map(.x = 1:length(urls), .f = function(i){
  
  ## To build the URL
  cat('To download: ', basename(urls[i]), '\n')
  urle <- urls[i]  
  dout <- glue('./tif/mapbiomas_col-raw')
  dir_create(dout)
  dnme <- basename(urle)
  fout <- paste0(dout, '/', dnme)
  
  ## To create the directory 
  dir_create(dout)
  
  ## To download
  if(!file.exists(fout)){
    cat('To download\n')
    download.file(url = urle, destfile = fout, mode = 'wb')  
  } else { 
    cat('Already download\n')
  }
  
  ## Finish 
  rm(urle, dout, dnme, fout)
  gc(reset = T)
  cat('Finish!\n')
  
})

# To extract by mask and make maps ----------------------------------------
col1 <- geodata::gadm(country = 'COL', level = 1, path = './tmpr')
col2 <- geodata::gadm(country = 'COL', level = 2, path = './tmpr')

## Select Cali 
cali <- col2[col2$NAME_2 == 'Santiago de Cali',]

## To read the rasters 
fles <- dir_ls('./tif/mapbiomas_col-raw')
rstr <- rast(fles)

## Crop / mask
rstr <- terra::crop(rstr, cali)
rstr <- terra::mask(rstr, cali)
names(rstr) <- glue('landcover_{seq(2000, 2020, 5)}')

## A simple plot 
plot(rstr)

## To write the raster 
dir_create('./tif/mapbiomas_cali-raw')
terra::writeRaster(x = rstr, filename = './tif/mapbiomas_cali-raw/landcover_2000-2020.tif', overwrite = TRUE)

# Raster to table ---------------------------------------------------------
vles <- terra::as.data.frame(rstr, xy = T)
vles <- vles |> gather(var, value, -c(x, y))
vles <- as_tibble(vles)
unique(vles$value)

# Build the legend for the raster values ----------------------------------

## Source: https://colombia.mapbiomas.org/wp-content/uploads/sites/3/2024/11/Codigo-de-la-Leyenda-coleccion-2-1.pdf

## Legend
lgnd <- tibble(value = sort(unique(vles$value)), class = c('Manglar', 'Silvicultura', 'Otra formación natural no forestal', 'Agricultura/Pasto', 'Urbano', 'Sin vegetación', 'Minería', 'Acuicultura', 'Cuerpo de agua'))
lgnd <- mutate(lgnd, color = c('#1F8D49', '#7A5900', '#D89F5C', '#FFEFC3', '#D4271E', '#DB4D4F', '#9C0027', '#091077', '#2532E4'))

## Join with the legend table 
vles <- vles |> 
  mutate(year = parse_number(var)) |> 
  inner_join(lgnd, by = 'value')

vles <- vles |> 
  mutate(year = factor(year, levels = seq(2000, 2020, 5)))

## Colors 
clrs <- pull(lgnd, color)
names(clrs) <- pull(lgnd, class)

## To draw the map 
gmap <- ggplot() + 
  geom_tile(data = vles, aes(x = x, y = y, fill = class)) + 
  scale_fill_manual(values = clrs) + 
  facet_wrap(.~year) + 
  labs(x = '', y = '', fill = 'Cobertura', caption = 'Adaptado de MapBiomas') +
  coord_sf() +
  theme_minimal() +
  theme(
    legend.position = 'bottom', 
    legend.title.position = 'top', 
    axis.text.y = element_text(angle = 90, hjust = 0.5, size = 6),
    axis.text = element_text(size = 6),
    legend.title = element_text(face = 'bold', hjust = 0.5)
  )

gmap
ggsave(plot = gmap, filename = './png/mapa_coberturas-Cali_2.jpg', units = 'in', width = 9, height = 7, dpi = 300, create.dir = T)


# Codigo: GEE


