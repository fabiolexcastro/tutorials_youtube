

# Cite library: 
# L. Busetto, L. Ranghetti (2016) MODIStsp: An R package for automatic preprocessing of 
# MODIS Land Products time series, 
# Computers & Geosciences, Volume 97, Pages 40-48, ISSN 0098-3004,

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, rgeos, ggspatial, RColorBrewer, ggspatial, rts, MODIStsp, rgeoboundaries)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
dpto <- vect('D:/data/spatial/igac/dptos.gpkg')
mpio <- vect('D:/data/spatial/igac/mpios.gpkg')
valle <- dpto
valle <- valle[valle$DPTO_CNMBR == 'VALLE DEL CAUCA',]
valle.mpio <- mpio[mpio$DPTO_CNMBR == 'VALLE DEL CAUCA',]
writeVector(valle, 'shp/valle.shp')

path_work <- 'tif/ndvi_valle-raw_2'
dir_create(path_work)

# GUI - help
MODIStsp()

MODIStsp::MODIStsp_get_prodnames()
MODIStsp_get_prodlayers('Vegetation Indexes_16Days_250m (M*D13Q1)')

spatial_filepath <- 'shp/valle.shp'

# Download ----------------------------------------------------------------
MODIStsp(
  gui = FALSE,
  out_folder = path_work,
  out_folder_mod = path_work,
  selprod = "Vegetation Indexes_16Days_250m (M*D13Q1)",
  bandsel = "NDVI",
  start_date = "2020.01.01",
  end_date = "2020.12.31",
  verbose = FALSE,
  spatmeth = "file",
  spafile = spatial_filepath,
  out_format = "GTiff"
)

y# To extract by mask  -----------------------------------------------------
infile <- './tif/ndvi_valle-raw/valle/VI_16Days_250m_v6/Time_Series/RData/Mixed/NDVI/MOD13Q1_MYD13Q1_NDVI_1_2020_361_2020_RData.RData' 
rstr <- get(load(infile)) 
rstr$MOD13Q1_NDVI_2020_001
rstr$MYD13Q1_NDVI_2020_009
rstr$MOD13Q1_NDVI_2020_017
rstr$MYD13Q1_NDVI_2020_025
rstr$MOD13Q1_NDVI_2020_033

# Convert to terra library ------------------------------------------------
rstr <- rast(rstr)
avrg <- app(rstr, mean)
avrg <- avrg * 0.0001
avrg <- terra::project(avrg, crs(valle))
avrg <- terra::crop(avrg, valle)
avrg <- terra::mask(avrg, valle)

terra::writeRaster(x = avrg, filename = './tif/ndvi_valle-cut/ndvi_avrg-2020.tif', overwrite = T)

plot(avrg)

# To make the map ---------------------------------------------------------
tble <- terra::as.data.frame(avrg, xy = T) %>% 
  as_tibble() %>% 
  setNames(c('x', 'y', 'value'))

gmap <- ggplot() + 
  geom_tile(data = tble, aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c() + 
  geom_sf(data = st_as_sf(dpto), fill = NA, col = 'grey50') +
  geom_sf(data = st_as_sf(valle), fill = NA, col = 'grey80') +
  geom_sf(data = st_as_sf(valle.mpio), fill = NA, col = 'grey80') +
  coord_sf(xlim = ext(valle)[1:2], ylim = ext(valle)[3:4]) +
  labs(x = 'Lon', y = 'Lat', fill = 'NDVI', caption = 'MODIS - USGS') +
  ggtitle(label = 'NDVI en el Valle del Cauca - 2020') +
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        legend.key.width = unit(3, 'line'), 
        plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        axis.text.x = element_text(size = 5), 
        text = element_text(family = 'Barlow'),
        axis.text.y = element_text(size = 5, angle = 90, hjust = 0.5), 
        legend.title = element_text(face = 'bold', hjust = 0.5))  +
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
  )) +
  annotation_scale(location =  "bl", width_hint = 0.5, text_family = 'Barlow', text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'Barlow', text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 

ggsave(plot = gmap, filename = './png/ndvi_v2.png', units = 'in', width = 7, height = 7, dpi = 300)


