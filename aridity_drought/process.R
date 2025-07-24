

## Fabio Castro - Llanos 
## Msc. GIS - Geographer 
## fabioalexandercastro@gmail.com
## Aridity and drought layers - Spatial Analysis

## Zomer RJ, Xu J, Spano D and Trabucco A. 
## CMIP6-based global estimates of future aridity index and potential evapotranspiration for 2021-2060 
## [version 4; peer review: 3 approved]. Open Res Europe 2025, 4:157 
## https://doi.org/10.12688/openreseurope.18110.4    

## Dataset for downloading: 
## https://www.scidb.cn/detail?dataSetId=11e920c1ee144fc2a691951096b96cbc 

# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, fs, sf, rmapshaper, cowplot, ggpubr, ggspatial, tidyverse, geodata, RColorBrewer)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Vector data  ------------------------------------------------------------
wrld <- geodata::world(resolution = 1, level = 0, version = 'latest', path = tempdir())
zone <- wrld[wrld$GID_0 == 'ECU',]

# Raster data -------------------------------------------------------------
bsln <- terra::rast('./WorldClim_2_1_1970-2000/aridity_index.tif')
ftre <- terra::rast('./MultiModel_Ensemble_All_585_2041-2060/aridity_index.tif')

# Extract by mask ---------------------------------------------------------
bsln <- terra::crop(bsln, zone)
bsln <- terra::mask(bsln, zone)
ftre <- terra::crop(ftre, zone)
ftre <- terra::mask(ftre, zone)

stck <- c(bsln, ftre)

# To reclassify -----------------------------------------------------------
mtrx <- matrix(c(-Inf, 0.03, 1,
                 0.03, 0.2,  2,
                 0.2,  0.5,  3,
                 0.5,  0.65, 4, 
                 0.65,  Inf,  5),
               ncol = 3, byrow = T)

rcls <- terra::classify(stck, mtrx)
names(rcls) <- c('Baseline', 'Future')

# Raster to polygon -------------------------------------------------------
pols.bsln <- terra::as.polygons(rcls[[1]])
pols.ftre <- terra::as.polygons(rcls[[2]])

# Simplify the polygons ---------------------------------------------------
pols.bsln <- rmapshaper::ms_simplify(st_as_sf(pols.bsln), keep = 0.08)
pols.ftre <- rmapshaper::ms_simplify(st_as_sf(pols.ftre), keep = 0.08)

# To draw the maps  -------------------------------------------------------

## Function
make.map <- function(shp, ttl){
  
  ##
  cat('To start the process!\n')  
  names(shp) <- c('Class', 'geometry')
  shp <- inner_join(
    shp, 
    tibble(Class = c(1, 2, 3, 4, 5), 
           Type = c('Hyper-Arid', 'Arid', 'Semi-Arid', 'Dry Sub-Humid', 'Humid'))
  )
  
  shp <- mutate(shp, Type = factor(Type, levels = c('Hyper-Arid', 'Arid', 'Semi-Arid', 'Dry Sub-Humid', 'Humid')))

  ## 
  clrs <- brewer.pal(n = 5, name = 'BrBG')
  names(clrs) <-  c('Hyper-Arid', 'Arid', 'Semi-Arid', 'Dry Sub-Humid', 'Humid')
  
  ## 
  g1 <- ggplot() + 
    geom_sf(data = shp, aes(fill = Type), col = 'grey30') + 
    scale_fill_manual(values = clrs) +
    geom_sf(data = st_as_sf(wrld), fill = NA, col = 'grey30') +
    coord_sf(xlim = c(-81, -75), ylim = c(-5, 1.5)) +
    ggtitle(label = ttl) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(hjust = 0.5, size = 6), 
      axis.text.y = element_text(hjust = 0.5, angle = 90, size = 6),
      legend.position = 'bottom', 
      legend.title = element_text(hjust = 0.5, face = 'bold'), 
      legend.title.position = 'top',
      plot.title = element_text(hjust = 0.5, face = 'bold')
    ) +
    annotation_scale(location =  "bl", width_hint = 0.5, text_family = 'georg', text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.2) +
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                           style = north_arrow_fancy_orienteering(text_family = 'georg', text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 
  
  return(g1)
  
  
}

## To draw the map
g1 <- make.map(shp = pols.bsln, ttl = 'Baseline')
g2 <- make.map(shp = pols.ftre, ttl = 'Future')

## Join both maps into only one
g3 <- ggpubr::ggarrange(g1, g2, ncol = 2, nrow = 1, common.legend = T, legend = 'bottom')
ggsave(plot = g3, filename = './png/ai_bsl-ftr.png', units = 'in', width = 7, height = 5, dpi = 300, create.dir = T)

# To count the area -------------------------------------------------------

pols.bsln <- vect(pols.bsln)
area.bsln <- terra::expanse(pols.bsln, unit = 'ha')
pols.ftre <- vect(pols.ftre)
area.ftre <- terra::expanse(pols.ftre, unit = 'ha')

area.bsln <- tibble(
  value = pols.bsln$Baseline, 
  type = c('Arid', 'Semi-Arid', 'Dry Sub-Humid', 'Humid'),
  ha = area.bsln, 
  period = 'Baseline'
)

area.ftre <- tibble(
  value = pols.ftre$Future, 
  type = c('Arid', 'Semi-Arid', 'Dry Sub-Humid', 'Humid'),
  ha = area.ftre, 
  period = 'Future'
)

area <- rbind(area.bsln, area.ftre) %>% 
  spread(period, ha)

write.csv(area, './tbl/area_bsl-ftr.csv', row.names = FALSE)

area$difference <- area$Future - area$Baseline
area
