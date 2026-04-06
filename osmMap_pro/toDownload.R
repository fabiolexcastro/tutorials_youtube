
## Fabio Alexander Castro Llanos 
## Un geógrafo en YouTube

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(sf, fs, glue, tidyverse, terra, osmdata, osrm)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999, warn = -1)

# Function ----------------------------------------------------------------
fetch_green <- function(key, values, boundary) {
  
  bbox <- st_bbox(boundary)
  
  q <- opq(bbox = bbox) |>
    add_osm_feature(key = key, value = values)
  
  dat <- osmdata_sf(q)
  
  polys <- dat$osm_polygons
  
  if (!is.null(polys) && nrow(polys) > 0) {
    polys <- st_transform(polys, st_crs(boundary))
    polys$key   <- key
    polys$value <- polys[[key]]
    
    polys_clipped <- st_intersection(polys, boundary)
    return(polys_clipped)
  }
  
  NULL

}
    
# To download -------------------------------------------------------------
qbox <- opq(bbox = 'Cali') 

## Limits ----------------------------------------------
lims <- qbox %>% add_osm_feature(key = "boundary", value = "administrative") %>% osmdata_sf()
lims <- lims$osm_multipolygons
cali <- lims %>% filter(name == 'Cali')

### Levels Barrios + Comunas
lvls <- unique(lims$admin_level)
brrs <- lims %>% filter(admin_level == 9) 
cmns <- lims %>% filter(admin_level == 8)

## Road -------------------------------------------------
road <- add_osm_feature(opq = qbox, key = 'highway') # See more about map feature: https://wiki.openstreetmap.org/wiki/Map_features
sort(available_features())
road <- osmdata_sf(road)
road_line <- road$osm_lines
road_pnts <- road$osm_points
road_poly <- road$osm_polygons

## Rivers ----------------------------------------------
rvrs <- add_osm_feature(opq = qbox, key = 'waterway', value = 'river')
rvrs <- osmdata_sf(rvrs)
rvrs <- rvrs$osm_lines

## Zonas verdes ----------------------------------------
green_list_cali <- list(fetch_green(key = "leisure", values = c("park", "garden", "recreation_ground", "pitch", "nature_reserve"), boundary = cali), 
                        fetch_green(key = "landuse",  values = c("forest", "grass", "meadow"), boundary = cali), 
                        fetch_green(key = "natural", values = c("wood", "heath", "scrub"), boundary = cali))
saveRDS(object = green_list_cali, file = './rds/green_list_cali.rds')

green_list_cali <- readRDS(file = './rds/green_list_cali.rds')
green_list_cali <- lapply(green_list_cali, function(x) {if(!is.null(x)) dplyr::select(x, osm_id, name, key, value, geometry)})

leis <- green_list_cali[[1]]
land <- green_list_cali[[2]]
natu <- green_list_cali[[3]]

# To write the results  ---------------------------------------------------
dir_create('./gpkg')
st_write(obj = cali, './gpkg/cali.gpkg')
st_write(obj = brrs, './gpkg/barrios.gpkg')
st_write(obj = cmns, './gpkg/comunas.gpkg')
st_write(obj = road_line, './gpkg/road_line.gpkg')
st_write(obj = road_poly, './gpkg/road_poly.gpkg')
st_write(obj = rvrs, './gpkg/rivers.gpkg')
st_write(obj = leis, './gpkg/leisure.gpkg')
st_write(obj = land, './gpkg/land.gpkg')
st_write(obj = natu, './gpkg/natu.gpkg')

st_write(obj = lims, './gpkg/lims.gpkg')

