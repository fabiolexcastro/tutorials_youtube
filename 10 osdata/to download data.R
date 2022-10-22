
# Fabio Alexander Castro Llanos 
# Máster en Ciencias de la Información Geográfica y Geógrafo 
# Universidad de Salzburgo y Universidad del Valle

# IDESC Cali: http://ws-idesc.cali.gov.co:8081/geoserver/web/;jsessionid=C7A0573E922A863F7F310930468D28F0?wicket:bookmarkablePage=:org.geoserver.web.demo.MapPreviewPage

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(sf, fs, glue, tidyverse, osmdata, osrm)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
cali <- st_read('http://ws-idesc.cali.gov.co:8081/geoserver/idesc/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=idesc:mc_perimetro_urbano_anno_2000&maxFeatures=50&outputFormat=json')[4,]
plot(st_geometry(cali))

cmns <- st_read('http://ws-idesc.cali.gov.co:8081/geoserver/idesc/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=idesc:mc_comunas&maxFeatures=50&outputFormat=json')

# Project the shapefile 
cali <- st_transform(cali, st_crs(4326))
cmns <- st_transform(cmns, st_crs(4326))

# To download -------------------------------------------------------------
qbox <- opq(bbox = 'Cali') 

# Road
road <- add_osm_feature(opq = qbox, key = 'highway') # See more about map feature: https://wiki.openstreetmap.org/wiki/Map_features
sort(available_features())
road <- osmdata_sf(road)
road_line <- road$osm_lines
road_pnts <- road$osm_points
road_poly <- road$osm_polygons

drop_na(road_pnts)
road_pnts %>% dplyr::select(name) %>% drop_na() %>% pull(name) %>% sort() # Paradas MIO

plot(st_geometry(road_pnts))
plot(st_geometry(road_poly))

road_poly %>% dplyr::select(name) %>% drop_na() %>% pull(name) %>% sort()

# Amenities
amnt <- add_osm_feature(opq = qbox, key = 'amenity')
amnt <- osmdata_sf(amnt)
amnt_line <- amnt$osm_lines
amnt_pnts <- amnt$osm_points
plot(st_geometry(amnt_pnts))

sum(is.na(amnt_pnts$name))
nrow(amnt_pnts)

amnt_pnts <- dplyr::select(amnt_pnts, name, amenity, geometry)
table(amnt_pnts$amenity)

# To search the rivers 
rver <- add_osm_feature(opq = qbox, key = 'waterway', value = 'river')
rver <- osmdata_sf(rver)
rver <- rver$osm_lines

plot(st_geometry(cali))
plot(st_geometry(rver), col = 'blue', add = TRUE)

# To write the results  ---------------------------------------------------
dir_create('./gpkg')
st_write(obj = cali, './gpkg/cali.gpkg')
st_write(obj = amnt_pnts, './gpkg/amenities_points.gpkg')
st_write(obj = road_line, './gpkg/roads_lines.gpkg')
st_write(obj = rver, './gpkg/river.gpkg')
st_write(obj = cmns, './gpkg/comunas.gpkg')

