
# Fabio Alexander Castro Llanos 
# Máster en Ciencias de la Información Geográfica y Geógrafo 

# Load libraries ----------------------------------------------------------
require(pacman)
p_load(raster, terra, sf, tidyverse, leaflet, plotly, rnaturalearthdata, rmapshaper, geodata, rnaturalearth, gtools, rgbif, glue, htmltools, mapview, leaflet.extras, fs)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Download ----------------------------------------------------------------
spce <- 'Espeletia barclayana'
occr <- occ_data(scientificName = spce, limit = 2e5, hasCoordinate = T, hasGeospatialIssue = F)
occr <- occr[[2]]
colnames(occr)

# occr <- dplyr::select(occr, scientificName, decimalLongitude, decimalLatitude, continent)
head(occr)

# World -------------------------------------------------------------------
wrld <- ne_countries(scale = 50, type = 'countries', returnclass = 'sf')

plot(st_geometry(wrld))
points(occr$decimalLongitude, occr$decimalLatitude)

col1 <- st_read('D:/data/spatial/igac/dptos.gpkg')
plot(st_geometry(col1))

# Simplify
col1 <- rmapshaper::ms_simplify(input = col1)

# Table to shapefile ------------------------------------------------------

shpf <- st_as_sf(x = occr, coords = c('decimalLongitude', 'decimalLatitude'), crs = st_crs(4326))


shpf %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers()

shpf %>% 
  leaflet() %>% 
  addProviderTiles(
    provider = "Esri.WorldImagery",
    group = "Satelital"
  ) %>% 
  addMarkers() %>% 
  addPolygons(
    data = col1, 
    col = 'black', 
    fillOpacity = 0.1, 
    popup = sprintf(paste0('Municipio: ', col1$DPTO_CNMBR))
  ) 

iconx <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion'
)


shpf %>% 
  leaflet() %>% 
  addProviderTiles(
    provider = "Esri.WorldImagery",
    group = "Satelital"
  ) %>% 
  addAwesomeMarkers(icon = iconx)

shpf %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    data = col1, 
    col = 'black', 
    fillOpacity = 0.1, 
    popup = sprintf(paste0('Municipio: ', col1$DPTO_CNMBR))
  ) 


icon <- icons('./icon/specie.png', iconWidth = 40, iconHeight = 40)

dir.create(path = './html')

shpf %>% 
  leaflet() %>% 
  addLayersControl(baseGroups = c("Satellite", "OSM","Terrain","CartoDB","Terrain.ESRI", "Toner Lite","CartoDB.Positron","Toner","heatmap de Castaña"),
                   overlayGroups = c( "Bloques", "Inventario"),
                   position = "topright",
                   options = layersControlOptions(collapsed = T)) %>%
  addProviderTiles(provider = "Esri.WorldImagery", group = "Satelital") %>% 
  addProviderTiles(providers$OpenStreetMap, group = "OSM")%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")%>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$CartoDB.DarkMatter,group = "CartoDB")%>%
  addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
  addProviderTiles(providers$Esri.WorldStreetMap, group = "Terrain.ESRI") %>%
  addProviderTiles(providers$CartoDB.Positron, group ="CartoDB.Positron") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addMiniMap(tiles = providers$Esri.WorldImagery,toggleDisplay = TRUE, position = "bottomleft")%>%
  addMarkers(
    icon = icon,
    popup = sprintf(paste0('Fecha del registro\n: ', as.character(shpf$EventDate)))
  ) %>% 
  addPolygons(
    data = col1, 
    col = 'white', 
    fillOpacity = 0.1, 
    popup = sprintf(paste0('Departamento: ', col1$DPTO_CNMBR))
  ) %>% 
  addControl(html = "<p><strong><em>Presencias de Espeletia barclayana en Colombia</em></strong></p>",
             position = "topright")%>%
  addControl(html = "<p><strong><em>Autor: Fabio Castro</em></strong></p>",
             position = "bottomright") %>% 
  addScaleBar(position = "bottomright",options = scaleBarOptions(maxWidth = 100,
                                                                 metric = TRUE,
                                                                 imperial = TRUE,
                                                                 updateWhenIdle = TRUE)) %>%
  addDrawToolbar(targetGroup = "Graficos",editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))%>%
  addMeasure(position = "topleft",
             primaryLengthUnit = "meters",
             primaryAreaUnit = "sqmeters",
             activeColor = "#3D535D",
             completedColor = "#7D4479")%>% 
  addSearchOSM() %>% 
  addControlGPS() %>% 
  addResetMapButton() %>% 
  mapshot(url = "./html/index.html")


