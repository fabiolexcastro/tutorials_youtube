

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, leaflegend, stringr, sf, tidyverse,
               gtools, leaflet, plotly, foreign, htmltools, mapview, 
               leaflet.extras)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Source: https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwiYgOLljqv1AhV4TDABHaESAJUQFnoECAsQAQ&url=https%3A%2F%2Ffirms.modaps.eosdis.nasa.gov%2F&usg=AOvVaw0SpBeUeSeQUbS9N4kFVS_Z

# Load data ---------------------------------------------------------------
fire <- read.dbf('../data/seccion 3/INCENDIOS/Incendios_ValleCauca.dbf')
fire <- as_tibble(fire)
mpio <- st_read('../data/seccion 3/MGN2018_MPIO_POLITICO/MGN_MPIO_POLITICO.shp')

# Filtering dpto: Valle del Cauca -----------------------------------------
mpio <- filter(mpio, DPTO_CNMBR == 'VALLE DEL CAUCA')

# Table to shapefile ------------------------------------------------------
shpf <- st_as_sf(x = fire, coords = c('LONGITUDE', 'LATITUDE'), crs = st_crs(4326))

hist(shpf$CONFIDENCE)

# Filtering confidence > 50 -----------------------------------------------
shpf <- filter(shpf, CONFIDENCE >= 75)
plot(st_geometry(shpf))

# Leaflet map -------------------------------------------------------------

leaflet() %>% 
  addTiles()

shpf %>% 
  leaflet() %>%
  addTiles() %>% 
  addMarkers()

# Base leaflet
shpf %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers()

# ESRI World image
shpf %>% 
  leaflet() %>% 
  addProviderTiles(
    provider = "Esri.WorldImagery",
    group = "Satelital"
  ) %>% 
  addMarkers()

# Changing the icon

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

icon <- icons('C:/Users/Usuario/Pictures/Icons/fire_v4.svg', iconWidth = 20, iconHeight = 20)

shpf %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(
    icon = icon,
    popup = sprintf(paste0('FRP: ', as.character(shpf$FRP), ' - Fecha: ', as.character(shpf$ACQ_DATE)))
  ) 

shpf %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    data = mpio, 
    col = 'black', 
    fillOpacity = 0.1, 
    popup = sprintf(paste0('Municipio: ', mpio$MPIO_CNMBR))
  ) 

shpf %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(
    icon = icon,
    popup = sprintf(paste0('FRP: ', as.character(shpf$FRP), ' - Fecha: ', as.character(shpf$ACQ_DATE)))
  ) %>% 
  addPolygons(
    data = mpio, 
    col = 'black', 
    fillOpacity = 0.1, 
    popup = sprintf(paste0('Municipio: ', mpio$MPIO_CNMBR))
  ) 

dir.create(path = '../html')

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
    popup = sprintf(paste0('FRP: ', as.character(shpf$FRP)))
  ) %>% 
  addPolygons(
    data = mpio, 
    col = 'white', 
    fillOpacity = 0.1, 
    popup = sprintf(paste0('Municipio: ', mpio$MPIO_CNMBR))
  ) %>% 
  addControl(html = "<p><strong><em>Visualización de datos de NASA-FIRE para el Valle del Cauca</em></strong></p>",
             position = "topright")%>%
  addControl(html = "<p><strong><em>Autor: Fabio Castro</em></strong></p>",
             position = "bottomright") %>% 
  addResetMapButton() %>% 
  mapshot(url = "../html/index.html")


