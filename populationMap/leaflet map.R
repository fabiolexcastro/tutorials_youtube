

# Load libraries ----------------------------------------------------------
require(pacman)
p_load(raster, terra, sf, tidyverse, leaflet, classInt, plotly, rnaturalearthdata, rmapshaper, geodata, rnaturalearth, gtools, glue, htmltools, mapview, leaflet.extras, fs)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
shpf <- st_read('./gpkg/population.gpkg')
shpf

# To make the intervals ---------------------------------------------------
brks <- classIntervals(var = shpf$total_2017, n = 6, style = 'quantile')
brks <- brks$brks
brks <- round(brks, digits = -2)
brks[7] <- brks[7] + 600

clss <- findInterval(x = pull(shpf, total_2017), vec = brks, all.inside = TRUE)
shpf <- mutate(shpf, class = clss)
lbls <- tibble(class = 1:6, min = brks[1:6], max = brks[2:7], interval = paste0(min, '-', max))

# Join intervals with the shapefile ---------------------------------------
shpf <- inner_join(shpf, lbls, by = 'class')
shpf <- mutate(shpf, interval = factor(interval, levels = lbls$interval))
shpf <- rmapshaper::ms_simplify(shpf)

# Get the colors -----------------------------------------------------------
pal <- colorNumeric("viridis", NULL)
paln <- brewer.pal(n = 6, name = 'YlOrRd')
clrs <- tibble(clases = lbls$interval, paln)
clrs <- mutate(clrs, clases = factor(clases, levels = lbls$interval))
shpf <- mutate(shpf, clases = interval)
shpf <- inner_join(shpf, clrs, by = 'clases')

# Categorical (factor) -----------------------------------------------------

leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.DarkMatter,group = "CartoDB") %>%
  addPolygons(data = shpf,
              stroke = TRUE, 
              color = 'white', 
              weight = 0.4, 
              fillColor = shpf$paln, 
              fillOpacity = 0.5,
              smoothFactor = 0.5,
              popup = paste0('Número de habiantes: ', shpf$total_2017, '<br>', 'Municipio:', ' ', str_to_title(shpf$MPIO_CNMBR), '<br>')) %>% 
  addLegend('bottomright',
            colors = clrs$paln,
            labels = clrs$clases, 
            title = 'Cantidad de habitantes') %>% 
  addScaleBar(position = "bottomright",options = scaleBarOptions(maxWidth = 100,
                                                                 metric = TRUE,
                                                                 imperial = TRUE,
                                                                 updateWhenIdle = TRUE)) %>%
  addDrawToolbar(targetGroup = "Graficos", editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))%>%
  addMeasure(position = "topleft",
             primaryLengthUnit = "meters",
             primaryAreaUnit = "sqmeters",
             activeColor = "#3D535D",
             completedColor = "#7D4479") %>% 
  addSearchOSM() %>% 
  addControlGPS() %>% 
  addMiniMap(tiles = "CartoDB.DarkMatter", position = 'topleft') %>% 
  addControl(html = "<p style = 'font-family:serif; font-weight:bold'><strong>Cantidad de habitantes a nivel municipal en Colombia (DANE - 2017)</strong></p>",
             position = "topright")%>%
  addControl(html = "<p style = 'font-family:serif; font-weight:bold'><strong>Autor: Fabio Castro</strong></p>",
             position = "bottomright") %>% 
  addResetMapButton() %>% 
  mapshot(url = "./html/index.html", title = 'Población Colombia')


# Continous ---------------------------------------------------------------

leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.DarkMatter,group = "CartoDB") %>%
  addPolygons(data = shpf,
              stroke = TRUE, 
              color = 'white', 
              weight = 0.4, 
              fillColor = shpf$total_2017, 
              fillOpacity = 0.5,
              smoothFactor = 0.5,
              popup = paste0('Número de habiantes: ', shpf$total_2017, '<br>', 'Municipio:', ' ', str_to_title(shpf$MPIO_CNMBR), '<br>')) %>% 
  addLegend('bottomright',
            colors = clrs$paln,
            title = 'Cantidad de habitantes') 



