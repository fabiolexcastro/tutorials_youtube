

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, ggrepel, stringr, sf, hrbrthemes, gridExtra, colorspace, cartography, ghibli, ggspatial, ggthemes, ggpubr, exactextractr, tidyverse, gtools, microbenchmark)
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
col <- raster::getData(name = 'GADM', country = 'COL', level = 1)
tm1 <- raster::getData(name = 'worldclim', var = 'tmean', res = 0.5, lon = -76.5, lat = 6) %>% mean()
tm2 <- raster::getData(name = 'worldclim', var = 'tmean', res = 0.5, lon = -76.5, lat = -1) %>% mean()
cnt <- sf::st_read('D:/academico/maestria/tesis/data/shp/bse/all_countries.shp')

# Division ----------------------------------------------------------------
tm1 <- tm1 / 10
tm2 <- tm2 / 10

# Mosaicking --------------------------------------------------------------
tmp <- raster::mosaic(tm1, tm2, fun = 'mean')
tmp <- raster::crop(tmp, col) %>% raster::mask(., col)

# Zonal -------------------------------------------------------------------
col <- st_as_sf(col)
col$gid <- 1:nrow(col)
plot(st_geometry(col), add = TRUE)

col.rst <- rasterize(as(col, 'Spatial'), tmp, field = 'gid')

znl_a <- raster::zonal(tmp, col.rst, fun = 'mean')
znl_b <- exact_extract(tmp, col, 'mean')

# Count the seconds ------------------------------------------------------
microbenchmark(
  znl_a <- raster::zonal(tmp, col.rst, fun = 'mean'),
  znl_b <- exact_extract(tmp, col, 'mean'),
  times = 1,
  unit = 's'
)

system.time(expr = {znl_a <- raster::zonal(tmp, col.rst, fun = 'mean')})
system.time(expr = {znl_b <-  exact_extract(tmp, col, 'mean')})

# Now to make the graph and the map -------------------------------------
tmn.tbl <- raster::rasterToPoints(tmp, spatial = FALSE)
tmn.tbl <- as_tibble(tmn.tbl)

# Zonal table ----------------------
col$tmp <- znl_b
znl <- col %>% st_centroid %>% st_coordinates %>% as_tibble %>% mutate(temp = round(znl_b, 1))
znl$name <- col$NAME_1

gmap <- ggplot() + 
  geom_tile(data = tmn.tbl, aes(x = x, y = y, fill = layer)) +
  scale_fill_binned_sequential(palette = 'Heat', breaks = seq(0, 30, 5)) +
  geom_sf(data = col, fill = NA) + 
  geom_sf(data = cnt %>% filter(CONTINENT %in% c('South America', 'North America')), fill = NA) + 
  coord_sf(xlim = c(-79.5, extent(tmp)[2]), ylim = c(extent(tmp)[3], 12)) +
  ggtitle(label = 'Temperatura promedio anual para Colombia') +
  geom_text_repel(data = znl, aes(x = X, y = Y, label = temp), fontface = 'bold', color = 'white', size = 2.5) +
  geom_text_repel(data = znl, aes(x = X, y = Y, label = name), 
                  color = '#A4A4A4', size = 2, force = 2,
                  box.padding = 1, segment.color = 'grey10') +
  theme_ipsum_es() +
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 13, face = 'bold', hjust = 0.5)) +
  scale_x_continuous(breaks = c(-82, -78, -74, -70)) +
  labs(x = '', y = '', fill = 'Temperatura (C)') 

dir.create('../png')
ggsave(plot = gmap, filename = '../png/map_v2.jpg', units = 'in', width = 6, height = 7, dpi = 300)

znl.top <- znl %>% top_n(x = ., wt = temp, n = 5) 
znl.top <- znl.top %>% arrange(desc(temp))
znl.top <- znl.top %>% mutate(name = factor(name, levels = name))

gpnt <- ggplot(data = znl.top, aes(x = name, y = temp)) + 
  geom_point() + 
  scale_y_continuous(limits = c(26, 28), breaks = seq(26, 28, 0.25)) +
  theme_ipsum_es() + 
  theme() +
  labs(x = '', y = 'Temperatura (C)')

# To make the boxplot
col.sub <- col %>% filter(NAME_1 %in% pull(znl.top, name)) %>% as(., 'Spatial')
tmn.sub <- map(.x = 1:nrow(col.sub@data), function(k){
  raster::crop(tmp, col.sub[k,]) %>% raster::mask(., col.sub[k,]) %>% rasterToPoints() %>% as_tibble() %>% mutate(dpto = col.sub@data$NAME_1[k])
}) %>% bind_rows()

gbxp <- ggplot(data = tmn.sub, aes(x = dpto, y = layer, group = dpto, fill = dpto)) + 
  geom_boxplot() +
  scale_fill_manual(values = qualitative_hcl(15, palette = "Harmonic")[1:5]) +
  theme_ipsum_es() +
  theme(legend.position = c(0.88, 0.3)) +
  labs(x = '', y = 'Temperatura (C)', fill = '')


fig1 <- annotate_figure(gmap, top = text_grob(''))
fig2 <- annotate_figure(gpnt, top = text_grob('Top 5 - Departamentos con mayor temperatura', color = 'black', face = 'bold', size = 13))
fig3 <- annotate_figure(gbxp, top = text_grob('Comportamiento de la temperatura para el top 5', color = 'black', face = 'bold', size = 13))

layout <- matrix(c(1, 2, 1, 3), nrow = 2, ncol = 2, byrow = TRUE)# layout <- matrix(c(1, 1, 2, 1, 1, 3, 1, 1, 4 ), nrow = 3, ncol = 3, byrow = TRUE)
all <- grid.arrange(fig1, fig2, fig3, layout_matrix = layout)

ggsave(plot = all, filename = '../png/figures_v1.png', units = 'in', width = 11, height = 11, dpi = 300)
ggsave(plot = all, filename = '../png/figures_v1.jpg', units = 'in', width = 11, height = 8, dpi = 300)

