
# Load libraries ----------------------------------------------------------
require(pacman)
p_load(ggplot2, tidyverse, classInt, showtext, colourpicker, extrafont, cptcity, RColorBrewer, sf, readxl, plotly, WDI, xlsx, FAOSTAT, rnaturalearth, rnaturalearthdata)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999)

# Font --------------------------------------------------------------------
font_add_google(family = 'Roboto Condensed', name = 'Roboto Condensed')
showtext_auto()

# Load data ---------------------------------------------------------------
shpf <- ne_countries(returnclass = 'sf', scale = 50)
tble <- read_excel(path = './tble/dabase_global_total.xlsx')[,-1]

# Loss ha map  ------------------------------------------------------------
# Deforestation 

dfrs <- tble %>% group_by(iso) %>% summarise(loss_ha = sum(loss_ha, na.rm = T)) %>% ungroup()
nrow(dfrs); nrow(shpf)
shpf_dfrs <- inner_join(shpf, dfrs, by = c('adm0_a3_us' = 'iso'))
shpf_dfrs <- dplyr::select(shpf_dfrs, adm0_a3_us, geounit, loss_ha, geometry)
shpf_dfrs <- shpf_dfrs %>% drop_na()

# find_cpt(name = 'hult'); image(matrix(1:100), col = cpt("hult_gr45_hult"))

# To classify the colors 
clss <- classIntervals(var = pull(shpf_dfrs, loss_ha), n = 6, style = 'fisher')
clss <- clss$brks
clss <- round(clss, -5)
clss <- c(clss[1], 500000, 1000000, clss[2:length(clss)])
hist(pull(shpf_dfrs, loss_ha))

lbls <- tibble(value = 1:8, inf = clss[1:8], sup = clss[2:9], interval = paste0(inf, '-', sup))
shpf_dfrs <- mutate(shpf_dfrs, class_lossHa = findInterval(x = shpf_dfrs$loss_ha, vec = clss, all.inside = TRUE))
shpf_dfrs <- full_join(shpf_dfrs, lbls, by = c('class_lossHa' = 'value'))
shpf_dfrs <- mutate(shpf_dfrs, interval = factor(interval, levels = lbls$interval))

g_dfrs <- ggplot() + 
  geom_sf(data = shpf_dfrs, aes(fill = interval), lwd = 0.25, col = 'grey50') + 
  # scale_fill_viridis_d() +
  scale_fill_manual(values = brewer.pal(n = 8, name = 'YlOrBr')) +
  # scale_fill_gradientn(colors = rev(cpt(n = 10, 'hult_gr51_hult'))) + 
  labs(x = '', y = '', fill = 'Cobertura de bosque pérdida (ha)', caption = 'Adaptado de U. Maryland') +
  ggtitle(label = 'Área en hectáreas de bosque deforestado entre 2000 y 2020') +
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        panel.background = element_rect(fill = 'white'),  # 2C619E
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        # legend.key.width = unit(3, 'line'), 
        legend.text = element_text(family = 'Roboto Condensed', size = 20), 
        axis.text.x = element_blank(),
        plot.title = element_text(family = 'Roboto Condensed', size = 36, hjust = 0.5, face = 'bold'),
        axis.text.y = element_blank(), 
        plot.caption = element_text(family = 'Roboto Condensed', size = 20),
        axis.title = element_text(family = 'Roboto Condensed', size = 24, face = 'bold'),
        legend.title = element_text(family = 'Roboto Condensed', size = 30))

ggsave(plot = g_dfrs, filename = 'png/global_deforested.jpg', units = 'in', width = 9, height = 6, dpi = 300)

# Population growth -------------------------------------------------------
popu <- tble %>% group_by(country, iso) %>% dplyr::summarise(pop_growth = mean(pop_growth, na.rm = T)) %>% ungroup() %>% drop_na()
popu <- inner_join(shpf, popu, by = c('adm0_a3_us' = 'iso'))

g_popu <- ggplot() + 
  geom_sf(data = popu, aes(fill = pop_growth), lwd = 0.25, col = 'grey50') + 
  # scale_fill_manual(values = brewer.pal(n = 8, name = 'YlOrBr')) +
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'YlGnBu')) +
  labs(x = '', y = '', fill = 'Crecimiento poblacional (%)', caption = 'Adaptado del Banco Mundial') +
  ggtitle(label = 'Crecimiento promedio de la población') +
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        panel.background = element_rect(fill = 'white'),  # 2C619E
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.width = unit(3, 'line'),
        legend.text = element_text(family = 'Roboto Condensed', size = 20), 
        axis.text.x = element_blank(),
        plot.title = element_text(family = 'Roboto Condensed', size = 36, hjust = 0.5, face = 'bold'),
        axis.text.y = element_blank(), 
        plot.caption = element_text(family = 'Roboto Condensed', size = 20),
        axis.title = element_text(family = 'Roboto Condensed', size = 24, face = 'bold'),
        legend.title = element_text(family = 'Roboto Condensed', size = 30))

ggsave(plot = g_popu, filename = 'png/population_growth.png', units = 'in', width = 9, height = 6, dpi = 300)

# Food inflation ----------------------------------------------------------
finf <- tble %>% group_by(country, iso) %>% dplyr::summarise(food_inflation = mean(food_inflation, na.rm = T)) %>% ungroup()
finf <- drop_na(finf)
finf <- inner_join(shpf, finf, by = c('adm0_a3_us' = 'iso'))

# To classify the colors 
clss <- classIntervals(var = pull(finf, food_inflation), n = 6, style = 'fisher')
clss <- clss$brks
clss <- round(clss, 0)
lbls <- tibble(value = 1:6, inf = clss[1:6], sup = clss[2:7], interval = paste0(inf, '-', sup))
finf <- mutate(finf, class_foodInflation = findInterval(x = finf$food_inflation, vec = clss, all.inside = TRUE))
finf <- full_join(finf, lbls, by = c('class_foodInflation' = 'value'))
finf <- mutate(finf, interval = factor(interval, levels = lbls$interval))

g_finf <- ggplot() + 
  geom_sf(data = finf, aes(fill = interval), lwd = 0.25, col = 'grey50') + 
  # scale_fill_manual(values = brewer.pal(n = 8, name = 'YlOrBr')) +
  scale_fill_manual(values = brewer.pal(n = 7, name = 'YlOrRd')) +
  labs(x = '', y = '', fill = 'Inflación (%)', caption = 'Adaptado de la FAO') +
  ggtitle(label = 'Inflación en los alimentos - Promedio entre 2000 y 2020') +
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        panel.background = element_rect(fill = 'white'),  # 2C619E
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.text = element_text(family = 'Roboto Condensed', size = 20), 
        axis.text.x = element_blank(),
        plot.title = element_text(family = 'Roboto Condensed', size = 36, hjust = 0.5, face = 'bold'),
        axis.text.y = element_blank(), 
        plot.caption = element_text(family = 'Roboto Condensed', size = 20),
        axis.title = element_text(family = 'Roboto Condensed', size = 24, face = 'bold'),
        legend.title = element_text(family = 'Roboto Condensed', size = 30))

ggsave(plot = g_finf, filename = './png/food_inflation.png', units = 'in', width = 9, height = 6, dpi = 300)

# Task: make the other maps.... and share with me in fabioalexandercastro@gmail.com

