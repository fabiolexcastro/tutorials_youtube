

require(pacman)
pacman::p_load(hrbrthemes, grid, png, ggrepel, cowplot, magick, cptcity, ggnewscale, RColorBrewer, gridExtra, ggpubr, ggspatial, extrafont, showtext)

font_add_google(family = 'Roboto', name = 'Roboto condensed' )
showtext_auto()

wrld <- st_read('D:/data/world/all_countries.shp') %>% filter(CONTINENT %in% c('North America', 'South America'))

my_theme <- theme(axis.text.x = element_text(size = 19, family = 'Roboto'), 
                  axis.text.y = element_text(size = 19, family = 'Roboto', angle = 90, hjust = 0.5),
                  axis.title.x = element_text(size = 26, family = 'Roboto'), 
                  axis.title.y = element_text(size = 26, family = 'Roboto'),
                  legend.text = element_text(size = 21, family = 'Roboto'),
                  plot.title = element_text(size = 30, face = 'bold', hjust = 0.5, family = 'Roboto'),
                  plot.subtitle = element_text(size = 30, face = 'bold', hjust = 0.5, family = 'Roboto'),
                  legend.title = element_text(size = 26, family = 'Roboto', face = 'bold'),
                  legend.position = 'bottom',
                  legend.key.width = unit(2.0, 'line'),
                  panel.background = element_rect(fill = "white"),
                  plot.caption = element_text(size = 10, face = 'bold', family = 'Roboto'),
                  panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.1))
