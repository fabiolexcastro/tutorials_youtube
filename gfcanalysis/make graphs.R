
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, gfcanalysis, tidyverse, xlsx, readxl, extrafont, showtext)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Font --------------------------------------------------------------------
windowsFonts(georg = windowsFont('Georgia'))

# Load data ---------------------------------------------------------------
excel_sheets('tble/forest_caqueta.xlsx')
tble.frst <- read_excel(path = 'tble/forest_caqueta.xlsx', sheet = 'Forest')
tble.loss <- read_excel(path = 'tble/forest_caqueta.xlsx', sheet = 'Loss')
tble.gain <- read_excel(path = 'tble/forest_caqueta.xlsx', sheet = 'Gain')

tble.frst

# Forest 2000 -------------------------------------------------------------
tble.frst <- mutate(tble.frst, municipio = str_to_title(municipio))
tble.frst <- mutate(tble.frst, class = factor(class, levels = c('No bosque', 'Bosque')))

# Order by forest
tble.frst
ordr <- tble.frst %>% filter(class == 'Bosque') %>% arrange(desc(forest_hectares)) %>% pull(municipio)
tble.frst <- mutate(tble.frst, municipio = factor(municipio, levels = ordr))

gfrst_2000 <- ggplot(data = tble.frst, aes(x = municipio, y = forest_hectares, group = municipio, fill = class)) + 
  geom_col(stat = 'identity') + 
  scale_fill_manual(values = c('#AB2425', '#299140')) +
  labs(x = 'Municipio', y = 'Hectareas', fill = 'Tipo') +
  ggtitle(label = 'Cobertura de bosque y no bosque para el año 2000') +
  theme_minimal() + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.background = element_rect(fill = 'white'),
        axis.title = element_text(face = 'bold'),
        legend.title = element_text(face = 'bold'),
        legend.key = element_rect(colour = 'grey50'),
        plot.title = element_text(face = 'bold', hjust = 0.5, size = 18),
        legend.position = c(0.8, 0.8),
        text = element_text(family = 'georg', color = 'grey50')) + 
  guides(fill = guide_legend(title.position = 'top', title.hjust = 0.5))

gfrst_2000
ggsave(plot = gfrst_2000, filename = 'png/forest_2000.png', units = 'in', width = 13, height = 7, dpi = 300)
