
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, forcats, xgboost, hrbrthemes, caret, glue, fs, sf, showtext, rnaturalearthdata, rnaturalearth, rmapshaper, fs, tidytext)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Font
font_add(family = "Bahnschrift", regular = "Bahnschrift.ttf")
showtext_auto()

# Load data ---------------------------------------------------------------
tble <- read_csv('./out/results_run1.csv') %>% suppressMessages()
fix(tble)
lbls <- read_csv('./out/labels.csv') %>% suppressMessages()

tble <- tble %>% mutate(variable = fct_reorder(variable, avg, .desc = TRUE))
tble
tble <- tble %>% arrange(desc(avg))

tble %>% filter(variable != 'rsquared') %>% pull(avg) %>% sum()
fix(tble)

# To make the graph -------------------------------------------------------
gbar <- ggplot(data = tble, aes(x = variable, y = avg, fill = variable, col = variable)) + 
  geom_col(position = 'dodge') +
  geom_errorbar(aes(x = variable, ymin = min, ymax = max, group = variable), position = position_dodge(width = 0.9), width = 0.5, color = '#252525') +
  scale_x_discrete(labels = tble$nombre) +   
  scale_fill_manual(values = c('#969696', "#d53e4f", "#f46d43", "#fdae61", "#e6f598", "#abdda4", "#66c2a5", "#3288bd", '#EDC949'),
                    labels =  tble$nombre) +
  scale_color_manual(values = c('#969696', "#d53e4f", "#f46d43", "#fdae61", "#e6f598", "#abdda4", "#66c2a5", "#3288bd", '#EDC949')) +
  geom_text(aes(y = avg, label = round(avg, 2)), vjust = 0, size = 10, fontface = 'bold', position =  position_dodge(width = 0.9), col = 'grey40') + 
  theme_ipsum_ps() + 
  labs(x = '', y = 'Importance', fill = '') +
  theme(axis.text.x = element_text(angle = 45, family = 'Bahnschrift', size = 28, vjust = 0.5),
        axis.text = element_text(family = 'Bahnschrift', size = 28), 
        axis.text.y = element_text(family = 'Bahnschrift', size = 28, face = 'bold'),
        axis.title.x = element_text(family = 'Bahnschrift', face = 'bold', size = 28),
        axis.title.y = element_text(family = 'Bahnschrift', face = 'bold', size = 28),
        panel.grid.major = element_blank(),
        legend.text = element_text(family = 'Bahnschrift', size = 28), 
        legend.title = element_text(family = 'Bahnschrift', face = 'bold', size = 28),
        strip.text = element_text(family = 'Bahnschrift', face = 'bold', hjust = 0.5, size = 28),
        legend.background = element_rect(color = NA),
        legend.position = 'bottom') +
  guides(fill = guide_legend(nrow = 2), 
         color = 'none')

gbar

ggsave(plot = gbar, filename = './png/graph_importance.png', units = 'in', width = 11, height = 7, dpi = 300)




# Finish
