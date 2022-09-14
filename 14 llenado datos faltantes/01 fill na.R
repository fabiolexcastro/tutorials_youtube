

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, naniar, glue, fs, imputeTS, lubridate)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
tble <- read_csv('tbl/complete_boyaca.csv')
stts <- unique(tble$code)
crds <- read_csv('https://raw.githubusercontent.com/fabiolexcastro/tutorials_youtube/master/12%20regression%20linear%20model/tbl/stations_boyaca.csv')

n_miss(tble)

# To count the NAs --------------------------------------------------------
cnas <- tble %>% 
  group_by(code) %>% 
  summarise(count_nas = sum(is.na(value))) %>% 
  ungroup() %>% 
  arrange(desc(count_nas))

# Station code: 24015090
dfrm <- tble %>% filter(code == 24015090) 

crds %>% filter(Code == 24015090)

# Some interesting functions ---------------------------------------------

# Some data
n_miss(dfrm)
miss_var_summary(dfrm)
miss_var_table(dfrm)
miss_var_run(dfrm, value)

# Some graphs
vis_miss(dfrm)
vis_miss(dfrm, cluster = T)
gg_miss_var(x = dfrm)
gg_miss_case(x = dfrm)
gg_miss_case(x = tble, facet = code)

# More about: https://rpubs.com/odenipinedo/dealing-with-missing-data-in-R

# Get the year, month and day as a individual column  ---------------------
dfrm <- dfrm %>% 
  mutate(month = month(date), 
         year = year(date), 
         day = day(date)) %>% 
  dplyr::select(code, year, month, day, date, value)

# Check the NA rows
dfrm[!complete.cases(dfrm$value),]

# Create a function -------------------------------------------------------
fill_nas <- function(mnt){
  
  # Filtering for the month
  cat(mnt, '\t')
  dfr <- filter(dfrm, month == mnt)
  pull(dfr, value)
  vls <- na_interpolation(x = dfr$value, option = 'spline') # linear, spline, stine
  vls <- round(vls, digits = 1)
  dfr <- mutate(dfr, value_2 = vls)
  cat('Done!\n')
  return(dfr)
  
}

dfrm <-  map_dfr(.x = 1L:12L, .f = fill_nas)
dfrm


# To make a graph ---------------------------------------------------------

# Raw data
g1 <- ggplot(data = dfrm, aes(x = date, y = value)) + 
  geom_line(group = 1, col = 'blue') + 
  scale_x_date(date_breaks = 'month', date_labels = '%B') + #?strptime
  ggtitle(label = 'Comportamiento de la temperatura para la estación climática del\nmunicipio Santa Sofia (Año 2020)') +
  labs(x = 'Mes', y = 'Temperatura (\u00B0C)') +
  theme_minimal() + 
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5, family = 'serif'), 
        axis.text.x = element_text(family = 'serif'), 
        axis.text.y = element_text(family = 'serif'), 
        axis.title.x = element_text(family = 'serif', face = 'bold'), 
        axis.title.y = element_text(family = 'serif', face = 'bold'))

g2 <- ggplot(data = dfrm, aes(x = date, y = value_2)) + 
  geom_line(group = 1, col = 'blue') + 
  scale_x_date(date_breaks = 'month', date_labels = '%B') + #?strptime
  ggtitle(label = 'Comportamiento de la temperatura para la estación climática del\nmunicipio Santa Sofia (Año 2020)') +
  labs(x = 'Mes', y = 'Temperatura (\u00B0C)') +
  theme_minimal() + 
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5, family = 'serif'), 
        axis.text.x = element_text(family = 'serif'), 
        axis.text.y = element_text(family = 'serif'), 
        axis.title.x = element_text(family = 'serif', face = 'bold'), 
        axis.title.y = element_text(family = 'serif', face = 'bold'))

ggsave(plot = g2, filename = 'png/temperatura_santasofia.png', units = 'in', width = 9, height = 7, dpi = 300)
