

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(raster, terra, sf, nabor, fs, ranger, broom, viridis, CAST, ggpubr, lubridate, tidyverse, glue, geodata, gtools, rgeos, meteo, sp, spacetime, gstat, plyr, xts, snowfall, doParallel,  CAST, ranger)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Reference ---------------------------------------------------------------
# Montgomery, E. & Vining, D. & Peck. 2006. Introducción Al Análisis de Regresión Lineal. 3ed ed. México: Cecsa.

# Load data ---------------------------------------------------------------
dbse <- read_csv('tbl/database.csv')
 
# Formula
frml <- value ~ srtm

# To make the linear model ------------------------------------------------
olsm <- lm(frml, data = dbse)
summary(olsm)

# Convert the model to a dataframe
mdel <- tidy(summary(olsm))

names(olsm)

# Get the residual from the model -----------------------------------------
dbse <- mutate(dbse, residual = olsm$residuals)

# Add the predicted values ------------------------------------------------
dbse <- mutate(dbse, predicted = predict(olsm))

# To make the graph -------------------------------------------------------
gmod <- ggplot(dbse, aes(x = srtm, y = value)) +
  geom_smooth(method="lm", se=FALSE, color="lightgrey") +
  geom_segment(aes(xend = srtm, yend = predicted), col='red', lty='dashed') +
  geom_point() +
  geom_point(aes(y = predicted), col='red') +
  labs(x = 'SRTM', y = 'Temperatura (\u00B0C)') +
  theme_light() + 
  theme(axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'))

ggsave(plot = gmod, filename = 'png/model_linear.png', units = 'in', width = 7, height = 5, dpi = 300)
