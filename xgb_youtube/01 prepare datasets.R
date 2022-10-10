
# Prepare datasets 
# Castro-Llanos Fabio Alexander

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, fs)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
db <- read_csv('db.csv')
vars <- c('iso3', 'continent', 'year', 'df_ha', 'gdp_perc', 'food_exports', 'exports_gdp', 'food_infl',
          'pop_growth', 'gdp_growth', 'foreing_invest', 'tmp_change')

# Select ------------------------------------------------------------------
db <- dplyr::select(db, all_of(vars))
nrow(db)
nrow(drop_na(db))

# To write ----------------------------------------------------------------
dir_create('./inp')
write.csv(db, './inp/database.csv')


