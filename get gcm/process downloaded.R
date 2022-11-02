

# Load libraries -------------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tiydverse, glue)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
fles <- dir_ls('./ssp585', regexp = '.nc$') %>% as.character()

# Precipitation -----------------------------------------------------------
prec <- grep('pr', fles, value = T)
prec <- purrr::map(.x = prec, .f = rast)
prec
