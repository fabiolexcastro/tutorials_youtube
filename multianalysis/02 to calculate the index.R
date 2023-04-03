



# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, glue, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Study zone --------------------------------------------------------------
col1 <- vect('D:/data/spatial/igac/dptos.gpkg')
limt <- col1[col1$DPTO_CNMBR == 'CUNDINAMARCA',]


