
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
dpto <- terra::vect('../shp/MGN_DPTO_POLITICO.shp')
dpto$DPTO_CNMBR  <- iconv(dpto$DPTO_CNMBR , from = 'UTF-8', to = 'latin1')

hila <- dpto[dpto$DPTO_CNMBR == 'HUILA']

# A simple plot
plot(dpto)
plot(hila, add = TRUE, col = 'red')

# Con que departamentos limita Huila --------------------------------------
ps_1 <- relate(hila, dpto, 'touches')
ps_1 <- as.vector(ps_1)
rl_1 <- dpto[ps_1,]
plot(rl_1, add = TRUE, col = 'green')

rl_1$DPTO_CNMBR

# Municipios --------------------------------------------------------------
mpio <- terra::vect('../shp/MGN_MPIO_POLITICO.shp')
mpio$MPIO_CNMBR <- iconv(mpio$MPIO_CNMBR, from = 'UTF-8', to = 'latin1')
mpio$DPTO_CNMBR <- iconv(mpio$DPTO_CNMBR, from = 'UTF-8', to = 'latin1')

area <- terra::vect('../shp/Parques_Nacionales_Naturales_de_Colombia.shp')

rslt <- map(.x = 1:nrow(area), .f = function(i){
  
  cat(i, '\n')
  are <- area[i, ]
  pos <- relate(mpio, are, 'intersects')[,1]
  mps <- mpio[pos,]
  mps <- mps$MPIO_CNMBR
  cat('Done\n')
  return(mps)
  
})

rslt <- unlist(rslt) %>% unique()
length(rslt)

plot(mpio, border = 'grey')
plot(mpio[mpio$MPIO_CNMBR%in% rslt,], add = T, col = 'red', border = 'red')

# Vias y municipios del Valle del Cauca -----------------------------------
road <- terra::vect('../shp/road_tipo_1.shp')
road <- project(road, mpio)
crs(road)
crs(mpio)

svll <- mpio[mpio$MPIO_CNMBR == 'SEVILLA',]

plot(svll)
plot(road, add = TRUE)

relate(road, svll, relation="crosses")
relate(svll, road, relation="contains")
