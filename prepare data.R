
# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, fs, tidyverse, sf, glue, rmapshaper)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
fles <- './shpf' %>% dir_ls(regexp = '.shp$') %>% as.character()
ccao <- st_read(fles[1])
coca <- st_read(fles[2])
zomc <- st_read('./gpkg/zomac_pdet.gpkg')
cflc <- st_read('./shpf/conflicto.shp')
# Simplify polygons -------------------------------------------------------

# Cacao
ccao <- dplyr::select(ccao, NOMBRE_DPT, NOM_MUNICI, prd)
ccao <- ms_simplify(ccao)
object.size(ccao)
object.size(ms_simplify(ccao))

# Coca
coca <- dplyr::select(coca, NOMBRE_DPT, value)
coca <- ms_simplify(coca)

# Zomac
zomc <- dplyr::select(zomc, NOMBRE_DPT, NOM_MUNICI, type_3)
zomc <- ms_simplify(zomc)

# Conflict 
cflc <- dplyr::select(cflc, confl_clt, dpto, mpio, geometry)
cflc <- ms_simplify(cflc)

# To write these shapefiles -----------------------------------------------
st_write(ccao, './gpkg/cacao.gpkg')
st_write(coca, './gpkg/coca.gpkg')
st_write(zomc, './gpkg/zomac_pdet.gpkg', append = T)
st_write(cflc, './gpkg/conflicto.gpkg')
