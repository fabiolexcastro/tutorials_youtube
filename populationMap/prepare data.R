

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, tidyverse, rmapshaper, readxl, leaflet, leaflet.extras, gtools, fs, glue, classInt)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999, warn = -1)

# Source: https://www.dane.gov.co/index.php/estadisticas-por-tema/demografia-y-poblacion/proyecciones-de-poblacion

# Load data ---------------------------------------------------------------
mpios <- st_read('D:/data/spatial/igac/mpios.gpkg')
table <- read_excel('./tble/anexo-area-proyecciones-poblacion-Municipal_Area_1985-2017.xlsx')

colnames(table)[6] <- 'area_geografica'

table <- spread(table, area_geografica, Total)
table <- table[,1:8]
colnames(table) <- c('dp', 'dpnom', 'cod_mpio', 'mpio', 'year', 'cabecera_municipal', 'rural', 'total')
table <- pivot_wider(data = table, names_from = year, names_sep = '.', values_from = c(cabecera_municipal, rural, total))

# Join  -------------------------------------------------------------------

rslt <- inner_join(mpios, table, by = c('MPIO_CCNCT' = 'cod_mpio'))
rslt <- dplyr::select(rslt, DPTO_CCDGO, MPIO_CCDGO, MPIO_CNMBR, DPTO_CNMBR, rural.2017, cabecera_municipal.2017, total.2017, geom)
colnames(rslt) <- gsub('\\.', '_', colnames(rslt))

plot(dplyr::select(rslt, rural_2017))

# To write
st_write(rslt, './gpkg/population.gpkg')
