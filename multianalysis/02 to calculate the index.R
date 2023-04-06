
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, glue, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Study zone --------------------------------------------------------------
col1 <- vect('G:/D/data/IGAC/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')
limt <- col1[col1$DPTO_CNMBR == 'CUNDINAMARCA',]

# Raster files ------------------------------------------------------------
rstr <- dir_ls('raster_input_v1') %>% as.character()
accs <- grep('accesibility', rstr, value = T) %>% terra::rast()
srtm <- grep('srtm', rstr, value = T) %>% terra::rast()
tavg <- grep('tavg', rstr, value = T) %>% terra::rast()

# Shapefiles files --------------------------------------------------------
shpf <- dir_ls('shape_input_v1') %>% as.character()
prot <- grep('protected', shpf, value = T) %>% terra::vect()
rvr1 <- grep('doble', shpf, value = T) %>% terra::vect()
rvr2 <- grep('sencillos', shpf, value = T) %>% terra::vect()

# Travel 60 minutes -------------------------------------------------------
accs_rclf <- accs
accs_rclf[which.lyr(accs_rclf < 90)] <- NA
plot(accs_rclf)
accs_rclf <- accs_rclf * 0 + 1

# SRTM 1000 - 2500 --------------------------------------------------------
srtm_rclf <- srtm
srtm_rclf[which.lyr(srtm_rclf < 1200)] <- NA
srtm_rclf[which.lyr(srtm_rclf > 2700)] <- NA
srtm_rclf <- srtm_rclf * 0 + 10

# Tavg 20 - 30 ------------------------------------------------------------
tavg_rclf <- mean(tavg)
tavg_rclf[which.lyr(tavg_rclf < 20)] <- NA
tavg_rclf <- tavg_rclf * 0 + 100

stck <- c(accs_rclf, srtm_rclf, tavg_rclf)
plot(stck)
suma <- sum(stck)
poly <- terra::as.polygons(suma)

plot(poly)
plot(rvr1, add = TRUE)
plot(rvr2, add = TRUE)

# Remove protected areas --------------------------------------------------
plot(suma)
plot(prot, add = T)

poly_prot <- terra::intersect(poly, prot)
plot(poly_prot, add = T, border = 'red')

# Remove these areas from the sum 
suma <- terra::mask(suma, poly_prot, inverse = TRUE)
plot(suma, col = 'green')
plot(poly_prot, add = TRUE, border = 'red')

plot(rvr1, add = TRUE, border = 'blue')
plot(rvr2, add = TRUE, border = 'blue', col = 'blue')

# To write the results ----------------------------------------------------
terra::writeRaster(x = suma, filename = 'raster_output_v1/result_v1.tif')



