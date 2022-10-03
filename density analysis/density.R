

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rmapshaper, climateStability, ade4, elevatr, geodata, KernSmooth, spatstat, crayon, terra, sf, qs, classInt, tidyverse, ggspatial, hrbrthemes, RColorBrewer, geodata, fs, glue)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
bsin <- st_read('D:/data/spatial/igac/basins_col.gpkg')
bsin <- st_transform(bsin, crs = st_crs(4326))
cttm <- st_read('./gpgk/catatumbo.gpkg')

loss <- read_csv('tble/loss_2021.csv')

mask <- get_elev_raster(as(cttm, 'Spatial'), z = 8)
mask <- raster::crop(mask, as(cttm, 'Spatial')) %>% raster::mask(., as(cttm, 'Spatial'))
srtm <- mask
mask <- mask * 0 + 1

# To prepare --------------------------------------------------------------
pnts <- st_as_sf(loss, coords = c('x', 'y'), crs = st_crs(4326))
pnts <- as(pnts, 'Spatial')

w <- owin(xrange=c(raster::extent(mask)@xmin,
                   raster::extent(mask)@xmax),
          yrange=c(raster::extent(mask)@ymin,
                   raster::extent(mask)@ymax),
          mask=matrix(TRUE,dim(mask)[1],dim(mask)[2]))

# Density function --------------------------------------------------------
pppf <- ppp(x = pnts@coords[,1], y = pnts@coords[,2], window = w)

# Way 01 ------------------------------------------------------------------

# Calculating Cross Validated Bandwidth Selection for Kernel Density usingh MSE
bw_dig <- bw.diggle(pppf)

# Kernel
kernel <- density.ppp(x = pppf, sigma = bw_dig, at = "pixels", verbose = F, diggle = T)
kernel <- raster::raster(kernel)

# Scaling
kernel <- rescale0to1(kernel)
kern01 <- raster::crop(kernel, as(cttm, 'Spatial')) %>% raster::mask(., as(cttm, 'Spatial'))

# Way 02 ------------------------------------------------------------------

# Adehabitat
est <- KernSmooth::bkde2D(pnts@coords, 
                          bandwidth=c(dpik(pnts@coords[,1]),dpik(pnts@coords[,2])), 
                          gridsize=c(ncol(mask),nrow(mask)),
                          range.x=list(c(raster::extent(mask)@xmin,
                                         raster::extent(mask)@xmax),
                                       c(raster::extent(mask)@ymin,
                                         raster::extent(mask)@ymax)))
est$fhat[est$fhat<0.00001] <- 0 ## ignore very small values

est.raster <- raster(list(x=est$x1,y=est$x2,z=est$fhat))
xmin(est.raster) <- raster::extent(mask)@xmin
xmax(est.raster) <- raster::extent(mask)@xmax
ymin(est.raster) <- raster::extent(mask)@ymin
ymax(est.raster) <- raster::extent(mask)@ymax

kernel <- est.raster;rm(est);gc()
kernel <- rescale0to1(kernel)
kern02 <- raster::mask(kernel, as(cttm, 'Spatial'))

# To write the results ----------------------------------------------------

writeRaster(kern01, './rstr/kernel01.tif')
writeRaster(kern02, './rstr/kernel02.tif')
