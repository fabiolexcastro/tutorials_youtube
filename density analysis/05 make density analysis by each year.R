
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rmapshaper,ade4, KernSmooth, spatstat, crayon, terra, sf, qs, classInt, tidyverse, ggspatial, hrbrthemes, RColorBrewer, geodata, fs, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Function ----------------------------------------------------------------
source('https://raw.githubusercontent.com/CIAT-DAPA/gap_analysis_landraces/master/03_gap_methods/kernel_function.R')

# Load data ---------------------------------------------------------------
fles <- dir_ls('../qs')

test <- qs::qread(fles[1])
unique(test$year)

cntn <- c('Africa', 'Asia', 'latin', 'Oceania')
znes <- terra::vect('../gpkg/countries_104.gpkg')
mask_raw <- terra::rast('../tif/mask/wrl/mask.tif')

years <- 2004:2020

map(.x = 2:length(years), .f = function(k){make_density(cnt = cntn[1], yea = years[k])})
map(.x = 1:length(years), .f = function(k){make_density(cnt = cntn[2], yea = years[k])})
map(.x = 1:length(years), .f = function(k){make_density(cnt = cntn[3], yea = years[k])})


make_density <- function(cnt, yea){
  
  # Proof
  # cnt <- cntn[3]
  # yea <- 2004
  
  cat(yea, '\n')
  
  # Filtering
  fle <- grep(cnt, fles, value = T) %>% as.character()
  tbl <- qs::qread(fle)
  # zne <- znes[znes$CONTINENT == str_to_sentence(cnt),]
  zne <- znes[znes$CONTINENT %in% c('South America', 'North America')]
  
  
  # To make the density for each country
  cvs <- map(1:nrow(zne), function(i){
    
    try(expr = {
      
      zn <- zne[i,] %>% st_as_sf()
      is <- zn$ISO3
      tb <- filter(tbl, iso3 == is)
      tb <- filter(tb, year == yea)
      ms <- terra::crop(mask_raw, vect(zn)) %>% terra::mask(., vect(zn))
      mask <- raster(ms)
      occurrences <- st_as_sf(tb, coords = c('x', 'y'), crs = st_crs(4326)) %>% as(., 'Spatial')
      
      w <- owin(xrange=c(raster::extent(mask)@xmin,
                         raster::extent(mask)@xmax),
                yrange=c(raster::extent(mask)@ymin,
                         raster::extent(mask)@ymax),
                mask=matrix(TRUE,dim(mask)[1],dim(mask)[2]))
      
      ### Transforming occurrences to ppp object 
      
      
      ### Calculating Cross Validated Bandwidth Selection for Kernel Density usingh MSE
      cat("Cross Validated Bandwidth Selection for Kernel Density usingh MSE","\n")
      bw_dig <- bw.diggle(occurrences_ppp)
      
      ### Calculating density
      cat("Calculating Kernel density using Cross Validated Bandwidth Selection for Kernel Density parameter ","\n")
      kernel <- density.ppp(x=occurrences_ppp,sigma=bw_dig,at="pixels",verbose=F,diggle=T)
      kernel <- raster::raster(kernel);rm(w,bw_dig);gc()
      
      scale <- T
      if(scale==T){
        #kernel <- kernel/max(kernel[],na.rm=T)
        kernel <- raster::scale(kernel,center=F,scale = T)
      } else { 
        kernel <- kernel
      }
      
      # Adehabitat
      est <- KernSmooth::bkde2D(occurrences@coords, 
                                bandwidth=c(dpik(occurrences@coords[,1]),dpik(occurrences@coords[,2])), 
                                gridsize=c(ncol(mask),nrow(mask)),
                                range.x=list(c(raster::extent(mask)@xmin,
                                               raster::extent(mask)@xmax),
                                             c(raster::extent(mask)@ymin,
                                               raster::extent(mask)@ymax)))
      est$fhat[est$fhat<0.00001] <- 0 ## ignore very small values
      
      est.raster <-  raster(list(x=est$x1,y=est$x2,z=est$fhat))
      #projection(est.raster) <- CRS("+init=epsg:4326")
      xmin(est.raster) <- raster::extent(mask)@xmin
      xmax(est.raster) <- raster::extent(mask)@xmax
      ymin(est.raster) <- raster::extent(mask)@ymin
      ymax(est.raster) <- raster::extent(mask)@ymax
      
    s
      
      if(scale==T){
        
        #kernel <- kernel/max(kernel[],na.rm=T)
        kernel <- raster::scale(kernel,center=F,scale = T)
      } else {
        kernel <- kernel
      }
      
      kernel <- raster::mask(kernel, as(zn, 'Spatial'))
      cat('Finish!\n')
      return(kernel)
      
    })
    
  })
  
  bse <- cvs
  
  # To make the mosaic
  cvs$fun <- mean
  cvs$na.rm  <- TRUE
  
  # Mosaicking
  msc <- do.call(mosaic, cvs)
  
  # To write these rasters
  raster::writeRaster(x = msc, 
                      filename = glue('../tif/density_years/y{yea}_{cnt}.tif'), overwrite = TRUE)
  
  cat('Done!\n')

}

# To list the results -----------------------------------------------------
fles <- dir_ls('../tif/density_years')
fles <- grep('2012', fles, value = T)
fles <- as.character(fles)
