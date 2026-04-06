
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, glue)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
bsin <- vect('D:/data/spatial/igac/basins_col.gpkg')
dpto <- vect('D:/data/spatial/igac/mpios.gpkg')

# Project the shapefile 
bsin <- terra::project(bsin, crs(dpto))

# Intersection ------------------------------------------------------------
bsin.dpto <- terra::intersect(bsin, dpto)
bsin.dpto
bsin.cali <- bsin.dpto[bsin.dpto$MPIO_CNMBR == 'CALI',]
bsin.cali$NOMSZH
bsin.cali$NOMSZH <- gsub('ï¿½', 'í', bsin.cali$NOMSZH)

bsin.zone <- bsin.cali[bsin.cali$NOMSZH == 'Ríos Cali, río Lilí, río Melendez y río Canaveralejo',]
bsin.zone <- st_as_sf(bsin.zone)

# To write 
terra::writeVector(bsin.zone, './gpkg/rios_zone.gpkg', overwrite = T)
st_write(bsin.zone, './gpkg/bsin.gpkg')

# To download -------------------------------------------------------------

# Parameters --------------------------------------------------------------
root <- 'https://nex-gddp-cmip6.s3-us-west-2.amazonaws.com/NEX-GDDP-CMIP6'
ssps <- c('ssp245', 'ssp585')
vars <- c('pr', 'tasmax', 'tasmin')
gcms <- c('ACCESS-CM2', 'ACCESS-ESM1-5', 'BCC-CSM2-MR', 'CanESM5', 'CESM2-WACCM', 'CESM2', 'CMCC-CM2-SR5', 'CMCC-ESM2', 'CNRM-ESM2', 'CNRM-ESM2', 'CNRM-CM6-1', 'CNRM-ESM2-1', 'EC-Earth3-Veg-LR')

# Historic dataset downlaod ----------------------------------------------
down.hist <- function(var, gcm, ab1, ab2){
  
  # Proof 
  var <- 'tasmax'
  gcm <- gcms[1]
  ab1 <- 'r1i1p1f1'
  ab2 <- 'gn'
  
  # Start 
  cat('To process: ', var, ' ', gcm, '\n')
  urlw <- as.character(glue('{root}/{gcm}/historical/{ab1}/{var}/{var}_day_{gcm}_historical_{ab1}_{ab2}_{1989}.nc'))
  dirs <- as.character(glue('./tif/climate/cmip6_daily/historical/{gcm}/{var}/{basename(urlw)}'))
  dout <- unique(dirname(dirs))
  ifelse(!file.exists(dout), dir_create(dout), print('Directorio existe'))
  
  # To download 
  map(.x = 1:length(urlw), .f = function(i){
    
    cat('>>> To start the process:', i, '\t')
    
    # TO select the url for download
    url <- urlw[i]
    out <- dirs[i]
    
    # To download
    download.file(url = url, destfile = out, mode = 'wb')
    
    rst <- rast(out)
  
    # To extract by mask 
    rst <- rotate(rst)
    rst <- crop(rst, bsin.zone)
    
    # To write the final raster
    terra::writeRaster(x = rst, filename = glue('{dout}/bsin_{basename(out)}'), overwrite = T)
    file.remove(out); rm(rst); gc(reset = T)
    cat('Done!\n')
    
  })
  
}

# Future dataset download -------------------------------------------------

down.ftre <- function(var, gcm, ssp, ab1, ab2){
  
  # Proof 
  # var <- 'tasmax'
  # gcm <- gcms[1]
  # ssp <- 'ssp370'
  # ab1 <- 'r1i1p1f1'
  # ab2 <- 'gn'
  
  # Start 
  cat('To process: ', var, ' ', gcm, '\n')
  
  'https://nex-gddp-cmip6.s3-us-west-2.amazonaws.com/NEX-GDDP-CMIP6/ACCESS-CM2/ssp370/r1i1p1f1/tasmax/tasmax_day_ACCESS-CM2_ssp370_r1i1p1f1_gn_2015.nc' # R
  'https://nex-gddp-cmip6.s3-us-west-2.amazonaws.com/NEX-GDDP-CMIP6/ACCESS-CM2/ssp370/r1i1p1f1/tasmax/tasmax_day_ACCESS-CM2_ssp370_r1i1p1f1_gn_2015.nc' # WEB
  
  urlw <- as.character(glue('{root}/{gcm}/{ssp}/{ab1}/{var}/{var}_day_{gcm}_{ssp}_{ab1}_{ab2}_{2015:2016}.nc')) # 20monkemonke
  
  dirs <- as.character(glue('./tif/climate/cmip6_daily/future/{gcm}/{var}/{basename(urlw)}'))
  dout <- unique(dirname(dirs))
  ifelse(!file.exists(dout), dir_create(dout), print('Directorio existe'))
  
  # To download 
  map(.x = 1:length(urlw), .f = function(i){
    
    cat('>>> To start the process:', i, '\t')
    
    # To select the url for download
    url <- urlw[i]
    out <- dirs[i]
    
    # To download
    download.file(url = url, destfile = out, mode = 'wb')
    
    rst <- rast(out)
    
    # To extract by mask 
    rst <- rotate(rst)
    rst <- crop(rst, bsin.zone)
    
    # To write the final raster
    terra::writeRaster(x = rst, filename = glue('{dout}/bsin_{basename(out)}'), overwrite = T)
    file.remove(out); rm(rst); gc(reset = T)
    cat('Done!\n')
    
  })
  
}

