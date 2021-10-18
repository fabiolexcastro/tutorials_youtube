
# Load libraries  ---------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, glue, sf, tidyverse, fs)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Tidy the URL ------------------------------------------------------------
base <- 'https://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/2.5m/wc2.1_2.5m'
vars <- c('prec', 'tmax', 'tmin')
mdls <- c('BCC-CSM2-MR', 'CNRM-CM6-1', 'CNRM-ESM2-1', 'CanESM5', 'GFDL-ESM4', 'IPSL-CM6A-LR', 'MIROC-ES2L', 'MIROC6', 'MRI-ESM2-0')
prds <- c('2021-2040', '2041-2060', '2061-2080', '2081-2100')
ssps <- c('ssp126', 'ssp245', 'ssp370', 'ssp585')
colm <- raster::getData(name = 'GADM', country = 'COL', level = 0)

# Function to use ---------------------------------------------------------
get_raster <- function(var, ssp, prd, mdl){
  
  # var <- vars[1]
  # mdl <- mdls[1]
  # ssp <- ssps[3]
  # prd <- prds[1]

  cat('Start ', var, ' ', mdl, ' ', ssp, ' ', prd)
  link <- glue('{base}_{var}_{mdl}_{ssp}_{prd}.zip')
  name <- basename(link)
  dout <- glue('./data/raster/{ssp}/{prd}')
  ifelse(!dir_exists(dout), dir_create(dout), print('Folder exists'))
  
  cat('To download\n')
  download.file(url = link, 
                destfile = glue('{dout}/{name}'), 
                mode = 'wb')
  
  cat('To unzip the file\n')
  unzip(glue('{dout}/{name}'), exdir = dout)
  cat('Done\n')
  
}

# Apply the function ------------------------------------------------------


for(i in 1:length(mdls)){
  
  cat(mdls[i], '\n')
  
  for(j in 1:length(vars)){
  
    cat(vars[i], '\n')
    get_raster(var = vars[j], ssp = ssps[3], prd = '2021-2040', mdl = mdls[i])
    cat('Done\n')
    
  }
  
}




