
# Load libraries ----------------------------------------------------------
require(ncdf4); require(raster); library(tidyverse)

g <- gc(reset = T); rm(list = ls()) 
.rs.restartR()                    
options(warn = -1, scipen = 999, timeout = 3600)  

# Source ------------------------------------------------------------------
source("https://raw.githubusercontent.com/CIAT-DAPA/WFP-profiles/main/archive/999_1b_search_CMIP6_functions.R")

# Vars --------------------------------------------------------------------
vars <- c("pr","tasmax","tasmin")
models <- c('ACCESS-ESM1-5', 'ACCESS-CM2', 'INM-CM5-0', 'NESM3')
models <-  'MPI-ESM1-2-HR'
ssp <- "ssp585" # "ssp245","ssp126","ssp370"

varmod <- expand.grid(vars, models)
names(varmod) <- c("vars", "models")

# Historical --------------------------------------------------------------
dh <- list()

for (i in 1:nrow(varmod)){
  var <- varmod$vars[i]
  model <- varmod$models[i]
  dh[[i]] <- try(getMetaCMIP6(offset = 0,
                              limit = 10000,
                              activity_id="CMIP",
                              experiment_id = "historical",
                              frequency = "day",
                              member_id = "r1i1p1f1",
                              variable_id = var,
                              source_id = model,
                              mip_era = "CMIP6"))
}


# Remove any unsuccessful attempts
dhc <- lapply(dh, function(x){if(inherits(x, "data.table")){return(x)}else{NULL}})
dhist <- data.table::rbindlist(dhc, fill = TRUE)
dhist


# Future ------------------------------------------------------------------
df <- list()

for (i in 1:nrow(varmod)){
  var <- varmod$vars[i]
  model <- varmod$models[i]
  df[[i]] <- try(getMetaCMIP6(offset = 0,
                              limit = 10000,
                              activity_id="ScenarioMIP",
                              experiment_id = ssp,
                              member_id = "r1i1p1f1",
                              frequency = "day",
                              variable_id = var,
                              source_id = model,
                              mip_era = "CMIP6"))
}

# remove any unsuccessful attempts
dfc <- lapply(df, function(x){if(inherits(x, "data.table")){return(x)}else{NULL}})
dfut <- data.table::rbindlist(dfc, fill = TRUE)

# Combine both (baseline and future) --------------------------------------
dd <- rbind(dhist, dfut)
dir.create(paste0('./', ssp))
data.table::fwrite(dd, paste0("./", ssp, "/", ssp,"_cmip6_filter_index_", Sys.Date(), ".csv"), row.names = FALSE)

# Now download ------------------------------------------------------------
options(timeout=3600)

getDataCMIP6 <- function(i, idx, downdir, silent = FALSE){
  
  d <- idx[i,]
  
  # print something
  if (silent) print(d$file_url); flush.console()
  
  # specify where to save
  # fstr <- strsplit(d$file_url, "/CMIP6/|/cmip6/")[[1]][2]
  flocal <- file.path(downdir, basename(d$file_url))
  d$localfile <- flocal
  
  # where to save
  dir.create(dirname(flocal), FALSE, TRUE)
  
  # should we download?
  if (!file.exists(flocal)){
    # try downloading
    try(download.file(d$file_url, flocal, mode = "wb", quiet = silent))
  }
  
  file_damage <- FALSE
  
  tryCatch({ result <- raster::stack(flocal) }, error = function(e) {file_damage <<- TRUE})
  # expand for other kind type of files
  
  if(file_damage){
    cat("deleting", basename(flocal), "\n")
    unlink(flocal)
    flush.console()
  }else{
    cat("keeping", basename(flocal), "\n") 
  }
  
  return(NULL)
  
}

# Data directory ----------------------------------------------------------
datadir <- paste0("./", ssp)
downdir <- paste0("./", ssp)

f <- list.files(datadir, ".csv$", full.names = TRUE)
j <- which.max(file.info(f)$ctime)
idx <- read.csv(f[j], stringsAsFactors = FALSE)
idx <- idx[idx$file_start_date < "2060-12-31" & idx$file_end_date > "1980-01-01",]

downloadParallel <- F

if(downloadParallel){
  library(future.apply)
  plan(multiprocess, workers = 4)
  future_lapply(1:nrow(idx), getDataCMIP6, idx, downdir, silent=FALSE, future.seed = TRUE)
} else {
  # download files
  lapply(1:nrow(idx), getDataCMIP6, idx, downdir, silent=FALSE)
}
