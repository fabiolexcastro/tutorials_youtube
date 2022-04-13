
# Load libraries ----------------------------------------------------------
library(pacman)
p_load(tidyverse, fs, glue, raster, maptools, rgdal, sp)

rm(list = ls())

# Agroclimatic
# Crop: Coffee arabica
# Source: https://gaez.fao.org/pages/ecocrop-find-plant

# Load data and setup -----------------------------------------------------
src.dir <- "./"
cropParamFile <- "../tbl/req_coffee.csv"
cropDir <- glue('../ecocrop/baseline')
dir_create(cropDir)
cDir <- glue('../raster/current')
crop <- 'coffee'

# Baseline ----------------------------------------------------------------

# Reading crop parameters from parameter file
cropPar <- read.csv(cropParamFile, header=T)
nTest <- ncol(cropPar) #Number of test into file crop parameters
testName <- names(cropPar)[nTest] #Name of the last test

#Source scripts and libraries
stop("no")
source(paste(src.dir,"EcoCrop-model.R", sep = ""))

if(!file.exists(paste(cropDir, "/", crop, "_", testName, "_suitability.asc", sep=""))) {
  
  #Run principal function
  cat(paste("Processing : ", crop, " ", testName, "\n", sep=""))
  
  eco <- suitCalc(climPath=cDir, 
                  Gmin=cropPar[1,nTest], #Minimum lenght of the growing season 
                  Gmax=cropPar[2,nTest], #Maximum lenght of the growing season
                  Tkmp=cropPar[3,nTest], #Killing temperature  
                  Tmin=cropPar[4,nTest], #Minimum temperature
                  Topmin=cropPar[5,nTest], #Minimum optimum temperature
                  Topmax=cropPar[6,nTest], #Maximum optimum temperature
                  Tmax=cropPar[7,nTest], #Maximum temperature
                  Rmin=cropPar[8,nTest], #Minimum precipitation
                  Ropmin=cropPar[9,nTest], #Minimum optimum precipitation
                  Ropmax=cropPar[10,nTest], #Maximum optimum precipitation
                  Rmax=cropPar[11,nTest], #Maximum precipitation
                  #outfolder = paste(cropDir, "/analyses/runs", sep=""),
                  outfolder = paste(cropDir, sep=""),
                  #sowDat=sowDat,
                  #harDat=harDat,
                  cropname=paste(crop, "_", testName, sep=""))
  
} else { cat(paste("Processed : ", crop, " ", testName, "\n", sep=""))}

rslt <- raster('../ecocrop/baseline/coffee_Coffea.arabica_suitability.tif')
plot(rslt)
