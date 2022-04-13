#Julian Ramirez
#eejarv@leeds.ac.uk
#Oct 2010
#Modified Mar 2013
#Modified 3rd May 2013 for bean EAF analysis (JRV)
#Modified July 2013 to include seasonality (LPM)

#This R script computes the EcoCrop suitability index based on a set of parameters

require(rgdal)
require(raster)

makeLogFile <- function(filePathName, climPath, sowDat, harDat, cropname, Gmin, Gmax, Tkmp, Tmin, Topmin, Topmax, Tmax, Rmin, Ropmin, Ropmax, Rmax) {
  con <- file(filePathName, "w")
  writeLines(paste("CLIMATE_FILES:", climPath), con)
  writeLines(paste("SOWING_DATE:", sowDat), con)
  writeLines(paste("HARVEST_DATE:", harDat), con)
  writeLines(paste("CROP:", cropname), con)
  writeLines(paste("GMIN:", Gmin), con)
  writeLines(paste("GMAX:", Gmax), con)
  writeLines(paste("TKMP:", Tkmp), con)
  writeLines(paste("TMIN:", Tmin), con)
  writeLines(paste("TOPMIN:", Topmin), con)
  writeLines(paste("TOPMAX:", Topmax), con)
  writeLines(paste("TMAX:", Tmax), con)
  writeLines(paste("RMIN:", Rmin), con)
  writeLines(paste("ROPMIN:", Ropmin), con)
  writeLines(paste("ROPMAX:", Ropmax), con)
  writeLines(paste("RMAX:", Rmax), con)
  close(con)
}

suitCalc <- function(climPath='', sowDat='', harDat='', Gmin=90,Gmax=90,Tkmp=0,Tmin=9,Topmin=20,Topmax=34,Tmax=50,Rmin=150,Ropmin=300,Ropmax=400,Rmax=600, outfolder='', cropname='',ext=".asc",cropClimate=F) {
  minAdapt <- 0
  maxAdapt <- 1
  
  if (is.na(ext)) {ext <- ".asc"}
  
  #Checking climPath folder for consistency
  if (!file.exists(climPath)) {
    stop("The specified folder where the climate files should be does not exist, please check...")
  }
  #Checking climate files for existence
  for (i in 1:12) {
    if (!file.exists(paste(climPath, "/tmean_", i, ext, sep=""))) {
      stop("Error mean temperature for month ", i, ": file does not exist")
    } else if (!file.exists(paste(climPath, "/tmin_", i, ext, sep=""))) {
      stop("Error min temperature for month ", i, ": file does not exist")
    } 
    if (!is.na(Rmin)) {
      if (!file.exists(paste(climPath, "/prec_", i, ext, sep=""))) {
        stop("Error precipitation for month ", i, ": file does not exist")
      }
    }
  }
  cat("Input climate files verification successful \n")
  
 
  
  #Checking parameters for consistency part 2
  #commented out because of 
  #if (Gmin > 365) {
  #	stop("Gmin cannot be greater than 365")
  #}
  
  #Checking if outfolder does exist, and creating it if necessary
  if (!file.exists(outfolder)) {
    dir.create(outfolder, recursive=T)
  }
  
  #Creating the log file
  logFileName <- paste(outfolder, "/", cropname, "-parameters.model", sep="")
  createLog <- makeLogFile(logFileName, climPath, sowDat, harDat, cropname, Gmin, Gmax, Tkmp, Tmin, Topmin, Topmax, Tmax, Rmin, Ropmin, Ropmax, Rmax)
  
  #Creating the stack of the whole list of variables
  if (is.na(Rmin)) {
    climateStack <- stack(stack(paste(climPath, "/tmean_", c(1:12), ext, sep="")), 
                          stack(paste(climPath, "/tmin_", c(1:12), ext, sep="")))
  } else {
    climateStack <- stack(stack(paste(climPath, "/tmean_", c(1:12), ext, sep="")), 
                          stack(paste(climPath, "/tmin_", c(1:12), ext, sep="")), 
                          stack(paste(climPath, "/prec_", c(1:12), ext, sep="")))
  }
  
  #Calculating regression models between Rmin-Ropmin and Ropmax-Rmax
  if (is.na(Rmin)) {
    rainLeftM <- NA; rainLeftB <- NA
    rainRightM <- NA; rainRightB <- NA
  } else {
    rainLeftReg <- lsfit(x=c(Rmin,Ropmin), y=c(0,1))
    rainLeftM <- rainLeftReg$coefficients[2]
    rainLeftB <- rainLeftReg$coefficients[1]
    rainRightReg <- lsfit(x=c(Ropmax,Rmax), y=c(1,0))
    rainRightM <- rainRightReg$coefficients[2]
    rainRightB <- rainRightReg$coefficients[1]
  }
  Tkill <- Tkmp + 40
  
  Gavg <- round(mean(c(Gmin, Gmax)) / 30)

  if (is.na(Gavg)) {
    cat("Growing season taken from reported sowing and harvest dates \n")
    #Checking existence of sowing and harvest date
    if (!file.exists(sowDat)) {stop("The sowing dates file does not exist, please check...")}
    if (!file.exists(harDat)) {stop("The harvest dates file does not exist, please check...")}
    rsSow <- raster(sowDat)
    rsHar <- raster(harDat)
    #load sow and har rasters
    if (cropClimate) {climateStack <- crop(climateStack,rsSow)}
  } else {
    cat("Growing season is", Gavg, "months \n")
  }
  
  #params object
  params <- list()
  params$Gavg <- Gavg; params$Tkill <- Tkill; params$Tmin <- Tmin
  params$Topmin <- Topmin; params$Topmax <- Topmax; params$Tmax <- Tmax
  params$Rmin <- Rmin; params$Ropmin <- Ropmin; params$Ropmax <- Ropmax
  params$Rmax <- Rmax; params$rainLeftM <- rainLeftM; params$rainLeftB <- rainLeftB
  params$rainRightM <- rainRightM; params$rainRightB <- rainRightB
  
  #Final grid naming and creation
  pSuitName <- paste(outfolder, "/", cropname, "_psuitability.tif", sep="")
  tSuitName <- paste(outfolder, "/", cropname, "_tsuitability.tif", sep="")
  fSuitName <- paste(outfolder, "/", cropname, "_suitability.tif", sep="")
  cumPptName <- paste(outfolder, "/", cropname, "_gsrain.tif", sep="")
  meanTName <- paste(outfolder, "/", cropname, "_gstmean.tif", sep="")
  
  pSuitRaster <- raster(climateStack, 0) #filename(pSuitRaster) <- pSuitName
  tSuitRaster <- raster(climateStack, 0) #filename(tSuitRaster) <- tSuitName
  fSuitRaster <- raster(climateStack, 0) #filename(fSuitRaster) <- fSuitName
  cumPptRaster <- raster(climateStack, 0) #filename(fSuitRaster) <- fSuitName
  meanTRaster <- raster(climateStack, 0) #filename(fSuitRaster) <- fSuitName
  
  bs <- blockSize(climateStack, n=41, minblocks=2)
  cat("processing in: ", bs$n, " chunks \n", sep="")
  #pb <- pbCreate(bs$n, type='text', style=3)

  for (b in 1:bs$n) {
    cat(" ",round(b/bs$n*100,2),"%",sep="")
    iniCell <- 1+(bs$row[b]-1)*ncol(pSuitRaster)
    finCell <- (bs$row[b]+bs$nrow[b]-1)*ncol(pSuitRaster)
    allCells <- iniCell:finCell
    validCells <- allCells[which(!is.na(climateStack[[1]][allCells]))]
    validXY <- xyFromCell(climateStack,validCells)
   
    if (length(validCells) > 0) {
      rowVals <- raster::extract(climateStack,validCells)
      if (is.na(Gavg)) {
        rowVals <- cbind(rowVals,sow=raster::extract(rsSow,validXY))
        rowVals <- cbind(rowVals,har=raster::extract(rsHar,validXY))
        rasVals <- apply(rowVals, 1, suitFun_sowVar, params)
      } else {
        rasVals <- apply(rowVals, 1, suitFun_sowFix, params)
      }
      precVecSuit <- rasVals[1,]
      tempVecSuit <- rasVals[2,]
      finlVecSuit <- rasVals[3,]
      cumPptVec <- rasVals[4,]
      meanTVec <- rasVals[5,]
      rm(rasVals)
    } else {
      precVecSuit <- NA
      tempVecSuit <- NA
      finlVecSuit <- NA
      cumPptVec <- NA
      meanTVec <- NA
    }
    
    pSuitRaster[validCells] <- precVecSuit
    tSuitRaster[validCells] <- tempVecSuit
    fSuitRaster[validCells] <- finlVecSuit
    cumPptRaster[validCells] <- cumPptVec
    meanTRaster[validCells] <- meanTVec
    
    #pbStep(pb, b)
  }
  cat("\n")
  #pbClose(pb)
  pSuitRaster <- writeRaster(pSuitRaster, pSuitName, format='GTiff', overwrite=TRUE)
  tSuitRaster <- writeRaster(tSuitRaster, tSuitName, format='GTiff', overwrite=TRUE)
  fSuitRaster <- writeRaster(fSuitRaster, fSuitName, format='GTiff', overwrite=TRUE)
  cumPptRaster <- writeRaster(cumPptRaster, cumPptName, format='GTiff', overwrite=TRUE)
  meanTRaster <- writeRaster(meanTRaster, meanTName, format='GTiff', overwrite=TRUE)
  
  return(stack(pSuitRaster, tSuitRaster, fSuitRaster,cumPptRaster,meanTRaster))
}


#This is the function that evaluates the suitability on a pixel basis
#this is a modified version that uses a planting and harvest date by a column in the input matrix
#this version has the bugfix reported by LPM in 2012
suitFun_sowVar <- function(dataPixel,params) {
  #cat(dataPixel,"\n")
  if (length(which(is.na(dataPixel)))!=0) {
    return(c(NA,NA,NA,NA,NA))
  } else {
    #get parameters from params object
    Gavg <- params$Gavg; Tkill <- params$Tkill; Tmin <- params$Tmin
    Topmin <- params$Topmin; Topmax <- params$Topmax; Tmax <- params$Tmax
    Rmin <- params$Rmin; Ropmin <- params$Ropmin; Ropmax <- params$Ropmax
    Rmax <- params$Rmax; rainLeftM <- params$rainLeftM; rainLeftB <- params$rainLeftB
    rainRightM <- params$rainRightM; rainRightB <- params$rainRightB
    
    TavDataPixel <- dataPixel[1:12]
    TnDataPixel <- dataPixel[13:24]
    if (is.na(Rmin)) {
      PptDataPixel <- rep(NA,12)
      sowDate <- dataPixel[25]
      harDate <- dataPixel[26]
    } else {
      PptDataPixel <- dataPixel[25:36]
      sowDate <- dataPixel[37]
      harDate <- dataPixel[38]
    }
    
    if (sowDate == 0 & harDate == 0) {
      return(c(NA,NA,NA,NA,NA))
    } else {
      if (sowDate == 0) {sowDate <- 1}
      if (harDate == 0) {harDate <- 1}
      
      #determine growing season
      Gi <- ceiling(sowDate/30); if (Gi > 12) {Gi <- 12}
      Gf <- ceiling(harDate/30); if (Gf>12) {Gf <- Gf-12}
      if (Gf < Gi) {Gdur <- (Gf+12)-Gi+1} else if (Gf == Gi) {Gdur <- 1} else {Gdur <- Gf-Gi+1}
      
      #cat(sowDate," (",Gi,") and ", harDate," (",Gf,")\n",sep="")
      
      tSuit <- rep(NA, 12)
      pSuit <- rep(NA, 12)
      cumPpt <- rep(NA, 12)
      
      ##########################
      #Note: below if will reduce computational time for a perennial crop (where Gavg=12)
      if (Gdur == 12) {
        #for perennial crop
        meanT <- mean(TavDataPixel[1:12])
        for (i in 1:12) {
          #Temp. iteration
          if (TnDataPixel[i] < Tkill) {
            tSuit[i] <- 0
          } else if (TavDataPixel[i] < Tmin) {
            tSuit[i] <- 0
          } else if (TavDataPixel[i] < Topmin) {
            tSuit[i] <- 1 - ((Topmin - TavDataPixel[i]) * (1 / (Topmin - Tmin)))
          } else if (TavDataPixel[i] < Topmax) {
            tSuit[i] <- 1
          } else if (TavDataPixel[i] < Tmax) {
            tSuit[i] <- (Tmax - TavDataPixel[i]) * (1 / (Tmax - Topmax))
          } else {
            tSuit[i] <- 0
          }
        }
        
        if (!is.na(Rmin)) {
          #total annual rainfall
          cumPpt <- sum(PptDataPixel[1:12])
          
          #Single precipitation iteration
          if (cumPpt < Rmin) {
            pSuit <- 0
          } else if (cumPpt >= Rmin & cumPpt <= Ropmin) {
            pSuit <- (rainLeftM) * cumPpt + (rainLeftB)
          } else if (cumPpt > Ropmin & cumPpt < Ropmax) {
            pSuit <- 1
          } else if (cumPpt >= Ropmax &  cumPpt <= Rmax) {
            pSuit <- (rainRightM) * cumPpt + (rainRightB)
          } else if (cumPpt > Rmax) {
            pSuit <- 0
          } else {
            pSuit <- NA
          }
        } else {
          pSuit <- NA
          cumPpt <- NA
        }
        
        #Minimum cumulated temperature and rainfall suitability
        i <- 1
        start.month <- i
        end.month <- i + Gdur - 1
        
        ecotf <- rep(NA, 12)
        ecot <- rep(NA, 12)
        ecot[1] <- 1
        ecopf <- rep(NA, 12)
        finSuit <- rep(NA, 12)
        mthCounter <- 1
        for (j in start.month:end.month) {
          r.end.mth <- j
          if (r.end.mth > 12) {r.end.mth <- r.end.mth - 12}
          r.nxt.mth <- r.end.mth + 1
          
          nxtCounter <- mthCounter + 1
          if (tSuit[r.end.mth] < ecot[mthCounter]) {
            ecot[nxtCounter] <- tSuit[r.end.mth]
          } else {
            ecot[nxtCounter] <- ecot[mthCounter]
          }
          mthCounter <- mthCounter + 1
        }
        ecotf[1:12] <- min(ecot,na.rm=T)
        ecopf[1:12] <- pSuit #max(ecop,na.rm=T)
        finSuit[1:12] <- pSuit * min(ecot,na.rm=T) #PM 0702: the seasonality wasn't #JRV modified
      } else {
        #for annual crop (Gdur is less than 12)
        if (Gf < Gi) {Gf_m <- Gf+12} else {Gf_m <- Gf} #where the harvest month is in next year
        
        #iterating months in the growing season
        for (i in 1:12) {
          #start.month <- i
          #end.month <- i + Gdur #- 1
          
          #Temp. iteration
          if (TnDataPixel[i] < Tkill) {
            tSuit[i] <- 0
          } else if (TavDataPixel[i] < Tmin) {
            tSuit[i] <- 0
          } else if (TavDataPixel[i] < Topmin) {
            tSuit[i] <- 1 - ((Topmin - TavDataPixel[i]) * (1 / (Topmin - Tmin)))
          } else if (TavDataPixel[i] < Topmax) {
            tSuit[i] <- 1
          } else if (TavDataPixel[i] < Tmax) {
            tSuit[i] <- (Tmax - TavDataPixel[i]) * (1 / (Tmax - Topmax))
          } else {
            tSuit[i] <- 0
          }
        }
        
        #mean gs T
        if (Gf_m > 12) {
          meanT <- mean(TavDataPixel[c(Gi:12,1:Gf)])
        } else {
          meanT <- mean(TavDataPixel[Gi:Gf])
        }
        
        if (!is.na(Rmin)) {
          #Ppt growing season
          if (Gf_m > 12) {
            cumPpt <- sum(PptDataPixel[c(Gi:12,1:Gf)])
          } else {
            cumPpt <- sum(PptDataPixel[Gi:Gf])
          }
          
          #Precipitation iteration
          if (cumPpt < Rmin) {
            pSuit <- 0
          } else if (cumPpt >= Rmin & cumPpt <= Ropmin) {
            pSuit <- (rainLeftM) * cumPpt + (rainLeftB)
          } else if (cumPpt > Ropmin & cumPpt < Ropmax) {
            pSuit <- 1
          } else if (cumPpt >= Ropmax &  cumPpt <= Rmax) {
            pSuit <- (rainRightM) * cumPpt + (rainRightB)
          } else if (cumPpt > Rmax) {
            pSuit <- 0
          } else {
            pSuit <- NA
          }
        } else {
          pSuit <- NA
          cumPpt <- NA
        }
        
        #Minimum cumulated temperature and rainfall suitability
        ecotf <- rep(NA, 12)
        ecopf <- rep(NA, 12)
        finSuit <- rep(NA, 12)
        start.month <- Gi
        end.month <- Gf_m
        ecot <- rep(NA, Gdur)
        ecot[1] <- 1
        mthCounter <- 1
        for (j in start.month:end.month) {
          r.end.mth <- j
          if (r.end.mth > 12) {r.end.mth <- r.end.mth - 12}
          r.nxt.mth <- r.end.mth + 1
          
          nxtCounter <- mthCounter + 1
          if (tSuit[r.end.mth] < ecot[mthCounter]) {
            ecot[nxtCounter] <- tSuit[r.end.mth]
          } else {
            ecot[nxtCounter] <- ecot[mthCounter]
          }
          mthCounter <- mthCounter + 1
        }
        ecotf[1:12] <- min(ecot,na.rm=T)
        ecopf[1:12] <- pSuit #max(ecop,na.rm=T) 
      }
      precFinSuit <- round(max(ecopf * 100))
      tempFinSuit <- round(max(ecotf * 100))
      
      if (!is.na(Rmin)) {
        finSuit <- round((max(ecopf) * max(ecotf)) * 100)
      } else {
        finSuit <- tempFinSuit
      }
      res <- c(precFinSuit, tempFinSuit, finSuit, cumPpt, meanT)
      return(res)
    }
  }
}


#This is the function that evaluates the suitability on a pixel basis
#this is the original function, which takes the given fixed growing season from the input parameters
#this version has the bugfix reported by LPM in 2012
suitFun_sowFix <- function(dataPixel,params) {
  if(length(which(is.na(dataPixel)))!=0) {
    return(c(NA,NA,NA,NA,NA))
  } else {
    #get parameters from params object
    Gavg <- params$Gavg; Tkill <- params$Tkill; Tmin <- params$Tmin
    Topmin <- params$Topmin; Topmax <- params$Topmax; Tmax <- params$Tmax
    Rmin <- params$Rmin; Ropmin <- params$Ropmin; Ropmax <- params$Ropmax
    Rmax <- params$Rmax; rainLeftM <- params$rainLeftM; rainLeftB <- params$rainLeftB
    rainRightM <- params$rainRightM; rainRightB <- params$rainRightB
    
    TavDataPixel <- dataPixel[1:12]
    TnDataPixel <- dataPixel[13:24]
    PptDataPixel <- dataPixel[25:36]
    tSuit <- rep(NA, 12)
    pSuit <- rep(NA, 12)
    cumPpt <- rep(NA, 12)
    
    ##########################
    #Note: below if will reduce computational time for a perennial crop (where Gavg=12)
    if (Gavg == 12) {
      #for perennial crop
      for (i in 1:12) {
        #Temp. iteration
        if (TnDataPixel[i] < Tkill) {
          tSuit[i] <- 0
        } else if (TavDataPixel[i] < Tmin) {
          tSuit[i] <- 0
        } else if (TavDataPixel[i] < Topmin) {
          tSuit[i] <- 1 - ((Topmin - TavDataPixel[i]) * (1 / (Topmin - Tmin)))
        } else if (TavDataPixel[i] < Topmax) {
          tSuit[i] <- 1
        } else if (TavDataPixel[i] < Tmax) {
          tSuit[i] <- (Tmax - TavDataPixel[i]) * (1 / (Tmax - Topmax))
        } else {
          tSuit[i] <- 0
        }
      }
      
	if (!is.na(Rmin)) {
      #total annual rainfall
      cumPpt <- sum(PptDataPixel[1:12])
      
		#Single precipitation iteration
		 if (cumPpt < Rmin) {
			pSuit <- 0
		  } else if (cumPpt >= Rmin & cumPpt <= Ropmin) {
			pSuit <- (rainLeftM) * cumPpt + (rainLeftB)
		  } else if (cumPpt > Ropmin & cumPpt < Ropmax) {
			pSuit <- 1
		  } else if (cumPpt >= Ropmax &  cumPpt <= Rmax) {
			pSuit <- (rainRightM) * cumPpt + (rainRightB)
		  } else if (cumPpt > Rmax) {
			pSuit <- 0
		  } else {
			pSuit <- NA
		  }
		 } else {
			pSuit <- NA
			cumPpt <- NA
        }
      #Minimum cumulated temperature and rainfall suitability
      i <- 1
      start.month <- i
      end.month <- i + Gavg - 1
      
      ecotf <- rep(NA, 12)
      ecot <- rep(NA, Gavg)
      ecot[1] <- 1
      ecopf <- rep(NA, 12)
      finSuit <- rep(NA, 12)
      #ecop <- rep(NA, Gavg)
      #ecop[1] <- 0
      mthCounter <- 1
      for (j in start.month:end.month) {
        r.end.mth <- j
        if (r.end.mth > 12) {r.end.mth <- r.end.mth - 12}
        r.nxt.mth <- r.end.mth + 1
        
        nxtCounter <- mthCounter + 1
        if (tSuit[r.end.mth] < ecot[mthCounter]) {
          ecot[nxtCounter] <- tSuit[r.end.mth]
        } else {
          ecot[nxtCounter] <- ecot[mthCounter]
        }
        mthCounter <- mthCounter + 1
      }
      ecotf[1:12] <- min(ecot,na.rm=T)
      ecopf[1:12] <- pSuit #max(ecop,na.rm=T)
      finSuit[1:12] <- pSuit * min(ecot,na.rm=T) #PM 0702: the seasonality wasn't #JRV modified

    } else {
      #for annual crop (Gavg is less than 12)
      for (i in 1:12) {
        start.month <- i
        end.month <- i + Gavg - 1
        
        #Temp. iteration
        if (TnDataPixel[i] < Tkill) {
          tSuit[i] <- 0
        } else if (TavDataPixel[i] < Tmin) {
          tSuit[i] <- 0
        } else if (TavDataPixel[i] < Topmin) {
          tSuit[i] <- 1 - ((Topmin - TavDataPixel[i]) * (1 / (Topmin - Tmin)))
        } else if (TavDataPixel[i] < Topmax) {
          tSuit[i] <- 1
        } else if (TavDataPixel[i] < Tmax) {
          tSuit[i] <- (Tmax - TavDataPixel[i]) * (1 / (Tmax - Topmax))
        } else {
          tSuit[i] <- 0
        }
      if (!is.na(Rmin)) {  
        #Ppt growing season
        end.mth.p <- end.month
        if (end.mth.p > 12) {
          end.mth.p <- end.mth.p - 12
          cumPpt[i] <- sum(PptDataPixel[c(start.month:12,1:end.mth.p)])
        } else {
          cumPpt[i] <- sum(PptDataPixel[start.month:end.mth.p])
        }
        
        #Precipitation iteration
        if (cumPpt[i] < Rmin) {
          pSuit[i] <- 0
        } else if (cumPpt[i] >= Rmin & cumPpt[i] <= Ropmin) {
          pSuit[i] <- (rainLeftM) * cumPpt[i] + (rainLeftB)
        } else if (cumPpt[i] > Ropmin & cumPpt[i] < Ropmax) {
          pSuit[i] <- 1
        } else if (cumPpt[i] >= Ropmax &  cumPpt[i] <= Rmax) {
          pSuit[i] <- (rainRightM) * cumPpt[i] + (rainRightB)
        } else if (cumPpt[i] > Rmax) {
          pSuit[i] <- 0
        } else {
          pSuit[i] <- NA
        }
      } else {
          pSuit <- NA
          cumPpt <- NA
        }
	}
      
      #Minimum cumulated temperature and rainfall suitability
      ecotf <- rep(NA, 12)
      ecopf <- rep(NA, 12)
      finSuit <- rep(NA, 12)
      #looping potential growing seasons
      for (i in 1:12) {
        start.month <- i
        end.month <- i + Gavg - 1
        ecot <- rep(NA, Gavg)
        ecot[1] <- 1
        mthCounter <- 1
        #looping months within the growing season
        for (j in start.month:end.month) {
          r.end.mth <- j
          if (r.end.mth > 12) {r.end.mth <- r.end.mth - 12}
          r.nxt.mth <- r.end.mth + 1
          
          nxtCounter <- mthCounter + 1
          if (tSuit[r.end.mth] < ecot[mthCounter]) {
            ecot[nxtCounter] <- tSuit[r.end.mth]
          } else {
            ecot[nxtCounter] <- ecot[mthCounter]
          }
          mthCounter <- mthCounter + 1
        }
        ecotf[i] <- min(ecot,na.rm=T)
        ecopf[i] <- pSuit[i] #PM 0702: different for annual crops.
        finSuit[i] <- ecopf[i] * ecotf[i] #PM 0702: the seasonality wasn't
	    }
    }
   	tempFinSuit <- round(max(ecotf * 100))
	if (tempFinSuit == 0) {tempFinGS <- 0} else {tempFinGS <- max(which(ecotf == max(ecotf)))} 
	
	if (!is.na(Rmin)) {
        precFinSuit <- round(max(ecopf * 100))
		if (precFinSuit == 0) {precFinGS <- 0} else {precFinGS <- max(which(ecopf == max(ecopf)))}
		finFinSuit <- round(max(finSuit * 100)) #JRV 0703: calculate maximum suitability of all GS
    } else {
        precFinSuit <-NA
		precFinGS <-NA
		finFinSuit <- tempFinSuit
    }
	
    res <- c(precFinSuit, tempFinSuit, finFinSuit, precFinGS, tempFinGS)
    return(res)
  }
}



