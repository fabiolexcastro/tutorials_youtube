
raster_kernel <- function(mask, occDir, out_dir, kernel_method, scale){
  
  
  if(!file.exists(paste0(out_dir, "/kernel.tif")) ){
    cat("Reading mask and occurrences shapefile \n")
    mask <- raster(mask)
    
    ### Reading occurrences 
    
    occurrences <- shapefile(paste0(occDir, "/Occ.shp"))
    
    #####
    if(kernel_method==1){
      cat("Using Kernel Classical version","\n")
      
      
      ### Transforming mask to owin object 
      w <- spatstat::owin(xrange=c(raster::extent(mask)@xmin,
                                   raster::extent(mask)@xmax),
                          yrange =c(raster::extent(mask)@ymin,
                                    raster::extent(mask)@ymax),
                          mask=matrix(TRUE,dim(mask)[1],dim(mask)[2])
      )
      
      ### Transforming occurrences to ppp object 
      occurrences_ppp <- spatstat::ppp(x=occurrences@coords[,1],y=occurrences@coords[,2],window=w)
      
      ### Calculating Cross Validated Bandwidth Selection for Kernel Density usingh MSE
      cat("Cross Validated Bandwidth Selection for Kernel Density usingh MSE","\n")
      bw_dig <- spatstat::bw.diggle(occurrences_ppp)
      
      ### Calculating density
      cat("Calculating Kernel density using Cross Validated Bandwidth Selection for Kernel Density parameter ","\n")
      
      kernel <- spatstat::density.ppp(x=occurrences_ppp,sigma=bw_dig,at="pixels",verbose=F,diggle=T)
      kernel <- raster::raster(kernel);rm(w,bw_dig);gc()
      
      if(scale==T){
        #kernel <- kernel/max(kernel[],na.rm=T)
        kernel <- raster::scale(kernel,center=F,scale = T)
      } else { 
        kernel <- kernel
      }
      
    } else if(kernel_method==2){
      cat("Using Adehabitat Kernel UD version","\n")
      
      df_mask <- as.data.frame(mask, xy = TRUE )
      df_mask_sp <- SpatialPoints(df_mask[, c(1,2)])
      crs(df_mask_sp) <- crs(mask)
      spixels <- SpatialPixels(df_mask_sp)
      
      kernel <- adehabitatHR::kernelUD(occurrences, h = "LSCV", grid= spixels)
      
      
      res <- as.data.frame(kernel[[1]])
      sp::coordinates(res) <- ~ Var2 + Var1
      sp::gridded(res) <- TRUE
      crs(res) <- crs(mask)
      
      kernel<- raster::raster(res);rm(res);gc()
      kernel <- raster::mask(kernel, mask = mask)
      
      if(scale==T){
        kernel <- kernel/max(kernel[],na.rm=T)
        #kernel <- raster::scale(kernel,center=F,scale = T)
      } else {
        kernel <- kernel
      }
      
    } else if(kernel_method==3){
      
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
      
      kernel <- est.raster;rm(est);gc()
      
      if(scale==T){
        
        #kernel <- kernel/max(kernel[],na.rm=T)
        kernel <- raster::scale(kernel,center=F,scale = T)
      } else {
        kernel <- kernel
      }
      
    }
    
    
    ### Rasterizing density object
    
    crs(kernel) <- crs(mask)
    
    ### Fitting to mask parameters
    cat("Fitting Kernel density to mask","\n")
    kernel <- raster::crop(kernel, mask)
    kernel <- kernel * mask
    
    cat("Creating kernel classes raster.. \n")
    
    kernel[kernel[] == 0] <- NA
    kernel <- kernel * 10000
    qVal_1 <- raster::quantile(x = kernel[], probs = c(.9, 1), na.rm = T)
    knl_temp <- kernel
    knl_temp[which(knl_temp[] <= qVal_1[1])] <- NA
    qVal_2 <- raster::quantile(x = knl_temp, probs = c(.6, .95), na.rm = TRUE)
    kernel_class <- raster::reclassify(kernel, c(-Inf,qVal_1[1],1, qVal_1[1],qVal_2[2],2, qVal_2[2],Inf,3))
    
    ### Saving raster object
    cat("Saving raster objects","\n")
    raster::writeRaster(kernel, paste0(out_dir, "/kernel.tif"))
    raster::writeRaster(kernel_class, paste0(out_dir, "/kernel_classes.tif"), format = "GTiff")
    
    return(kernel)
    
  }else{
    cat("kernel raster is already created... /n")
    return(NULL)
  }  
  
  
  cat("     ","\n")
  cat("DONE!","\n")
  cat("     ","\n")
  
}

