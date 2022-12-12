
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, tidyverse, glue, sf, RColorBrewer, ggspatial, hrbrthemes, showtext, rnaturalearthdata, rnaturalearth, extrafont, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Font --------------------------------------------------------------------
font_add_google(family = 'Fira Sans', name = 'Fira Sans Condensed')
showtext_auto()

# Download data -----------------------------------------------------------
nic0 <- geodata::gadm(country = 'NIC', level = 0, path = 'tmpr')
plot(nic0)
vars <- c('prec', 'tmax', 'tmin')

# Download 30s ------------------------------------------------------------
prec <- geodata::cmip6_tile(lon = -85, lat = 12, model = 'ACCESS-CM2', ssp = '585', time = '2021-2040', var = 'prec', path = 'tmpr')
tmax <- geodata::cmip6_tile(lon = -85, lat = 12, model = 'ACCESS-CM2', ssp = '585', time = '2021-2040', var = 'tmax', path = 'tmpr')
tmin <- geodata::cmip6_tile(lon = -85, lat = 12, model = 'ACCESS-CM2', ssp = '585', time = '2021-2040', var = 'tmin', path = 'tmpr')

# Extract by mask ---------------------------------------------------------
prec <- terra::crop(prec, nic0) %>% terra::mask(., nic0)
tmax <- terra::crop(tmax, nic0) %>% terra::mask(., nic0)
tmin <- terra::crop(tmin, nic0) %>% terra::mask(., nic0)

# To write ----------------------------------------------------------------
purrr::map(.x = 1:12, .f = function(i){
  cat(month.abb[i], '\n')
  dout <- './tif/c6/rcp585/ACCESS-CM2'
  terra::writeRaster(x = prec[[i]], filename = glue('{dout}/prec_{i}.asc'))
  terra::writeRaster(x = tmax[[i]] * 10, filename = glue('{dout}/tmax_{i}.asc'))
  terra::writeRaster(x = tmin[[i]] * 10, filename = glue('{dout}/tmin_{i}.asc'))
})

# To add the tmean --------------------------------------------------------
tavg <- (tmax + tmin) / 2
tavg <- tavg * 10

for(i in 1:12){terra::writeRaster(x = tavg[[i]], filename = glue('tif/c6/rcp585/ACCESS-CM2/tmean_{i}.asc'))}

# To make the model -------------------------------------------------------
source('https://raw.githubusercontent.com/fabiolexcastro/tutorials_youtube/master/08%20Ecocrop/r/EcoCrop-model.R')

cropParamFile <- "https://raw.githubusercontent.com/fabiolexcastro/tutorials_youtube/master/08%20Ecocrop/tbl/req_coffee.csv"
cropDir <- glue('ecocrop/cm6/ACCESS-CM2')
dir_create(cropDir)
cDir <- glue('tif/c6/rcp585/ACCESS-CM2')
crop <- 'coffee' 

# Reading crop parameters from parameter file
cropPar <- read.csv(cropParamFile, header=T)
nTest <- ncol(cropPar) #Number of test into file crop parameters
testName <- names(cropPar)[nTest] #Name of the last test

# To run ------------------------------------------------------------------
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

# To read the results -----------------------------------------------------
rstr <- rast('ecocrop/cm6/ACCESS-CM2/coffee_Coffea.arabica_suitability.tif')
names(rstr) <- 'value'
tble <- terra::as.data.frame(rstr, xy = TRUE) %>% as_tibble()

# Shapefiles
wrld <- ne_countries(returnclass = 'sf', scale = 50)
nica <- geodata::gadm(country = 'NIC', level = 1, path = 'tmpr')
nica <- st_as_sf(nica)

gmap <- ggplot() + 
  geom_tile(data = tble, aes(x = x, y = y, fill = value)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'RdYlGn'),
                       limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100)) +
  labs(x = 'Lon', y = 'Lat', fill = 'Idoneidad') + 
  geom_sf(data = wrld, fill = NA, col = 'grey70') +
  geom_sf(data = nica, fill = NA, col = 'white') +
  coord_sf(xlim = extent(nica)[1:2], ylim = extent(nica)[3:4]) +
  ggtitle(label = 'Idoneidad para el cultivo de café - 2030s (SSP 585) GCM: ACCESS-CM2', 
          subtitle = 'A partir de requerimientos agroclimáticos') +
  theme_ft_rc(base_family = 'Fira Sans') +
  theme(axis.text.x = element_text(size = 41, family = 'Fira Sans'), 
        axis.text.y = element_text(size = 41, family = 'Fira Sans', angle = 90, hjust = 0.5),
        axis.title.x = element_text(size = 60, family = 'Fira Sans'), 
        axis.title.y = element_text(size = 60, family = 'Fira Sans'),
        plot.title = element_text(size = 70, face = 'bold', hjust = 0.5, family = 'Fira Sans'),
        plot.subtitle = element_text(size = 70, face = 'bold', hjust = 0.5, family = 'Fira Sans'),
        legend.title = element_text(size = 60, family = 'Fira Sans', face = 'bold'),
        legend.position = 'bottom',
        legend.key.width = unit(4.8, 'line'),
        legend.key.height = unit(0.8, 'line'),
        legend.text = element_text(size = 60, family = 'Fira Sans'),
        # panel.background = element_rect(fill = "white"),
        plot.caption = element_text(size = 60, face = 'bold', family = 'Fira Sans'),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.1)) +
  annotation_scale(location =  "br", width_hint = 0.5, text_cex = 2.5, text_family = 'Fira Sans', text_col = 'white') +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'Fira Sans', text_col = 'white', text_size = 25)) +
  annotate(geom = "text", x = -92, y = 13.7, hjust = 0, vjust = 1, 
           label = 'Elaboró: Fabio Castro / YouTube: Un Geógrafo en YouTube',
           size = 7, family = "Roboto", color = "black")

ggsave(plot = gmap, filename = 'png/map_ecocrop_nicaragua_future_cafe.png', units = 'in', width = 13, height = 11, dpi = 300)  
