
# Random forest spatial interpolation
# Source: https://github.com/AleksandarSekulic/RFSI
# Jun 25th 2022

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(raster, terra, sf, fs, lubridate, tidyverse, glue, gtools, rgeos, meteo, sp, spacetime, gstat, plyr, xts, snowfall, doParallel,  CAST, ranger)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------

# Departments 
dpts <- st_read('D:/data/IGAC/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')
st_write(dpts, 'gpkg/dptos.gpkg')

# Ideam data
tble <- read_csv('G:/ideam/Copy of stations_catalog_daily.csv')
unique(tble$Variable)

# Filtering for tmean
tble <- filter(tble, Variable == 'tmean')
tble <- dplyr::select(tble, 1, 2, 3, 4, 7, 8, 10, 11, 12, 13)
tble <- mutate(tble, Lon = parse_number(LongitudeDD), Lat = parse_number(LatitudeDD))
tble <- dplyr::select(tble, -LongitudeDMS, -LatitudeDMS, -LongitudeDD, -LatitudeDD)
tble <- drop_na(tble)

# A simple plot
plot(st_geometry(dpts))
points(tble$Lon, tble$Lat, pch = 16, col = 'red')

# Frequence table 

as.data.frame(table(tble$Departament)) %>% arrange(desc(Freq))

# Filtering for Boyaca
tble <- filter(tble, Departament == 'Boyaca')
byca <- filter(dpts, DPTO_CNMBR == 'BOYACÁ')

write.csv(tble, 'tbl/stations_boyaca.csv', row.names = F)

st_write(byca, 'gpkg/boyaca.gpkg')

plot(st_geometry(byca))
points(tble$Lon, tble$Lat, pch = 16, col = 'red')

stts <- pull(tble, Code)

# List the climate tables -------------------------------------------------
fles <- dir_ls('G:/ideam/tmean-per-station', regexp = '.txt')
fles <- grep(paste0(stts, collapse = '|'), fles, value = T)
fles <- as.character(fles)

length(stts)
length(fles)

# To read the tables ------------------------------------------------------
tbls <- map(1:length(fles), function(i){read.table(fles[i], header = T)})
tbls <- map(tbls, as_tibble)
tbls <- map(1:length(tbls), function(i) mutate(tbls[[i]], code = stts[i]))
tbls <- bind_rows(tbls)
tbls <- bind_rows(tbls)

# To add the year - month - day
tbls <- mutate(tbls, year = str_sub(Date, 1, 4), month = str_sub(Date, 5, 6), day = str_sub(Date, 7, 8))
tbls <- mutate(tbls, date = paste0(year, '-', month, '-', day))
tbls <- mutate(tbls, date = as.Date(date, format = '%Y-%m-%d'))
tbls <- select(tbls, code, date, value = Value)
unique(tbls$Date)
min(tbls$date)
max(tbls$date)

# Get the sequence of each table ------------------------------------------
tbls
freq <- map(1:length(stts), function(i){
  
  tbl <- tbls %>% filter(code == stts[i])
  min <- min(tbl$date)
  max <- max(tbl$date)
  rsl <- as.data.frame(table(is.na(tbl$value)))
  rsl <- mutate(rsl, station = stts[i])
  rsl <- mutate(rsl, min_max = c(min, max))
  
})

freq <- bind_rows(freq)

tbls <- mutate(tbls, year = year(date))
tbls <- filter(tbls, year == 2000)

smmr <- tbls %>% 
  group_by(code, year) %>% 
  dplyr::summarise(value = mean(value, na.rm = T)) %>% 
  ungroup()

write.csv(smmr, 'tbl/mean_boyaca_2020.csv', row.names = FALSE)

