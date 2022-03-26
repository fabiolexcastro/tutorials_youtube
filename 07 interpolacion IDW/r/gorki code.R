library(raster)
library(ggplot2)
library(sf)
library(grid)
library(png)
library(cowplot)
library(openxlsx)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
world        <- ne_countries(scale= "small", returnclass = "sf") # Continentes del mundo
world.SA     <- subset(world, continent=="South America")     # Sur America
Sur_America     <- st_read ("SHP/SurAmerica.shp")  
SurAmerica_utm  <- st_transform(Sur_America ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Per            <- getData('GADM', country='Peru', level=1) %>%st_as_sf()  
Peru            <- getData('GADM', country='Peru', level=3) %>%st_as_sf()  
MDD             <- subset(Peru , NAME_1 == "Madre de Dios")
Marco_MDD = st_as_sfc(st_bbox(MDD))
img <- readPNG("PNG/CastaÃ±a1.png", FALSE)
g <- rasterGrob(img, x = unit(0.1, "npc"),y = unit(0.7, "npc"), width = unit(0.2, "npc"))
library(rgbif)
Berth_Exc    <- occ_search(scientificName="bertholletia excelsa")
CastaÃ±a      <- Berth_Exc$data
CastaÃ±a$image <- "PNG/CastaÃ±a1.png"
#------------------------------------------------------------------------
library(ggpubr)
Data     <- read.xlsx("Excel/Embrete.xlsx", sheet="Hoja2") 
Data[1,1] <- "MAPA DE DISTRIBUCION \nDE ESPECIE, \nCastaÃ±a"
Data[2,1] <- "Elaboradpor: \nGorky Florez Castillo"
Data[3,1] <- "Escala: Indicadas"
Data[4,1] <- "Sistemas de Coordenadas UTM \nZona 19S \nDatum:WGS84"
colnames(Data ) <- c("Mapa elaborado \nen  RStudio")
Tabla.p <- ggtexttable(Data, rows = NULL,theme =ttheme( base_size =6, "lBlackWhite"))
#-----------------------------------------------------------------------
img1 <- readPNG("PNG/Membrete3.png", FALSE)
g1   <- rasterGrob(img1, x = unit(0.82, "npc"),y = unit(0.115, "npc"), width = unit(0.35, "npc"))
img2 <- readPNG("PNG/Cap.png", FALSE)
g2   <- rasterGrob(img2, x = unit(0.16, "npc"),y = unit(0.115, "npc"), width = unit(0.1, "npc"))
img3 <- readPNG("PNG/Library.png", FALSE)
g3   <- rasterGrob(img3, x = unit(0.43, "npc"),y = unit(0.115, "npc"), width = unit(0.4, "npc"))

Map=ggplot()+ 
  geom_sf(data= SurAmerica_utm, fill="gray90", color="white")+
  geom_sf(data=Per , fill="gray81", color="white")+
  geom_sf(data=MDD, fill="gray81", color="white")+
  geom_sf_text(data = st_as_sf(Per), aes(label =  NAME_1), size = 4, color="blue",family="serif") +
  geom_image(data = CastaÃ±a, aes( x=decimalLongitude, y = decimalLatitude, image = image), size = 0.04)+
  geom_sf_text(data = st_as_sf(MDD), aes(label =  NAME_3), size = 2.5,family="serif") +
  annotate(geom = "text", x = -69, y = -10.5, label = "Brasil", family="serif", color = "grey22", size = 5)+
  annotate(geom = "text", x = -68.7, y = -12, label = "Bolivia", family="serif", color = "grey22", size = 5)+
  annotate(geom = "text", x = -70, y = -13.5, label = "Puno", family="serif", color = "blue", size = 4)+
  annotate(geom = "text", x = -72.8, y = -11.9, label = "CASTAÃA \nBertholletia Excelsa ", family="serif", color = "lightblue4", size = 4)+
  coord_sf(xlim = c(-73,-68.65311), ylim = c(-13.36179,-9.879849))+
  annotation_custom(g)+
  theme_bw()+
  geom_vline(xintercept = c(-73,-72,-71,-70,-69), color = "gray50",linetype = "dashed", size = 0.05)+ 
  geom_hline(yintercept = c(-13.5,-13,-12.5,-12,-11.5,-11,-10.5,-10), color = "gray50",linetype = "dashed", size = 0.05)+
  scale_x_continuous(breaks = seq(-73,-69, by = 1))+
  scale_y_continuous(breaks = seq(-13.5 ,-10, by = 0.5))+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme(axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8),
        axis.text.x  = element_text(face="bold", color="black", size=8),
        panel.border = element_rect(size = 2))+
  labs(x = NULL, y = NULL)+
  annotate(geom = "text", x = -70, y = -10, label = "MAPA de DISTRIBUCIÃN \nde", 
           family="serif", color = "indianred4", size = 3, fontface = "bold")+
  annotate(geom = "text", x = -70, y = -10.1, label = "BERTHOLLETIA EXCELSA en ", 
           fontface = "italic", family="serif",color = "indianred4", size =4, fontface = "bold")+
  annotate(geom = "text", x = -70, y = -10.2, label = "MADRE DE DIOS", 
           family="serif", color = "indianred4", size = 3, fontface = "bold")+
  annotate(geom = "text", x = -72.8, y = -13.3, label = "Ing.Gorky \nFlorez Castillo", 
           family="serif", color = "orangered4", size = 3)

SA=ggplot()+
  geom_sf(data = SurAmerica_utm, fill="gray", color="white")+
  geom_sf_text(data = st_as_sf(world.SA ), aes(label =  name_sort), size = 2.5,family="serif") +
  theme(panel.background = element_rect(fill = "lightskyblue"),
        panel.grid.major = element_line(color = "white",linetype = "dashed", size = 0.5),
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8),
        axis.text.x  = element_text(face="bold", color="black", size=8))+
  coord_sf(xlim = c(-89.99999,-34.82272), ylim = c(-55.9795,15))+
  labs(x = NULL, y = NULL)+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")

Naci=ggplot()+
  geom_sf(data = Per, fill="gray81", color="white")+
  geom_sf(data= MDD, fill="gray", color="white")+
  geom_sf_text(data = st_as_sf(Per), aes(label = NAME_1), size = 2.5,family="serif") +
  theme(panel.background = element_rect(fill = "lightskyblue"),
        panel.grid.major = element_line(color = "white",linetype = "dashed", size = 0.5),
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8),
        axis.text.x  = element_text(face="bold", color="black", size=8))+
  coord_sf(xlim = c(-80,-68.65311), ylim = c(-15,-5))+
  labs(x = NULL, y = NULL)+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")

MAPP=ggdraw() +
  coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE) +
  draw_plot(Map, width = 20, height = 20,x = 0.05, y = 2.5)+
  draw_plot(SA, width = 10, height = 10,x = 20, y = 10.3)+
  draw_plot(Naci, width = 6, height = 6,x = 21.3, y = 4.5)+
  annotation_custom(g1)+
  annotation_custom(g2)+
  annotation_custom(g3)+
  theme(panel.background = element_rect(fill = "white"))+
  annotation_custom(ggplotGrob(Tabla.p ),
                    xmin = 1.1, xmax = 2,ymin = 1.8,ymax = 3)

ggsave(plot = MAPP ,"MAPAS/Mapa de CastaÃ±a2.png",
       units = "cm", width = 29,height = 21, dpi = 900)# guardar grafico