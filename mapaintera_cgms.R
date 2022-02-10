# Mon Feb 22 22:16:37 2021 ------------------------------

#Working with Raster 

#---Loading packages

library(openxlsx)
library(tidyverse)
library(brazilmaps)
library(gifski)
library(geobr)
library(devtools)
library(mapproj)
library(esquisse)
library(raster)
library(rgdal)
library(rgeos)
library(gtools)
library(tmap)
library(shinyjs)
library(cptcity)
library(rasterVis)


#aggregate(variável, list(agrupador), função)
#aggregate(dados[, 4:6], list(c(mês, "ano"), mean)
#Importing data

#mapbiomas <- read.xlsx("Dados_Cobertura_MapBiomas_5.0_UF-MUN_SITE_v2.xlsx", 
                       #sheet = "LAND COVER - BIOMAS e UF")

#Importing Raster

files <- list.files("Maps1985_2019", full.names = TRUE, pattern = ".tif")%>%
          mixedsort()
layers <- lapply(files, FUN = raster)%>%
          stack()
plot(layers)

#layers <-layers[[c(1,35)]] #1985 e 2019

#Importing Shapefile

cg <- shapefile("Perimetro_Urbano.shp")
sr <- "+proj=longlat +datum=WGS84 +no_defs" #ajustando datum and extent 
cg <- spTransform(cg, sr)

#Cropping data

layers_shp <- raster::crop(layers, cg) %>%
raster::mask(cg)
plot(layers_shp)


#Condições para cores| criando vetores

vec <- c(1,8,1, #natural forest
         9,13,2, #planted forest
         14,21,3,#pastagem
         36,42,3,#pastagem
         22,30,4, #infraestrutura urbana
         31,33,5) #water

#---Matriz

mat <- matrix(vec, ncol=3, byrow = TRUE)
mat

#reclassificar

layers_rec <- raster::reclassify(layers_shp, mat, right = FALSE)
#layers_rec
cpal <- c('darkgreen','lightgreen','beige','darkred','blue',grey.colors(5,start = 1,end=0))
#levelplot(layers_rec, col.regions=cpal)

#----Mapa final
#tmap

#paleta de cores do Tmap

#tmaptools::palette_explorer()

# mapa final categorico
tm_shape(layers_rec[[35]]) +
  tm_raster(style = "cat", 
            palette = cpal,
            labels = c("Natural Forest", 
                       "Non Forest Natural Formation", 
                       "Pasture", 
                       "Urban Infrastructure", 
                       "Water"),
            title = "Land cover")+
  tm_graticules(lines = FALSE) + 
  tm_compass(position = c("right", "top"), size = 3, type = "4star") +
  tm_scale_bar(text.size = .7, position = c("left", "0")) + 
  tm_layout(title = "Mapa: Marcos Severgnini (2021) | MapBiomas",title.size = 0.6, main.title = "Land cover change Campo Grande 2019, Brazil", 
            main.title.position = "center",
            legend.position = c("left", "top"))
  

tmap::tmap_save(filename = "C:/Users/marco/Desktop/Doutorado/Mapa/Mapainterativo/map_2019.png",
                width = 20, 
                height = 20, 
                units = "cm", 
                dpi = 300)

