##preparación de capas ambientales

##distancias geograficas
#library(devtools)
#devtools::install_github("DerekYves/placement")
library(placement)
library(raster)
library(rgdal)
library(sf)
library(fasterize)
library(tidyverse)



###Selección area:Chile

#pacman::p_load(readr, leaflet, geojsonio, stringr, rgdal)
#Chile <- readOGR("G:/Mi unidad/Documentos Yoryi/Documents yoryi/Doctorado Tesis/Mapas/CHL_adm(1)/CHL_adm0.shp")
Chile_reg <- st_read("G:/Mi unidad/Documentos Yoryi/Documents yoryi/Doctorado Tesis/Mapas/CHL_adm(1)/CHL_adm3.shp")
  
###Seleccion de estudio: Valparaiso , viña y concon

Valparaiso <- Chile_reg %>% dplyr::filter(NAME_1 == "Valparaíso") %>% dplyr::filter(NAME_3 %in% c("Vina del Mar", "Valparaiso"))


###Generacion de capas###

#####Utilizar capa de altura de http://srtm.csi.cgiar.org/ para utilizar como molde de raster para capas siguientes
#(mejor resolucion que worldclim)

Valpo <- raster::getData("SRTM",  lon = -71.61269, lat = -33.0472)

Valpo <- crop(Valpo, extent(Valparaiso))

Temporal <- Valpo
values(Temporal) <- ifelse(is.na(values(Temporal)), 1, NA)
Borde_costa_Mascara <- fasterize(sf = Valparaiso, raster = Valpo)

saveRDS(Borde_costa_Mascara, "Borde_costa_masc.rds")

##Altura 

Borde_costa_mascara <- read_rds("Borde_costa_masc.rds")
Alt <- Valpo*Borde_costa_mascara

saveRDS(Alt, "Alt.rds")


###Distancia al mar###############################################

Dist_marValpo <- distance(Temporal, doEdge = TRUE)
Dist_marValpo <- Dist_marValpo*Borde_costa_Mascara

saveRDS(Dist_marValpo, "Dist_marValpo.RDS")


###Distance to rivers##############################################
#Rios bajados del sitio de DivaGis
Rios_Chile <- st_read("G:/Mi unidad/Documentos Yoryi/Documents yoryi/Doctorado Tesis/Mapas/Inland_water/CHL_water_lines_dcw.shp")

#Cortar la capa en base a ese extent
RiosV <- st_crop(Rios_Chile, Valparaiso)

RRios <- rasterize(as_Spatial(RiosV), Borde_costa_Mascara, field = 1, background = NA)
DistanceToRiver <- distance(RRios)
DistanceToRiver <- DistanceToRiver*Borde_costa_Mascara

saveRDS(DistanceToRiver, "DistanceToRiver.RDS")

###Distancia a cuerpos de agua#######################################
#Cuerpos de agua bajados del sitio de DivaGis
Agua_Chile <- st_read("G:/Mi unidad/Documentos Yoryi/Documents yoryi/Doctorado Tesis/Mapas/Inland_water/CHL_water_areas_dcw.shp")

#Cortar la capa en base a ese extent
AguaV <- st_crop(Agua_Chile, Valparaiso)
RAgua <- fasterize(sf= AguaV, raster=Valpo)
Distancia_agua <- distance(RAgua)
Distancia_agua <- Distancia_agua*Borde_costa_Mascara

saveRDS(Distancia_agua, "Distancia_agua.RDS")

#############distancia caminos########
#realizada con capa nueva de imagen raster obtenida de la NASA:build_up

caminos <- read_rds("Capa_caminos.rds")

#Cortar la capa en base a ese extent
CaminosV <- crop(caminos, Valparaiso)
DistanceToRoad <- distance(CaminosV)

saveRDS(DistanceToRoad, "DistanceToRoad.RDS")

#############distancia construcciones########
#realizada con capa nueva de imagen raster obtenida de la NASA:build_up

#setwd("G:/Mi unidad/Documentos Yoryi/Documents yoryi/Doctorado Tesis/Monitoreo aves/Analisis_Occu_punto")

construccion <- read_rds("Capa_construccion.rds")

#Cortar la capa en base a ese extent
ConstruccionV <- crop(construccion, Valparaiso)
DistanceToBuild_up <- distance(ConstruccionV)
plot(DistanceToBuild_up)
saveRDS(DistanceToBuild_up, "DistanceToBuild_up.RDS")

#inMemory(DistanceToBuild_up)

###Poblacion############
#poblacion bajada del sitio de DivaGis/capa raster

Pop <- raster("G:/Mi unidad/Documentos Yoryi/Documents yoryi/Doctorado Tesis/Mapas/CHL_msk_pop/chl_msk_pop.gri")
Pop <- resample(Pop, Borde_costa_Mascara)
Pop <- Pop*Borde_costa_Mascara

saveRDS(Pop, "Pop.RDS")
##########################








###########################
#Caracteristicas a puntos de muestreo definidos en registro completo aves

Coords <- read_csv("G:/Mi unidad/Documentos Yoryi/Documents yoryi/Doctorado Tesis/Monitoreo aves/Muestreo Aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves.csv") %>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL"))) %>% dplyr::select(Sitio, Longitud, Latitud) %>% dplyr::distinct()

DistanceToRiver <- readRDS("DistanceToRiver.RDS")
DistanceToBuild_up <- readRDS("DistanceToBuild_up.RDS")
DistanceToRoad <- readRDS("DistanceToRoad.RDS")
Dist_marValpo <- readRDS("Dist_marValpo.RDS")
Distancia_agua <- readRDS("Distancia_agua.RDS")
Pop <- readRDS("Pop.RDS")
Alt <- readRDS("Alt.RDS")

Mapas1 <- stack(DistanceToRiver,Dist_marValpo, Distancia_agua, Alt) #Pop sacado el 7-10- capa poblacion sin playa ancha/revisar
names(Mapas1) <- c("Distancia_rio", "Distancia_Costa", "Distancia_Agua", "Altura")
Puntos <- SpatialPoints(coords = Coords[c("Longitud", "Latitud")], proj4string = CRS(as.character("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")))
Valores <- as.data.frame(raster::extract(Mapas1, Puntos, method = "bilinear"))
Coords <- bind_cols(Coords, Valores)

Distancias1 <- Coords %>% dplyr::select(Sitio, Distancia_rio, Distancia_Costa, Distancia_Agua, Altura)


Mapas2 <- stack(DistanceToBuild_up, DistanceToRoad)
names(Mapas2) <- c("Distancia_construccion", "Distancia_camino")
Puntos <- SpatialPoints(coords = Coords[c("Longitud", "Latitud")], proj4string = CRS(as.character("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")))
Valores <- as.data.frame(raster::extract(Mapas2, Puntos, method = "bilinear"))
Coords <- bind_cols(Coords, Valores)

Distancias2 <- Coords %>% dplyr::select(Sitio, Distancia_construccion, Distancia_camino)

Distancias <- full_join(Distancias1, Distancias2)

saveRDS(Distancias, "Distancias.rds")


