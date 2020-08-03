##Generación de capas de buffer (10 distancias distintas desde de los puntos de muestreo)
#A partir de raster de cobertura de suelo de Chile completo

library(leaflet)
library(raster)
library(tidyverse)
library(sf)

setwd("~/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit")

LandCover <- readRDS("Landcover.rds")
values(LandCover) <- ifelse(is.na(values(LandCover)), 0, values(LandCover))
Codes <- readRDS("Codes.rds")

Coords <- read_csv("~/Documents/Doctorado tesis/Monitoreo aves/Muestreo Aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves.csv") %>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL"))) %>% dplyr::select(Sitio, Longitud, Latitud) %>% dplyr::distinct() %>% st_as_sf(coords = c(2,3), crs = "+proj=longlat +datum=WGS84 +no_defs")

## Buffers

Coords2 <- Coords %>% sf::st_transform("+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

Occdata_ocu <- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Analisis_Occu_punto/Occdata_ocu.rds")

Distancias <- 2200

Bs <- list()
for(x in 1:length(Distancias)){
  Buffers <- list()
  
  for(i in 1:nrow(Coords2)){
    Buffer <- Coords2[i,] %>% st_buffer(Distancias[x]) %>% rasterize(LandCover)
    
    Temp <- raster::trim(Buffer * LandCover)
    
    Table <- data.frame(Code = as.numeric(as.character(names(values(Temp) %>% table))), Frecuencia = values(Temp) %>% table %>% as.numeric(), stringsAsFactors = F) %>% full_join(Codes) %>% dplyr::filter(!is.na(Frecuencia))
  
    
    Buffers[[i]] <- Table %>% dplyr::select(Selected, Frecuencia) %>% mutate(Proporcion = 100*(Frecuencia/(table(values(Buffer))) %>% as.numeric())) %>% dplyr::select(Selected, Proporcion) %>% pivot_wider(names_from = Selected, values_from = Proporcion, names_prefix = paste0("Buffer_", Distancias[x],"_"))
    message(paste("Site", i, "sum =", (Buffers[[1]] %>% rowSums())))
  }
  Bs[[x]] <- Buffers %>% purrr::reduce(bind_rows)
  Bs[[x]][is.na(Bs[[x]])] <- 0
  Bs[[x]]$Sitio <- Coords$Sitio
  Occdata_ocu <- Occdata_ocu %>% full_join(Bs[[x]])
  print(paste(x, "de", length(Distancias), "listo!"))
}


saveRDS(Occdata_ocu, "Occdata_occu_Buffers.rds")


Occdata_ocu <- Occdata_ocu %>% mutate(Buffer_30_Oceano = Buffer_30_Oceano + Buffer_30_0, Buffer_600_Oceano = Buffer_600_Oceano + Buffer_600_0, Buffer_1100_Oceano = Buffer_1100_Oceano + Buffer_1100_0, Buffer_1700_Oceano = Buffer_1700_Oceano + Buffer_1700_0, Buffer_2200_Oceano = Buffer_2200_Oceano + Buffer_2200_0, Buffer_2800_Oceano = Buffer_2800_Oceano + Buffer_2800_0, Buffer_3300_Oceano = Buffer_3300_Oceano + Buffer_3300_0, Buffer_3900_Oceano = Buffer_3900_Oceano + Buffer_3900_0, Buffer_4400_Oceano = Buffer_4400_Oceano + Buffer_4400_0, Buffer_5000_Oceano = Buffer_5000_Oceano + Buffer_5000_0) %>% 
  dplyr::select(-Buffer_30_0, -Buffer_600_0, -Buffer_1100_0, -Buffer_1700_0, -Buffer_2200_0, -Buffer_2800_0, -Buffer_3300_0, -Buffer_3900_0, -Buffer_4400_0, -Buffer_5000_0)

saveRDS(Occdata_ocu, "Occdata_occu.rds")


############
Buffer <- Coords2 %>% st_buffer(Distancias[1]) %>% rasterize(LandCover, 1)

Temp <- raster::trim(Buffer * LandCover)

a <- rasterToPolygons(Temp) %>% st_as_sf() %>% rename(Code = layer)

test <- merge(a, Codes) %>% mutate(Selected = as.character(Selected),Selected = ifelse(Selected == "0", "Oceano", Selected))

ggplot() + geom_sf(data = test, aes(fill = Selected))

test2 <- test %>% dplyr::select(Selected) %>% group_by(Selected) %>% summarise(n = n()) %>% ungroup() %>% sf::st_transform('+proj=longlat +datum=WGS84' )

ggplot() + geom_sf(data = test2, aes(fill = Selected))

test3 <- sf::as_Spatial(test2)



leaflet(test3) %>% addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorFactor("viridis", Selected)(Selected),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE), popup = ~Selected)%>%
  addLegend(pal = colorFactor("viridis",test3$Selected), values = ~test3$Selected, opacity = 1)

