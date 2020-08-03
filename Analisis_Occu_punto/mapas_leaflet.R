#LEAFLET PARA VER MAPAS Y PUNTOS

library(leaflet)

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(Dist_marValpo),
                    na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(Dist_marValpo, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(Dist_marValpo),
            title = "Distancia al mar") %>% addMarkers(data=Coords, ~Longitud, ~Latitud, label = ~Sitio)


pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(Pop),
                    na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(Pop, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(Pop),
            title = "poblacion") %>% addMarkers(data=Puntos)



distancia_rio<- read_rds("DistanceToRiver.rds")

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(distancia_rio),
                    na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(distancia_rio, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(distancia_rio),
            title = "Distancia rio") %>% addMarkers(data=Coords, ~Longitud, ~Latitud, label = ~Sitio)