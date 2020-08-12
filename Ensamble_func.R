# Ensamble aves definido por rasgos funcionales

library(ggrepel)
library(tidyverse)
library(stringr)
library(MuMIn)
library(caret)
library(readr)

setwd("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit")



#Informacion de ensambles

Todo_NMDS <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Analisis_Occu_punto/Todo_NMDS.csv")
Hull <- Todo_NMDS %>% group_by(AMBIENTE, Estacion) %>% slice(chull(MDS1, MDS2))


# Informacion rasgos funcionales
BirdFunc <- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdFunc.rds")
colnames(BirdFunc) <- make.names(colnames(BirdFunc))

Species <- read_csv("Species.csv") %>%   rename(Nombre =Especies)
Species <- full_join(x=Species, y= BirdFunc)

Species_hull <- Species %>% group_by(Diet.5Cat) %>% dplyr::filter(!is.na(MDS1), !is.na(MDS2))%>% slice(chull(MDS1, MDS2))#####


# Grafico con areas ambientes y especies con puntos

ggplot(Todo_NMDS, aes(x = MDS1, y = MDS2)) + 
  geom_polygon(data = Hull, aes( fill = AMBIENTE), alpha = 0.2)  + 
  facet_wrap(~Estacion) + 
  geom_point(data = Species, aes(color=Diet.5Cat)) + 
  ggrepel::geom_text_repel(data = Species, aes(label = Nombre), size=2)+
  theme_bw()



###
ggplot(Todo_NMDS, aes(x = MDS1, y = MDS2)) + 
  geom_polygon(data = Hull, aes(color = AMBIENTE), alpha = 0.2)  + 
  facet_wrap(~Estacion) + 
  geom_point(data = Species, size=1, aes(color=Diet.5Cat)) + 
  geom_polygon(data=Species_hull,  aes(lty = Diet.5Cat, fill=Diet.5Cat), alpha = 0.2)+
  ggrepel::geom_text_repel(data = Species, aes(label = Nombre), size=2)+
  theme_bw()




#########################################################################################

#Resumen de modelos e importancia de variables

#Invierno
ResultadosInv2 <- ResultadosInv %>% group_by(Spp) %>% summarise(Modelo = unique(Modelo))


TablaInv <-ResultadosInv2 %>% mutate(CobVeg = ifelse(str_detect(Modelo,"CobVeg"), 1, 0), 
                                     Altura = ifelse(str_detect(Modelo, "Altura"),1, 0),
                                     Distancia_rio = ifelse(str_detect(Modelo, "Distancia_rio"),1, 0),
                                     Buffer_2200_Pastizales = ifelse(str_detect(Modelo, "Buffer_2200_Pastizales"),1, 0),
                                     Buffer_2200_Matorrales = ifelse(str_detect(Modelo, "Buffer_2200_Matorrales"),1, 0),
                                     Buffer_2200_Oceano = ifelse(str_detect(Modelo,"Buffer_2200_Oceano"), 1, 0), 
                                     Buffer_2200_Sup_impermeables = ifelse(str_detect(Modelo, "Buffer_2200_Sup_impermeables"),1, 0),
                                     Buffer_2200_Suelo_arenoso = ifelse(str_detect(Modelo,"Buffer_2200_Suelo_arenoso"), 1, 0), 
                                     Buffer_2200_Cultivos = ifelse(str_detect(Modelo,"Buffer_2200_Cultivos"), 1, 0), 
                                     Buffer_2200_Plantación_de_árboles = ifelse(str_detect(Modelo, "Buffer_2200_Plantación_de_árboles"),1, 0)) %>% 
  ungroup() %>% summarise_if(is.numeric, sum) %>% t() %>% as.data.frame()%>% rename(Invierno=V1)

TablaInv$Variables <- rownames(TablaInv)


#Primavera

ResultadosPrim2 <- ResultadosPrim %>% group_by(Spp) %>% summarise(Modelo = unique(Modelo))


TablaPrim <-ResultadosPrim2 %>% mutate(CobVeg = ifelse(str_detect(Modelo,"CobVeg"), 1, 0), 
                                       Altura = ifelse(str_detect(Modelo, "Altura"),1, 0),
                                       Distancia_rio = ifelse(str_detect(Modelo, "Distancia_rio"),1, 0),
                                       Buffer_2200_Pastizales = ifelse(str_detect(Modelo, "Buffer_2200_Pastizales"),1, 0),
                                       Buffer_2200_Matorrales = ifelse(str_detect(Modelo, "Buffer_2200_Matorrales"),1, 0),
                                       Buffer_2200_Oceano = ifelse(str_detect(Modelo,"Buffer_2200_Oceano"), 1, 0), 
                                       Buffer_2200_Sup_impermeables = ifelse(str_detect(Modelo, "Buffer_2200_Sup_impermeables"),1, 0),
                                       Buffer_2200_Suelo_arenoso = ifelse(str_detect(Modelo,"Buffer_2200_Suelo_arenoso"), 1, 0), 
                                       Buffer_2200_Cultivos = ifelse(str_detect(Modelo,"Buffer_2200_Cultivos"), 1, 0), 
                                       Buffer_2200_Plantación_de_árboles = ifelse(str_detect(Modelo, "Buffer_2200_Plantación_de_árboles"),1, 0)) %>% 
  ungroup() %>% summarise_if(is.numeric, sum) %>% t() %>%  as.data.frame()%>% rename(Primavera=V1)

TablaPrim$Variables <- rownames(TablaPrim)

###
Resumen_Modelos <- full_join(TablaInv, TablaPrim) %>% dplyr::select(Variables, Invierno, Primavera)

#################################################################

#EVALUANDO RELACIONES CON VARIABLES MÁS IMPORTANTES
#######################################################ideas ideas ideas ideas ideas

#especies por estrato de forrajeo dominante

ForStratDom <- BirdFunc %>% pivot_longer(ForStrat.watbelowsurf:ForStrat.aerial, names_to = "Strata", values_to = "Percentage") %>% 
  group_by(Nombre) %>% dplyr::filter(Percentage == max(Percentage)) %>% 
dplyr::select(c(Nombre, Especie, Strata, Percentage))

#Prediccion ocupancia por sitio por especies (variables sitios)

PredSpSitio2 <- t(PredOccuSitio) %>%  as.data.frame()

names(PredSpSitio2) <- PredSpSitio2 %>% slice(1) %>% unlist()
PredSpSitio2 <- PredSpSitio2[-c(1:2),] 

PredSpSitio2$Especies <- rownames(PredSpSitio2)



  
##
PredSpSitio <- PredOccuSitio
PredSpSitio$Estacion <- rep(c("Primavera", "Invierno"), each = 36)
  
PredSpSitio <-PredSpSitio %>%pivot_longer(AGUILUCHO:SCUA, names_to = "Especie", values_to = "Ocupancia") %>% 
  group_split(Estacion) %>% 
  purrr::map(~pivot_wider(.x, names_from = Sitio, values_from = Ocupancia)) %>% 
  reduce(bind_rows)
##

