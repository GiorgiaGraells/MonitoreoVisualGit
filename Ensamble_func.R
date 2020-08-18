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
BirdFunc <- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdFunc.rds")%>% mutate(Especie=str_replace_all(Especie, " ", "_"))
colnames(BirdFunc) <- make.names(colnames(BirdFunc))

Species <- read_csv("Species.csv") %>%   rename(Especie =Especies)   
Species <- full_join(x=Species, y= BirdFunc)

Species_hull <- Species %>% group_by(Diet.5Cat) %>% dplyr::filter(!is.na(MDS1), !is.na(MDS2))%>% slice(chull(MDS1, MDS2))#####




# Grafico con areas ambientes y especies con puntos

ggplot(Todo_NMDS, aes(x = MDS1, y = MDS2)) + 
  geom_polygon(data = Hull, aes( fill = AMBIENTE), alpha = 0.3)  + 
  facet_wrap(~Estacion) + 
  geom_point(data = Species, aes(color=Diet.5Cat)) + 
  scale_fill_manual(values=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'))+
    theme_bw()

# Grafico con areas ambientes y especies con nombres

ggplot(Todo_NMDS, aes(x = MDS1, y = MDS2)) + 
  geom_polygon(data = Hull, aes( fill = AMBIENTE), alpha = 0.3)  + 
  facet_wrap(~Estacion) + 
  geom_point(data = Species, aes(color=Diet.5Cat)) + 
  ggrepel::geom_text_repel(data = Species, aes(label = Nombre), size=3)+
  scale_fill_manual(values=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'))+
  theme_bw()


########

ggplot(Todo_NMDS, aes(x = MDS1, y = MDS2)) + 
  geom_polygon(data = Hull, aes(color = AMBIENTE), alpha = 0.2)  + 
  facet_wrap(~Estacion) + 
  geom_point(data = Species, size=1) + 
  geom_polygon(data=Species_hull,  aes(lty = Diet.5Cat, fill=Diet.5Cat), alpha = 0.2)+
  scale_color_manual(values=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'))+
    theme_bw()




#########################################################################################

#Resumen de modelos e importancia de variables

ResultadosInv <- readRDS("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/ResultadosInv.rds")
ResultadosPrim <- readRDS("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/ResultadosPrim.rds")
PorSitioInv <- readRDS("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/PorSitioInv.rds")
PorSitioPrim <- readRDS("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/PorSitioPrim.rds")


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




################## frecuencia prediccion ocupancia >0 o num sp, definida por % variable (sitio)

data_ocu <-read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Occdata_occu.rds")

#Invierno
Para_Inv <- full_join(data_ocu, PorSitioInv) %>% 
  select(CobVeg, Altura, Buffer_2200_Cultivos, `Buffer_2200_Sup impermeables` , Sitio, AMBIENTE, Cathartes_aura:Stercorarius_chilensis) %>% 
  pivot_longer(Cathartes_aura:Stercorarius_chilensis, names_to = "Especie", values_to = "Ocupancia") %>% 
  dplyr::filter(Ocupancia > 0.3)


Especies_por_sitio <- Para_Inv %>% group_by(Sitio) %>% summarise(Riqueza = n())

Para_Inv2 <- full_join(Para_Inv, Especies_por_sitio) %>% full_join(ForStratDom)


ggplot(Para_Inv2, aes(x = Altura, y = Ocupancia, group = Especie)) + geom_point(aes(color = Strata, alpha=2)) + 
  facet_wrap(~AMBIENTE) #+ theme(legend.position = "none")

#Primavera
Para_Prim <- full_join(data_ocu, PorSitioPrim) %>% 
  select(CobVeg, Altura, Buffer_2200_Cultivos, `Buffer_2200_Sup impermeables` , Sitio, AMBIENTE, Anairetes_parulus:Zonotrichia_capensis) %>% 
  pivot_longer(Anairetes_parulus:Zonotrichia_capensis, names_to = "Especie", values_to = "Ocupancia") %>% 
  dplyr::filter(Ocupancia > 0.3)


Especies_por_sitio2 <- Para_Prim%>% group_by(Sitio) %>% summarise(Riqueza = n())

Para_Prim2 <- full_join(Para_Prim, Especies_por_sitio2) %>% full_join(ForStratDom)


ggplot(Para_Prim2, aes(x = Altura, y = Ocupancia, group = Especie)) + geom_point(aes(color = Strata), size=1) + 
  facet_wrap(~AMBIENTE) +theme_bw()





#para comparar urbano con roca natural
Comp <- Para_Prim2 %>% dplyr::filter(AMBIENTE==c("ROCA NATURAL","URBANO"))

ggplot(Comp, aes(x = Altura, y = Ocupancia, group = Especie)) + geom_point(aes(color = Strata, alpha=0.5)) + 
  facet_wrap(~AMBIENTE) +theme_bw()

ggplot(Comp, aes(x = CobVeg, y = Ocupancia, group = Especie)) + geom_point(aes(color = Strata, alpha=0.5)) + 
  facet_wrap(~AMBIENTE) +theme_bw()

ggplot(Comp, aes(x = Buffer_2200_Cultivos, y = Ocupancia, group = Especie)) + geom_point(aes(color = Strata, alpha=0.5)) + 
  facet_wrap(~AMBIENTE) +theme_bw()

ggplot(Comp, aes(x = 'Buffer_2200_Sup impermeables', y = Ocupancia, group = Especie)) + geom_point(aes(color = Strata, alpha=0.5)) + 
  facet_wrap(~AMBIENTE) +theme_bw()

