#Exploracion datos monitoreo visual aves

#reorden y vegan con preguntas generales Stefan tras reunion 18 agosot 2020

library(readxl)
library(tidyverse)
library(vegan)
library(MASS)
library(ggplot2)
library(knitr)



#####################################
# PRIMERA PARTE: RIQUEZA DE ESPECIES
#####################################

#Preparacion datos
BirdNames <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdNames.csv") %>% dplyr::select(-X1)

#Riqueza invierno
AvesInv <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv") %>% 
  dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))%>% rename(Nombre =Especie)
AvesInv <- AvesInv %>% group_by(Sitio, Nombre, AMBIENTE) %>% summarise(n=sum(N_individuos)) %>% 
  mutate(Nombre= str_replace(Nombre, "PELICANO", "PELICANO COMÚN"),
         Nombre= str_replace(Nombre, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Nombre= str_replace(Nombre, "LILEN", "LILE"))

AvesInv <- left_join(x=AvesInv, y= BirdNames) 

#Grafico riqueza especies invierno
AvesInv_riq <- AvesInv %>% group_by(Sitio, AMBIENTE, Habitat) %>% summarise(Riqueza = n())
AvesInv_riq <- AvesInv_riq %>% mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO","VERDE", "ROQUERIO INTERVENIDO","PLAYA INTERVENIDA",  "ROQUERIO NATURAL"))

Riqueza_Prom <- AvesInv_riq %>% group_by(AMBIENTE, Habitat) %>% summarise(Riqueza = mean(Riqueza))

ggplot(AvesInv_riq, aes(x=Sitio, y=Riqueza)) + geom_col(aes(fill=Habitat), position="dodge") + facet_wrap(~AMBIENTE, scales = "free_x", ncol=2)+
  scale_fill_manual(aesthetics = c("fill", "color"),values = c( '#5ab4ac','#d8b365')) +  theme_bw()+ ylab("Riqueza de especies invierno")+
  xlab("Sitios de muestreo")+   theme(axis.text.x=element_blank()) + geom_hline(data = Riqueza_Prom,aes(yintercept = Riqueza, color = Habitat), lty=2)

# Riqueza primavera

AMB_nombres <-read_rds("Occdata_occu.rds") %>% dplyr::select(Sitio, AMBIENTE)

AvesPrim <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")%>% 
  rename(Nombre =Especie) 
AvesPrim <- left_join(AvesPrim, AMB_nombres)
AvesPrim <- AvesPrim %>% group_by(Sitio, Nombre, AMBIENTE) %>% summarise(n=sum(N_individuos)) %>% 
  mutate(Nombre= str_replace(Nombre, "PELICANO", "PELICANO COMÚN"),
         Nombre= str_replace(Nombre, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Nombre= str_replace(Nombre, "LILEN", "LILE"),
         Nombre= str_replace(Nombre, "CAHUIL", "GAVIOTA CAHUIL")) %>% 
  mutate(AMBIENTE= str_replace(AMBIENTE, "ROCA INTERVENIDA", "ROQUERIO INTERVENIDO"),
         AMBIENTE= str_replace(AMBIENTE, "ROCA NATURAL", "ROQUERIO NATURAL"))

AvesPrim <- left_join(x=AvesPrim, y= BirdNames) 

#Grafico riqueza especies primavera
AvesPrim_riq <- AvesPrim %>% group_by(Sitio, AMBIENTE, Habitat) %>% summarise(Riqueza = n())
AvesPrim_riq <- AvesPrim_riq %>% mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO","VERDE", "ROQUERIO INTERVENIDO","PLAYA INTERVENIDA",  "ROQUERIO NATURAL"))

Riqueza_Prom <- AvesPrim_riq %>% group_by(AMBIENTE, Habitat) %>% summarise(Riqueza = mean(Riqueza))

ggplot(AvesPrim_riq, aes(x=Sitio, y=Riqueza)) + geom_col(aes(fill=Habitat), position="dodge") + facet_wrap(~AMBIENTE, scales = "free_x", ncol=2)+
  scale_fill_manual(aesthetics = c("fill", "color"),values = c( '#5ab4ac','#d8b365')) +  theme_bw()+ ylab("Riqueza de especies primavera")+
  xlab("Sitios de muestreo")+ theme(axis.text.x=element_blank()) + geom_hline(data = Riqueza_Prom,aes(yintercept = Riqueza, color = Habitat), lty=2)




#####################################
# SEGUNDA PARTE: ORDINATION ANALISIS- para presencia sp, abundancias y prob ocupancia
#####################################

#preparacion de datos 

AvesInv <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv") %>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))
AvesInv <- AvesInv %>% group_by(Sitio, Especie) %>% summarise(n=sum(N_individuos)) %>% spread(key = Especie, value = n, fill=0) %>% ungroup
AvesInv <- AvesInv %>% dplyr::select(-Sitio)

AvesPrim <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")
AvesPrim <- AvesPrim %>% group_by(Sitio, Especie) %>% summarise(n=sum(N_individuos)) %>% spread(key = Especie, value = n, fill=0) %>% ungroup
AvesPrim <-AvesPrim %>% dplyr::select(-Sitio) 

#Datos ambientales  incluidos buffers
Amb<- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Occdata_occu.rds")
Amb <- Amb %>% dplyr::select(-Sitio)%>% 
  dplyr::select(CobVeg, AMBIENTE, Distancia_rio, Altura, `Buffer_2200_Bosque Nativo`, `Buffer_2200_Cultivos`, `Buffer_2200_Grava`, `Buffer_2200_Oceano`, `Buffer_2200_Pastizales`, `Buffer_2200_Matorrales`, `Buffer_2200_Sup impermeables`, `Buffer_2200_Suelo arenoso`, `Buffer_2200_Plantación de árboles`) %>% 
  rename( `Cobertura vegetal` =CobVeg) %>% rename( `Distancia a río` =Distancia_rio) %>% 
  rename( `Bosque nativo` =`Buffer_2200_Bosque Nativo`) %>% rename( Cultivos =`Buffer_2200_Cultivos`) %>% 
  rename( Grava =`Buffer_2200_Grava`) %>% rename( Oceano =`Buffer_2200_Oceano`) %>% 
  rename( Pastizales =`Buffer_2200_Pastizales`) %>% rename( `Matorrales` =`Buffer_2200_Matorrales`) %>% 
  rename( `Superficies impermeables` =`Buffer_2200_Sup impermeables`) %>% rename( `Suelo arenoso` =`Buffer_2200_Suelo arenoso`) %>% 
  rename( `Plantacion de arboles` =`Buffer_2200_Plantación de árboles`)
  

#############################################################################################################################################
# PRES-AUS: En base a funcion metaMDS, que estandariza las abundancias, considera solo composicion de especies

##### METAMDS Non-metric Multidimensional scaling- NMDS

#Invierno
ord1Inv <- metaMDS(AvesInv) #stress= 0.234609 
stressplot(ord1Inv)

ordiplot(ord1Inv)
ordihull(ord1Inv,groups = Amb$AMBIENTE)
points(ord1Inv)

AnosimInv <-anosim(AvesInv, grouping=Amb$AMBIENTE, permutations = 999, distance = "bray", strata = NULL,
       parallel = getOption("mc.cores"))
summary(AnosimInv)
plot(AnosimInv)

##Primavera
ord1Prim <- metaMDS(AvesPrim) #stress= 0.1947774
stressplot(ord1Prim)

ordiplot(ord1Prim)
ordihull(ord1Prim, groups = Amb$AMBIENTE)
points(ord1Prim)

AnosimPrim <-anosim(AvesPrim, grouping=Amb$AMBIENTE, permutations = 999, distance = "bray", strata = NULL,
                   parallel = getOption("mc.cores"))
summary(AnosimPrim)
plot(AnosimPrim)


##### SIMPER:gives the contribution of each species to overall dissimilarities, but these
#          are caused by variation in species abundances, and only partly by differences among groups.

#  The function displays most important species for each pair of groups. These species contribute at
#  least to 70 % of the differences between groups.

SimperInv <- simper(AvesInv, group = Amb$AMBIENTE)
summary(SimperInv, ordered = TRUE)

SimperPrim <- simper(AvesPrim, group=Amb$AMBIENTE)
summary(SimperPrim)


##### ENVIT: Fits an Environmental Vector or Factor onto an Ordination

EnvfitInv <- envfit(ord=ord1Inv, env=Amb, permutations = 999, strata = NULL)
plot(ord1Inv)
plot(EnvfitInv)
ordihull(ord1Inv, groups = Amb$AMBIENTE)


EnvfitPrim<- envfit(ord=ord1Prim, env=Amb, permutations = 999, strata = NULL)
plot(ord1Prim)
plot(EnvfitPrim)
ordihull(ord1Prim, groups = Amb$AMBIENTE)


EnvfitPrim <- envfit(ord1Prim~ AMBIENTE+`Cobertura vegetal`+ `Distancia a río`+Altura+ `Bosque nativo`+ Cultivos+ Grava + Oceano +Pastizales+ Matorrales+`Superficies impermeables` + `Suelo arenoso`+ `Plantacion de arboles`, data=Amb, perm=999)
EnvfitPrim


#############################################################################################################################################
# ABUNDANCIAS: Modificacion de funcion metaMDS para considderar abundancias de especies por sitio

##### METAMDS: Non-metric Multidimensional scaling- NMDS

#Invierno
ord2Inv <- metaMDS(AvesInv,autotransform = FALSE) #stress= 0.1325971
stressplot(ord2Inv)

ordiplot(ord2Inv)
ordihull(ord2Inv,groups = Amb$AMBIENTE)
points(ord2Inv)

AnosimInv <-anosim(AvesInv, grouping=Amb$AMBIENTE, permutations = 999, distance = "bray", strata = NULL,
                   parallel = getOption("mc.cores"))
summary(AnosimInv)
plot(AnosimInv)


##Primavera
ord2Prim <- metaMDS(AvesPrim,autotransform = FALSE) #stress= 0.0.1566688 
stressplot(ord2Prim)

ordiplot(ord2Prim)
ordihull(ord2Prim, groups = Amb$AMBIENTE)
points(ord2Prim)

AnosimPrim <-anosim(AvesPrim, grouping=Amb$AMBIENTE, permutations = 999, distance = "bray", strata = NULL,
                    parallel = getOption("mc.cores"))
summary(AnosimPrim)
plot(AnosimPrim)



##### SIMPER:gives the contribution of each species to overall dissimilarities, but these
#          are caused by variation in species abundances, and only partly by differences among groups.

#  The function displays most important species for each pair of groups. These species contribute at
#  least to 70 % of the differences between groups.

SimperInv <- simper(AvesInv, group = Amb$AMBIENTE)
summary(SimperInv, ordered = TRUE)

SimperPrim <- simper(AvesPrim, group=Amb$AMBIENTE)
summary(SimperPrim)


##### ENVFIT: Fits an Environmental Vector or Factor onto an Ordination

EnvfitInv <- envfit(ord=ord2Inv, env=Amb, permutations = 999, strata = NULL)
plot(ord2Inv)
plot(EnvfitInv)
ordihull(ord2Inv, groups = Amb$AMBIENTE)

EnvfitInv <- envfit(ord2Inv~ AMBIENTE+`Cobertura vegetal`+ `Distancia a río`+Altura+ `Bosque nativo`+ Cultivos+ Grava + Oceano +Pastizales+ Matorrales+`Superficies impermeables` + `Suelo arenoso`+ `Plantacion de arboles`, data=Amb, perm=999)
EnvfitInv

EnvfitPrim<- envfit(ord=ord2Prim, env=Amb, permutations = 999, strata = NULL)
plot(ord2Prim)
plot(EnvfitPrim)
ordihull(ord2Prim, groups = Amb$AMBIENTE)


EnvfitPrim <- envfit(ord2Prim~ AMBIENTE+`Cobertura vegetal`+ `Distancia a río`+Altura+ `Bosque nativo`+ Cultivos+ Grava + Oceano +Pastizales+ Matorrales+`Superficies impermeables` + `Suelo arenoso`+ `Plantacion de arboles`, data=Amb, perm=999)
EnvfitPrim


#############################################################################################################################################
# OCUPANCIAS: ocupando probabilidad de presencia por sitio

#preparacion de datos

PorSitioInv <- readRDS("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/PorSitioInv.rds") %>% 
  dplyr::select(-Ambiente) %>% dplyr::select(-Sitio)
PorSitioPrim <- readRDS("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/PorSitioPrim.rds") %>%
  dplyr::select(-Ambiente) %>% dplyr::select(-Sitio)


##### METAMDS: Non-metric Multidimensional scaling- NMDS

#Invierno
ord3Inv <- metaMDS(PorSitioInv, autotransform = FALSE) #stress= 0.2183811
stressplot(ord3Inv)

ordiplot(ord3Inv)
ordihull(ord3Inv,groups = Amb$AMBIENTE)
points(ord3Inv)

AnosimInv <-anosim(PorSitioInv, grouping=Amb$AMBIENTE, permutations = 999, distance = "bray", strata = NULL,
                   parallel = getOption("mc.cores"))
summary(AnosimInv)
plot(AnosimInv)


##Primavera
ord3Prim <- metaMDS(PorSitioPrim,autotransform = FALSE) #stress= 0.2280575 
stressplot(ord3Prim)

ordiplot(ord3Prim)
ordihull(ord3Prim, groups = Amb$AMBIENTE)
points(ord3Prim)

AnosimPrim <-anosim(PorSitioPrim, grouping=Amb$AMBIENTE, permutations = 999, distance = "bray", strata = NULL,
                    parallel = getOption("mc.cores"))
summary(AnosimPrim)
plot(AnosimPrim)



##### SIMPER:gives the contribution of each species to overall dissimilarities, but these
#          are caused by variation in species abundances, and only partly by differences among groups.

#  The function displays most important species for each pair of groups. These species contribute at
#  least to 70 % of the differences between groups.

SimperInv <- simper(PorSitioInv, group = Amb$AMBIENTE)
summary(SimperInv, ordered = TRUE)

SimperPrim <- simper(PorSitioPrim, group=Amb$AMBIENTE)
summary(SimperPrim)


##### ENVFIT: Fits an Environmental Vector or Factor onto an Ordination

EnvfitInv <- envfit(ord=ord3Inv, env=Amb, permutations = 999, strata = NULL)
plot(ord3Inv)
plot(EnvfitInv)
ordihull(ord3Inv, groups = Amb$AMBIENTE)

EnvfitInv <- envfit(ord3Inv~ AMBIENTE+`Cobertura vegetal`+ `Distancia a río`+Altura+ `Bosque nativo`+ Cultivos+ Grava + Oceano +Pastizales+ Matorrales+`Superficies impermeables` + `Suelo arenoso`+ `Plantacion de arboles`, data=Amb, perm=999)
EnvfitInv

EnvfitPrim<- envfit(ord=ord3Prim, env=Amb, permutations = 999, strata = NULL)
plot(ord3Prim)
plot(EnvfitPrim)
ordihull(ord3Prim, groups = Amb$AMBIENTE)


EnvfitPrim <- envfit(ord3Prim~ AMBIENTE+`Cobertura vegetal`+ `Distancia a río`+Altura+ `Bosque nativo`+ Cultivos+ Grava + Oceano +Pastizales+ Matorrales+`Superficies impermeables` + `Suelo arenoso`+ `Plantacion de arboles`, data=Amb, perm=999)
EnvfitPrim

