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


#########################
## SEGUNDA PARTE: ABUNDANCIA


#preparacion de datos bundancia por especie

AvesInv <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv") %>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))
AvesInv <- AvesInv %>% group_by(Sitio, Especie) %>% summarise(n=sum(N_individuos)) %>% spread(key = Especie, value = n, fill=0) %>% ungroup
AvesInv <- AvesInv %>% dplyr::select(-Sitio)

AvesPrim <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")
AvesPrim <- AvesPrim %>% group_by(Sitio, Especie) %>% summarise(n=sum(N_individuos)) %>% spread(key = Especie, value = n, fill=0) %>% ungroup
AvesPrim <-AvesPrim %>% dplyr::select(-Sitio)

#Datos ambientales  incluidos buffers
Amb<- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Occdata_occu.rds")
Amb <- Amb %>% dplyr::select(-Sitio)





#### Non-metric Multidimensional scaling- NMDS
#Invierno

ord1Inv <- metaMDS(AvesInv) #stress= 0.2316387
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
ordihull(ord1Prim, groups = Amb$AMBIENTE, show.groups = TRUE)
points(ord1Prim)

AnosimPrim <-anosim(AvesPrim, grouping=Amb$AMBIENTE, permutations = 999, distance = "bray", strata = NULL,
                   parallel = getOption("mc.cores"))
summary(AnosimPrim)
plot(AnosimPrim)



####SIMPER:gives the contribution of each species to overall dissimilarities, but these
#          are caused by variation in species abundances, and only partly by differences among groups.

#  The function displays most important species for each pair of groups. These species contribute at
#  least to 70 % of the differences between groups.

SimperInv <- simper(AvesInv, group = Amb$AMBIENTE)
summary(SimperInv, ordered = TRUE)

SimperPrim <- simper(AvesPrim, group=Amb$AMBIENTE)
summary(SimperPrim)
