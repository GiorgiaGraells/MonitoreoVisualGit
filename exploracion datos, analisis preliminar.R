#Exploración datos y analisis preliiminares
#EXPLORACION 

library(readxl)
library(tidyverse)
library(MASS)
library(ggplot2)
library(knitr)
library(vegan)

#############
#Preparacion datos

AvesInv <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/Muestreo Aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves.csv") %>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))
AvesInv <- AvesInv %>% group_by(Sitio, Especie) %>% summarise(n=sum(N_individuos)) %>% spread(key = Especie, value = n, fill=0) %>% ungroup
AvesInv <- AvesInv %>% dplyr::select(-Sitio)

AvesPrim <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")
AvesPrim <- AvesPrim %>% group_by(Sitio, Especie) %>% summarise(n=sum(N_individuos)) %>% spread(key = Especie, value = n, fill=0) %>% ungroup
AvesPrim <-AvesPrim %>% dplyr::select(-Sitio)

Amb<- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/Analisis_Occu_punto/Occdata_ocu.rds")
Amb <- Amb %>% dplyr::select(-Sitio)

#############
#matriz de similitudes
#Invierno
Inv.dist <- vegdist(AvesInv)
Inv.ano <- with(Amb, anosim(Inv.dist, AMBIENTE))
summary(Inv.ano)
plot(Inv.ano, xlab="Clases", ylab="Ranking de distancias")

Prim.dist <- vegdist(AvesPrim)
Prim.ano <- with(Amb, anosim(Prim.dist, AMBIENTE))
summary(Prim.ano)
plot(Prim.ano, xlab="Clases", ylab="Ranking de distancias")

#########

#reagrupación aves por ambiente

# Inv_amb <- AvesInv %>% left_join(Sitio_Amb) %>% group_by(AMBIENTE, Especie) %>% summarise(n=sum(N_individuos))%>% spread(key = Especie, value = n, fill=0) %>% ungroup
# Prim_amb <- AvesPrim %>% left_join(Sitio_Amb) %>% group_by(AMBIENTE, Especie) %>% summarise(n=sum(N_individuos))%>% spread(key = Especie, value = n, fill=0) %>% ungroup
# 
# write.csv(Inv_amb, "AvesInv_porAmb.csv")
# write.csv(Prim_amb, "AvesPrim_porAmb.csv")

AvesInv_porAmb <- read_csv("AvesInv_porAmb.csv")
AvesPrim_porAmb <- read_csv("AvesPrim_porAmb.csv")

ggplot(AvesInv_porAmb, aes(AMBIENTE,CHERCAN)) + geom_point() 
       
