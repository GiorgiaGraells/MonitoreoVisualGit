#Exploracion datos monitoreo visual aves

#reorden y vegan con preguntas generales Stefan tras reunion 18 agosot 2020

library(readxl)
library(tidyverse)
library(vegan)
library(MASS)
library(ggplot2)
library(knitr)

#Preparacion datos


BirdNames <- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdFunc.rds")%>% mutate(Especie=str_replace_all(Especie, " ", "_"))
BirdNames <- BirdNames %>%  dplyr::select(Nombre, Especie) %>%  rename(cientifico =Especie) %>%   rename(Especie =Nombre) 

AvesInv <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv") %>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))
AvesInv <- AvesInv %>% group_by(Sitio, Especie) %>% summarise(n=sum(N_individuos)) %>% 
  mutate(Especie= str_replace(Especie, "PELICANO", "PELICANO COMÚN"),
         Especie= str_replace(Especie, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Especie= str_replace(Especie, "LILEN", "LILE"))

AvesInv <- left_join(x=AvesInv, y= BirdNames)





...


AvesPrim <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")
AvesPrim <- AvesPrim %>% group_by(Sitio, Especie) %>% summarise(n=sum(N_individuos)) %>% spread(key = Especie, value = n, fill=0) %>% ungroup
AvesPrim <-AvesPrim %>% dplyr::select(-Sitio)









#preparacion de datos para matriz similitudes 

AvesInv <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv") %>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))
AvesInv <- AvesInv %>% group_by(Sitio, Especie) %>% summarise(n=sum(N_individuos)) %>% spread(key = Especie, value = n, fill=0) %>% ungroup
AvesInv <- AvesInv %>% dplyr::select(-Sitio)

AvesPrim <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")
AvesPrim <- AvesPrim %>% group_by(Sitio, Especie) %>% summarise(n=sum(N_individuos)) %>% spread(key = Especie, value = n, fill=0) %>% ungroup
AvesPrim <-AvesPrim %>% dplyr::select(-Sitio)

#Datos ambientales  incluidos buffers
Amb<- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Occdata_occu.rds")
Amb <- Amb %>% dplyr::select(-Sitio)



