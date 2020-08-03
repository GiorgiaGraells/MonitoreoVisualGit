#Base de datos rasgos funcionales
#

library(tidyverse)
library(readr)
library(readxl)

#Se toman los nombres de las 37sp desde el archivo  Bird_guilts.xlsx
Bird_guilts <- read_excel("Bird guilts.xlsx") %>% dplyr::select(Nombre, Especie)

BirdFuncDat <- read_delim("BirdFuncDat.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(Especie=Scientific) %>% mutate(Especie= str_replace(Especie, "Larus pipixcan", "Leucophaeus pipixcan")) %>% 
  mutate(Especie= str_replace(Especie, "Larus modestus", "Leucophaeus modestus"))


BirdFunc <- left_join(Bird_guilts,BirdFuncDat)

saveRDS(BirdFunc, "BirdFunc.rds")

#prim
cluster1 <-BirdFunc %>% filter(Nombre %in% c("CORMORAN", "CHINCOL","GAVIOTA","PALOMA", "ZORZAL"))
cluster2 <-BirdFunc %>% filter(Nombre %in% c("TORTOLA", "RARA", "TORDO"))

#inv
cluster1 <-BirdFunc %>% filter(Nombre %in% c("TIUQUE", "PALOMA", "CHINCOL","GAVIOTA"))
cluster2<-BirdFunc%>%filter(Nombre%in%c("CORMORAN","PELICANO COMÚN","MONJA","PIQUERO","PILPILEN","CHURRETE CHICO","PILPILEN NEGRO","GARUMA","JOTE CABEZA NEGRA"))
cluster3 <-BirdFunc %>% filter(Nombre %in% c("BLANQUILLO","CHURRETE COMUN","QUELTEHUE","COLEGIAL","GOLONDRINA DORSO NEGRO","PERRITO","LILE","CHURRETE COSTERO", "GUANAY", "FRANKLIN","TAGUA COMUN","COTORRA ARGENTINA","DIUCA","GORRION","HUAIRAVO"))
cluster4 <-BirdFunc %>% filter(Nombre %in% c("TORTOLA", "RARA", "TORDO"))

