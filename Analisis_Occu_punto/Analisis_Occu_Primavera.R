#ANALISIS OCUPANCIA PRIMAVERA/Rstudio cloud

#library(devtools)
#devtools::install_github("derek-corcoran-barrios/DiversityOccu")

library(DiversityOccupancy)
library(readr)
library(dplyr)

setwd("~/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Analisis_Occu_punto")

data_reg <-read_rds("Occdata_regPRIM.rds")

data_ocu <-read_rds("Occdata_ocu.rds")
data_ocu <- data_ocu %>% select(-Sitio)

data_det <-read_rds("Occdata_detPrim.rds") 

##

OccuPrim <- batchoccu(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=48,  form= ~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_lobos+ Presencia_pescadores ~ CobVeg + AMBIENTE+ Distancia_rio+ Distancia_Costa +Distancia_Agua+ Altura+ Distancia_construccion+ Distancia_camino, dredge=TRUE, SppNames =c("BLANQUILLO", "CACHUDITO", "CAHUIL", "CHERCAN", "CHINCOL", 
                                                                                                                                                                                                                                                                                                                                                        "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "CODORNIZ",
                                                                                                                                                                                                                                                                                                                                                        "COLEGIAL", "COMETOCINO", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "FIOFIO",  
                                                                                                                                                                                                                                                                                                                                                        "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GARZA_GRANDE","GAVIOTA", "GAVIOTIN_ELEGANTE",
                                                                                                                                                                                                                                                                                                                                                        "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION", 
                                                                                                                                                                                                                                                                                                                                                        "GUANAY",  "HUAIRAVO", "HUALA", "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", 
                                                                                                                                                                                                                                                                                                                                                        "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",  "PICAFLOR_GIGANTE",  
                                                                                                                                                                                                                                                                                                                                                        "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA", 
                                                                                                                                                                                                                                                                                                                                                        "RAYADOR",  "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",  
                                                                                                                                                                                                                                                                                                                                                        "TORTOLA",  "ZARAPITO","ZORZAL"))

## OCUPANCIA PARA LAS 6 ESPECIES DE ENCUESTAS, POR AMBIENTE, PRIMAVERA

data_reg <-read_rds("Occdata_regPRIM.rds") %>% dplyr::select(starts_with("CHINCOL"), starts_with("CORMORAN"), starts_with("PALOMA"), starts_with("PELICANO"),starts_with("ZORZAL"))

data_ocu <-read_rds("Occdata_ocu.rds")
data_ocu <- data_ocu %>% select(-Sitio)

data_det <-read_rds("Occdata_detPrim.rds") 


OccuPrim <- batchoccu(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=5,  form= ~ Temperatura +Humedad+ DirViento +RapViento+ Agua ~ AMBIENTE, dredge=TRUE, SppNames =c( "CHINCOL", "CORMORAN", "PALOMA", "PELICANO", "ZORZAL"))

