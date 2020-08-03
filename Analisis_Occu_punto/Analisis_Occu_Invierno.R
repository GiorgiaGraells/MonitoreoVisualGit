#ANALISIS DE OCUPANCIA / Rstudio cloud

#devtools::install_github("derek-corcoran-barrios/DiversityOccu")

library(DiversityOccupancy)
library(readr)
library(dplyr)


#Invierno

data_reg <-read_rds("Occdata_regInv.rds")

data_ocu <-read_rds("Occdata_ocu.rds")
data_ocu <- data_ocu %>% dplyr::select(-Sitio)

data_det <-read_rds("Occdata_detInv.rds") 



###


Occu <- batchoccu(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=45,  form= ~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_otros ~ CobVeg + AMBIENTE+ Distancia_rio+ Distancia_Costa +Distancia_Agua+ Altura+ Distancia_construccion+ Distancia_camino, dredge=TRUE, SppNames =c("AGUILUCHO", "BLANQUILLO", "CHERCAN", "CHINCOL", 
                                                                                                                                                                                                                                                                                    "CHURRETE_CHICO",  "CHURRETE_COMUN", "CHURRETE_COSTERO", 
                                                                                                                                                                                                                                                                                   "COLEGIAL",  "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "DIUCON",  
                                                                                                                                                                                                                                                                                    "DORMILONA_CABEZA_NEGRA","GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GARZA_CHICA","GAVIOTA", 
                                                                                                                                                                                                                                                                                    "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION", 
                                                                                                                                                                                                                                                                                    "GUANAY",  "HUAIRAVO", "JILGUERO",  "JOTE_CABEZA_ROJA", 
                                                                                                                                                                                                                                                                                    "JOTE_CABEZA_NEGRA",  "LILEN",  "LOICA",  "MIRLO", 
                                                                                                                                                                                                                                                                                    "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",  "PICAFLOR",  
                                                                                                                                                                                                                                                                                    "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA", 
                                                                                                                                                                                                                                                                                    "SCUA",  "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",  
                                                                                                                                                                                                                                                                                    "TORTOLA",  "ZORZAL"))
##OCUPANCIA PARA LAS 6 ESPECIES DE ENCUESTAS, POR AMBIENTE, PRIMAVERA

data_reg <-read_rds("Occdata_regInv.rds") %>% dplyr::select(starts_with("CHINCOL"), starts_with("CORMORAN"), starts_with("PALOMA"), starts_with("PELICANO"),starts_with("ZORZAL"))

data_ocu <-read_rds("Occdata_ocu.rds")
data_ocu <- data_ocu %>% dplyr::select(-Sitio)

data_det <-read_rds("Occdata_detInv.rds") 

Occu <- batchoccu(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=5,  form= ~ Temperatura +Humedad+ DirViento +RapViento+ Agua ~ AMBIENTE, dredge=TRUE, SppNames =c("CHINCOL", "CORMORAN", "PALOMA", "PELICANO", "ZORZAL"))





#####
data_reg2 <- data_reg %>% dplyr::select(contains("GAVIOTA"), contains("CHINCOL"), contains("PALOMA"))
Occu3sp <- batchoccu(pres = data_reg2, sitecov = data_ocu, obscov = data_det, spp=3,  form= ~ Temperatura +Humedad+ DirViento +RapViento+Agua ~ CobVeg + AMBIENTE +Distancia_Costa + Altura, dredge=TRUE, SppNames =c( "GAVIOTA", "CHINCOL", "PALOMA"))


data_reg3 <- data_reg %>% dplyr::select(contains("GAVIOTA"), contains("CHINCOL"))
Occu2sp <- batchoccu(pres = data_reg3, sitecov = data_ocu, obscov = data_det, spp=2,  form= ~ Temperatura +Humedad ~ CobVeg + Distancia_Costa , dredge=TRUE, SppNames =c( "GAVIOTA", "CHINCOL"))

Occugav <- batchoccu(pres = data_reg3, sitecov = data_ocu, obscov = data_det, spp=2,  form= ~ 1 ~ CobVeg , dredge=TRUE, SppNames =c( "GAVIOTA", "CHINCOL"))

Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+Agua


data_gav <- data_reg %>% dplyr::select(contains("GAVIOTA"))


#archivo guardado en wd, nombre: Ocupancia45_avesInv.rds
Occu45 <-read_rds("Ocupancia45_avesInv.rds") 



