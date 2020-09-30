 #OCUPANCIA para MONITOREO PUNTO 

#Preparación de datos para analisis"

library(readr)
library(readxl)
library(tidyverse)
library(hms)
library(dplyr)


##########################################################
#########################################################

#REGISTRO AVES: INVIERNO 2019


Reg_completo_aves <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv") %>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL"))) 
Reg_completo_aves <- Reg_completo_aves  %>% 
  mutate(Especie=case_when(Especie == "JOTE NEGRO"~"JOTE CABEZA NEGRA",
                           Especie == "LILEN"~"LILE",
                           Especie == "PELICANO"~"PELICANO COMÚN", 
                          TRUE~Especie)) %>% rename(Nombre=Especie)

Nombre_sp <- read_excel("Bird guilts.xlsx") %>% dplyr::select(Nombre, Especie) %>% mutate(Especie=str_replace_all(Especie, " ", "_"))

Reg_completo_aves <- left_join(Reg_completo_aves, Nombre_sp)
 
### Modificar data frame

SITIOS <- Reg_completo_aves %>% pull(Sitio) %>% unique() %>% sort()
Spp <- Reg_completo_aves %>% pull(Especie) %>% unique() %>% sort()

Datos <- list()     #separación de especies en listas distintas
for(i in 1:length(Spp)){
  Datos[[i]] <- Reg_completo_aves %>% filter(Especie==Spp[i])
  Datos[[i]]  <- Datos[[i]]  %>% group_by(Sitio, Dia_muestreo) %>% summarise(n = sum(N_individuos))
  
}

names(Datos) <- Spp


### Generemos data frame base


OccData <- list()

for(x in 1:length(Datos)){
  Temp <- data.frame(Dia1 = rep(0, length(SITIOS)), Dia2 = rep(0, length(SITIOS)), Dia3 = rep(0, length(SITIOS)))
  row.names(Temp) <- SITIOS
  for(i in 1:nrow(Datos[[x]])){
    Temp[row.names(Temp) == Datos[[x]]$Sitio[i], Datos[[x]]$Dia_muestreo[i]] <- Datos[[x]]$n[i]
  }
  colnames(Temp) <- paste0(Spp[x], c(1,2,3))
  OccData[[x]] <- Temp
  message(paste(Spp[x], "Ready!"))
}

OccData <- bind_cols(OccData)
row.names(OccData) <- SITIOS


## Cambiando para ocupacia/solo presencia y ausencias

OccData2 <- OccData

for(i in 1:nrow(OccData)){
  OccData2[i,OccData[i,]>0] <- 1
}

saveRDS(OccData2, "Occdata_regInv.rds")

#################################################

#REGISTRO DE AVES : PRIMAVERA 2019

Registro_aves_verano <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")
Registro_aves_verano <- Registro_aves_verano %>% 
  mutate(Especie=case_when(Especie == "CAHUIL"~ "GAVIOTA CAHUIL",
                           Especie == "PELICANO"~"PELICANO COMÚN", 
                           TRUE~Especie)) %>% rename(Nombre=Especie)

Nombre_sp <- read_excel("Bird guilts.xlsx") %>% dplyr::select(Nombre, Especie) %>% mutate(Especie=str_replace_all(Especie, " ", "_"))

Registro_aves_verano <- left_join(Registro_aves_verano, Nombre_sp)



### Modificar data frame

SITIOS <- Registro_aves_verano %>% pull(Sitio) %>% unique() %>% sort()
Spp <- Registro_aves_verano %>% pull(Especie) %>% unique() %>% sort()

Datos <- list()     #separación de especies en listas distintas
for(i in 1:length(Spp)){
  Datos[[i]] <- Registro_aves_verano %>% filter(Especie==Spp[i])
  Datos[[i]]  <- Datos[[i]]  %>% group_by(Sitio, Dia_muestreo) %>% summarise(n = sum(N_individuos))
  
}

names(Datos) <- Spp


### Generemos data frame base


OccDatav <- list()

for(x in 1:length(Datos)){
  Temp <- data.frame(Dia1 = rep(0, length(SITIOS)), Dia2 = rep(0, length(SITIOS)), Dia3 = rep(0, length(SITIOS)))
  row.names(Temp) <- SITIOS
  for(i in 1:nrow(Datos[[x]])){
    Temp[row.names(Temp) == Datos[[x]]$Sitio[i], Datos[[x]]$Dia_muestreo[i]] <- Datos[[x]]$n[i]
  }
  colnames(Temp) <- paste0(Spp[x], c(1,2,3))
  OccDatav[[x]] <- Temp
  message(paste(Spp[x], "Ready!"))
}

OccDatav <- bind_cols(OccDatav)
row.names(OccDatav) <- SITIOS

#saveRDS(OccDatav, "Occdata_regPRIM_abund.rds")

## Cambiando para ocupacia/solo presencia y ausencias

OccData2v <- OccDatav

for(i in 1:nrow(OccDatav)){
  OccData2v[i,OccDatav[i,]>0] <- 1
}

saveRDS(OccData2v, "Occdata_regPRIM.rds")




##################################################################
##################################################################

## COVARIABLES OCUPANCIA: INVIERNO Y PRIMAVERA POR IGUAL

Registro_completo_suelo <- read_excel("G:/Mi unidad/Documentos Yoryi/Documents yoryi/Doctorado Tesis/Monitoreo aves/Muestreo Aves jun-jul 2019/Cobertura_suelo/Registro_completo_suelo.xlsx")
colnames(Registro_completo_suelo) <- make.names(colnames(Registro_completo_suelo))

#altura <- read_csv("G:/Mi unidad/Documentos Yoryi/Documents yoryi/Doctorado Tesis/Monitoreo aves/Muestreo Aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves.csv") %>% group_by(Sitio) %>% summarise(Altura=mean(Altura))%>% arrange(Sitio)

cov_occu <- Registro_completo_suelo %>% group_by(Sitio) %>% mutate(CobVeg = weighted.mean(x = c(CobVeg._N, CobVeg._S, CobVeg._E, CobVeg._O), w = c(Distancia_N, Distancia_S, Distancia_E, Distancia_O))) %>% dplyr::select(Sitio, CobVeg, AMBIENTE) %>% arrange(Sitio)# %>% full_join(altura) 

##...importando Distancias de la preparacion de capas
Distancias <- readRDS("Distancias.rds")
cov_occu <- cov_occu %>% full_join(Distancias) #%>% filter(!is.na(CobVeg), !is.na(AMBIENTE), !is.na(Altura),  !is.na(Poblacion))

saveRDS(as.data.frame(cov_occu), "Occdata_ocu.rds")


### Nos aseguramos de trabajar solo con los sitios que tienen sus Covariables medidas

#OccData2 <- readRDS("Occdata_reg.rds")

#OccData2 <- OccData2[row.names(OccData2)  %in% cov_occu$Sitio,]
#saveRDS(OccData2, "Occdata_reg.rds")

########################################################################
#######################################################################

## COVARIABLE DETECCIÓN: INVIERNO 2019

cov_occu <- read_rds("Occdata_ocu.rds")
cov_det <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv") %>% 
  dplyr::select(Sitio, Observador, Fecha_muestreo, Nubosidad, Neblina, Lluvia, Hora_muestreo, Presencia_otros, Presencia_perros, Nivel_trafico, Dia_muestreo) %>% 
  distinct() %>% mutate(Hora_muestreo = as.character(Hora_muestreo)) %>% filter(Sitio %in% cov_occu$Sitio)


##Hacemos un gather dejando el sito

cov_det2 <- cov_det %>% gather(key = "Variable", value = "Valor", -Sitio, -Dia_muestreo) %>% dplyr::select(-Dia_muestreo)  %>% group_split(Variable) 


Variables <- cov_det2 %>% purrr::map(~summarise(.x, Variable = unique(Variable))) %>% purrr::map(~pull(.x, Variable)) %>% reduce(c)

cov_det3 <- list()

for(i in 1:length(cov_det2)){
  temp <- cov_det2[[i]] %>% group_split(Sitio) %>% map(~dplyr::select(.x,Valor))

  for(j in 1:length(temp)){
    temp[[j]] <- data.frame(Variables1 = temp[[j]][1,], Variables2 = temp[[j]][2,], Variables3 = temp[[j]][3,])
    colnames(temp[[j]]) <- paste0(Variables[i], c(1,2,3))
  }
  cov_det3[[i]] <- bind_rows(temp)
}

names(cov_det3) <- Variables

#Arreglado de hora y fecha en su formato para trabajar

cov_det3$Fecha_muestreo <- cov_det3$Fecha_muestreo %>% mutate_all(lubridate::dmy)
cov_det3$Hora_muestreo <- cov_det3$Hora_muestreo %>% mutate_all(hms::as.hms)

saveRDS(cov_det3, "cov_det3Inv.rds")

#######################################################
#Agregando más datos historicos descargados de https://climatologia.meteochile.gob.cl

deteccion <- read_rds("cov_det3Inv.rds")



Temperatura <- read_delim("320041_2019_Temperatura_.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>% dplyr::mutate(momento = lubridate::dmy_hms(momento), Fecha = lubridate::date(momento))

## para el primer sitio y primer día

DatosTemp <- deteccion$Fecha_muestreo %>% rename(Temperatura_1= Fecha_muestreo1,Temperatura_2= Fecha_muestreo2, Temperatura_3= Fecha_muestreo3) %>% mutate(Temperatura_1 = NA, Temperatura_2 = NA, Temperatura_3 = NA)

for(i in 1:nrow(DatosTemp)){
  for(j in 1:ncol(DatosTemp)){
    Tiempo <- lubridate::ymd_hms(paste(as.character(deteccion$Fecha_muestreo[i,j]), as.character(deteccion$Hora_muestreo[i,j])))
    DatosTemp[i,j] <- Temperatura %>% dplyr::mutate(Diferencia = abs(momento - Tiempo)) %>% filter(Diferencia == min(Diferencia)) %>% dplyr::pull(Ts_Valor) %>% mean()
  }
  message(paste("Row", i, "Ready!"))
}


deteccion$Temperatura <- DatosTemp

###variable siguiente: HUMEDAD

Humedad <- read_delim("320041_2019_Humedad_.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>% mutate(momento = lubridate::dmy_hms(momento), Fecha = lubridate::date(momento))

## para el primer sitio y primer día

DatosTemp <- deteccion$Fecha_muestreo %>% rename(Humedad_1= Fecha_muestreo1,Humedad_2= Fecha_muestreo2, Humedad_3= Fecha_muestreo3) %>% mutate(Humedad_1 = NA, Humedad_2 = NA, Humedad_3 = NA)

for(i in 1:nrow(DatosTemp)){
  for(j in 1:ncol(DatosTemp)){
    Tiempo <- lubridate::ymd_hms(paste(as.character(deteccion$Fecha_muestreo[i,j]), as.character(deteccion$Hora_muestreo[i,j])))
    DatosTemp[i,j] <- Humedad %>% dplyr::mutate(Diferencia = abs(momento - Tiempo)) %>% filter(Diferencia == min(Diferencia)) %>% dplyr::pull(HR_Valor) %>% mean()
  }
  message(paste("Row", i, "Ready!"))
}

deteccion$Humedad <- DatosTemp

###variable siguiente: DIRECCION DEL VIENTO

DirViento <- read_delim("320041_2019_Viento_.csv", ";", escape_double = FALSE, trim_ws = TRUE)%>% mutate(momento = lubridate::dmy_hms(momento), Fecha = lubridate::date(momento))
## para el primer sitio y primer día

DatosTemp <- deteccion$Fecha_muestreo %>% rename(DirViento_1= Fecha_muestreo1, DirViento_2= Fecha_muestreo2, DirViento_3= Fecha_muestreo3) %>% mutate(DirViento_1 = NA, DirViento_2 = NA, DirViento_3 = NA)

for(i in 1:nrow(DatosTemp)){
  for(j in 1:ncol(DatosTemp)){
    Tiempo <- lubridate::ymd_hms(paste(as.character(deteccion$Fecha_muestreo[i,j]), as.character(deteccion$Hora_muestreo[i,j])))
    DatosTemp[i,j] <- DirViento %>% dplyr::mutate(Diferencia = abs(momento - Tiempo)) %>% filter(Diferencia == min(Diferencia)) %>% dplyr::pull(dd_Valor) %>% mean()
  }
  message(paste("Row", i, "Ready!"))
}


deteccion$DirViento <- DatosTemp

###variable siguiente: RAPIDEZ DEL VIENTO

RapViento <- read_delim("320041_2019_Viento_.csv", ";", escape_double = FALSE, trim_ws = TRUE)%>% mutate(momento = lubridate::dmy_hms(momento), Fecha = lubridate::date(momento))

## para el primer sitio y primer día

DatosTemp <- deteccion$Fecha_muestreo %>% rename(RapViento_1= Fecha_muestreo1, RapViento_2= Fecha_muestreo2, RapViento_3= Fecha_muestreo3) %>% mutate(RapViento_1 = NA, RapViento_2 = NA, RapViento_3 = NA)

for(i in 1:nrow(DatosTemp)){
  for(j in 1:ncol(DatosTemp)){
    Tiempo <- lubridate::ymd_hms(paste(as.character(deteccion$Fecha_muestreo[i,j]), as.character(deteccion$Hora_muestreo[i,j])))
    DatosTemp[i,j] <- RapViento %>% dplyr::mutate(Diferencia = abs(momento - Tiempo)) %>% filter(Diferencia == min(Diferencia)) %>% dplyr::pull(ff_Valor) %>% mean()
  }
  message(paste("Row", i, "Ready!"))
}


deteccion$RapViento <- DatosTemp

###variable siguiente: AGUA ACUMULADA ULTIMAS 6 HORAS

Agua <- read_delim("320041_2019_Agua6Horas_.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>% mutate(momento = lubridate::dmy_hms(momento), Fecha = lubridate::date(momento))

  
## para el primer sitio y primer día

DatosTemp <- deteccion$Fecha_muestreo %>% rename(Agua_1= Fecha_muestreo1, Agua_2= Fecha_muestreo2, Agua_3= Fecha_muestreo3) %>% mutate(Agua_1 = NA, Agua_2 = NA, Agua_3 = NA)

for(i in 1:nrow(DatosTemp)){
  for(j in 1:ncol(DatosTemp)){
    Tiempo <- lubridate::ymd_hms(paste(as.character(deteccion$Fecha_muestreo[i,j]), as.character(deteccion$Hora_muestreo[i,j])))
    DatosTemp[i,j] <- Agua %>% dplyr::mutate(Diferencia = abs(momento - Tiempo)) %>% filter(Diferencia == min(Diferencia)) %>% dplyr::pull(RRR6_Valor) %>% mean()
  }
  message(paste("Row", i, "Ready!"))
}


deteccion$Agua <- DatosTemp



saveRDS(deteccion, "Occdata_detInv.rds")


##########################################################


## COVARIABLE DETECCION: PRIMAVERA 2019

cov_occu <- read_rds("Occdata_ocu.rds")
cov_det <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv") %>% 
  dplyr::select(Sitio, Observador, Fecha_muestreo, Nubosidad, Neblina, Lluvia, Hora_muestreo, Presencia_pescadores, Presencia_lobos, Presencia_perros, Nivel_trafico, Dia_muestreo) %>% 
  distinct() %>% mutate(Hora_muestreo = as.character(Hora_muestreo), Fecha_muestreo = as.character(Fecha_muestreo)) %>% 
  filter(Sitio %in% cov_occu$Sitio)

##Hacemos un gather dejando el sito

cov_det2 <- cov_det %>% gather(key = "Variable", value = "Valor", -Sitio, -Dia_muestreo) %>% dplyr::select(-Dia_muestreo)  %>% group_split(Variable) 


Variables <- cov_det2 %>% purrr::map(~summarise(.x, Variable = unique(Variable))) %>% purrr::map(~pull(.x, Variable)) %>% reduce(c)

cov_det3 <- list()

for(i in 1:length(cov_det2)){
  temp <- cov_det2[[i]] %>% group_split(Sitio) %>% map(~dplyr::select(.x,Valor))
  
  for(j in 1:length(temp)){
    temp[[j]] <- data.frame(Variables1 = temp[[j]][1,], Variables2 = temp[[j]][2,], Variables3 = temp[[j]][3,])
    colnames(temp[[j]]) <- paste0(Variables[i], c(1,2,3))
  }
  cov_det3[[i]] <- bind_rows(temp)
}

names(cov_det3) <- Variables

#Arreglado de hora y fecha en su formato para trabajar

cov_det3$Fecha_muestreo <- cov_det3$Fecha_muestreo %>% mutate_all(lubridate::ymd)
cov_det3$Hora_muestreo <- cov_det3$Hora_muestreo %>% mutate_all(hms::as.hms)


saveRDS(cov_det3, "cov_det3Prim.rds")

#######################################################
#Agregando datos historicos descargados de https://climatologia.meteochile.gob.cl
#información para cada día y hora de detección

#setwd("G:/Mi unidad/Documentos Yoryi/Documents yoryi/Doctorado Tesis/Monitoreo aves/Analisis_Occu_punto")

deteccion <- read_rds("cov_det3Prim.rds")

Temperatura <- read_delim("320041_2019_Temperatura_.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>% dplyr::mutate(momento = lubridate::dmy_hms(momento), Fecha = lubridate::date(momento))

## para el primer sitio y primer día

DatosTemp <- deteccion$Fecha_muestreo %>% rename(Temperatura_1= Fecha_muestreo1,Temperatura_2= Fecha_muestreo2, Temperatura_3= Fecha_muestreo3) %>% mutate(Temperatura_1 = NA, Temperatura_2 = NA, Temperatura_3 = NA)

for(i in 1:nrow(DatosTemp)){
  for(j in 1:ncol(DatosTemp)){
    Tiempo <- lubridate::ymd_hms(paste(as.character(deteccion$Fecha_muestreo[i,j]), as.character(deteccion$Hora_muestreo[i,j])))
    DatosTemp[i,j] <- Temperatura %>% dplyr::mutate(Diferencia = abs(momento - Tiempo)) %>% filter(Diferencia == min(Diferencia)) %>% dplyr::pull(Ts_Valor) %>% mean()
  }
  message(paste("Row", i, "Ready!"))
}


deteccion$Temperatura <- DatosTemp

###variable siguiente: HUMEDAD

Humedad <- read_delim("320041_2019_Humedad_.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>% mutate(momento = lubridate::dmy_hms(momento), Fecha = lubridate::date(momento))

## para el primer sitio y primer día

DatosTemp <- deteccion$Fecha_muestreo %>% rename(Humedad_1= Fecha_muestreo1,Humedad_2= Fecha_muestreo2, Humedad_3= Fecha_muestreo3) %>% mutate(Humedad_1 = NA, Humedad_2 = NA, Humedad_3 = NA)

for(i in 1:nrow(DatosTemp)){
  for(j in 1:ncol(DatosTemp)){
    Tiempo <- lubridate::ymd_hms(paste(as.character(deteccion$Fecha_muestreo[i,j]), as.character(deteccion$Hora_muestreo[i,j])))
    DatosTemp[i,j] <- Humedad %>% dplyr::mutate(Diferencia = abs(momento - Tiempo)) %>% filter(Diferencia == min(Diferencia)) %>% dplyr::pull(HR_Valor) %>% mean()
  }
  message(paste("Row", i, "Ready!"))
}

deteccion$Humedad <- DatosTemp

###variable siguiente: DIRECCION DEL VIENTO

DirViento <- read_delim("320041_2019_Viento_.csv", ";", escape_double = FALSE, trim_ws = TRUE)%>% mutate(momento = lubridate::dmy_hms(momento), Fecha = lubridate::date(momento))
## para el primer sitio y primer día

DatosTemp <- deteccion$Fecha_muestreo %>% rename(DirViento_1= Fecha_muestreo1, DirViento_2= Fecha_muestreo2, DirViento_3= Fecha_muestreo3) %>% mutate(DirViento_1 = NA, DirViento_2 = NA, DirViento_3 = NA)

for(i in 1:nrow(DatosTemp)){
  for(j in 1:ncol(DatosTemp)){
    Tiempo <- lubridate::ymd_hms(paste(as.character(deteccion$Fecha_muestreo[i,j]), as.character(deteccion$Hora_muestreo[i,j])))
    DatosTemp[i,j] <- DirViento %>% dplyr::mutate(Diferencia = abs(momento - Tiempo)) %>% filter(Diferencia == min(Diferencia)) %>% dplyr::pull(dd_Valor) %>% mean()
  }
  message(paste("Row", i, "Ready!"))
}


deteccion$DirViento <- DatosTemp

###variable siguiente: RAPIDEZ DEL VIENTO

RapViento <- read_delim("320041_2019_Viento_.csv", ";", escape_double = FALSE, trim_ws = TRUE)%>% mutate(momento = lubridate::dmy_hms(momento), Fecha = lubridate::date(momento))

## para el primer sitio y primer día

DatosTemp <- deteccion$Fecha_muestreo %>% rename(RapViento_1= Fecha_muestreo1, RapViento_2= Fecha_muestreo2, RapViento_3= Fecha_muestreo3) %>% mutate(RapViento_1 = NA, RapViento_2 = NA, RapViento_3 = NA)

for(i in 1:nrow(DatosTemp)){
  for(j in 1:ncol(DatosTemp)){
    Tiempo <- lubridate::ymd_hms(paste(as.character(deteccion$Fecha_muestreo[i,j]), as.character(deteccion$Hora_muestreo[i,j])))
    DatosTemp[i,j] <- RapViento %>% dplyr::mutate(Diferencia = abs(momento - Tiempo)) %>% filter(Diferencia == min(Diferencia)) %>% dplyr::pull(ff_Valor) %>% mean()
  }
  message(paste("Row", i, "Ready!"))
}


deteccion$RapViento <- DatosTemp

###variable siguiente: AGUA ACUMULADA ULTIMAS 6 HORAS

Agua <- read_delim("320041_2019_Agua6Horas_.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>% mutate(momento = lubridate::dmy_hms(momento), Fecha = lubridate::date(momento))


## para el primer sitio y primer día

DatosTemp <- deteccion$Fecha_muestreo %>% rename(Agua_1= Fecha_muestreo1, Agua_2= Fecha_muestreo2, Agua_3= Fecha_muestreo3) %>% mutate(Agua_1 = NA, Agua_2 = NA, Agua_3 = NA)

for(i in 1:nrow(DatosTemp)){
  for(j in 1:ncol(DatosTemp)){
    Tiempo <- lubridate::ymd_hms(paste(as.character(deteccion$Fecha_muestreo[i,j]), as.character(deteccion$Hora_muestreo[i,j])))
    DatosTemp[i,j] <- Agua %>% dplyr::mutate(Diferencia = abs(momento - Tiempo)) %>% filter(Diferencia == min(Diferencia)) %>% dplyr::pull(RRR6_Valor) %>% mean()
  }
  message(paste("Row", i, "Ready!"))
}


deteccion$Agua <- DatosTemp



saveRDS(deteccion, "Occdata_detPrim.rds")



