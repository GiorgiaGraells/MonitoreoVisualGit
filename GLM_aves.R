# Verificar diferencias no estacionales para aves considderando especies
#mediante GLMmixto x estación

library(tidyverse)
library(lme4)

Inv  <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv")%>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))
Inv <- Inv %>% dplyr::select(AMBIENTE, Sitio, Especie, Dia_muestreo, N_individuos) %>%
  mutate(Especie= str_replace(Especie, "PELICANO", "PELICANO COMÚN"),
         Especie= str_replace(Especie, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Especie= str_replace(Especie, "LILEN", "LILE")) %>% 
  group_by(AMBIENTE, Sitio, Especie, Dia_muestreo) %>% 
  summarise(Abundancia = sum(N_individuos)) %>% 
  mutate(Estacion="Invierno")

AMB_nombres <-read_rds("Occdata_occu.rds") %>% dplyr::select(Sitio, AMBIENTE)
Prim <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")
Prim <- left_join(Prim, AMB_nombres) 
Prim <- Prim %>% dplyr::select(AMBIENTE, Sitio, Especie, Dia_muestreo, N_individuos) %>% 
  mutate(Especie= str_replace(Especie, "PELICANO", "PELICANO COMÚN"),
         Especie= str_replace(Especie, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Especie= str_replace(Especie, "LILEN", "LILE")) %>% 
  mutate(AMBIENTE=str_replace(AMBIENTE,"ROCA INTERVENIDA", "ROQUERIO INTERVENIDO"),
         AMBIENTE=str_replace(AMBIENTE,"ROCA NATURAL", "ROQUERIO NATURAL")) %>% 
  group_by(AMBIENTE, Sitio, Especie, Dia_muestreo) %>% 
  summarise(Abundancia = sum(N_individuos)) %>% 
  mutate(Estacion="Primavera")

# 1 solo DF
Aves <- bind_rows(Inv, Prim)

# Modelos a comparar, con y sin temporada de muestreo

fit1 <- glmer(Abundancia ~ Estacion *Especie + AMBIENTE+ (1|Sitio), family = poisson, data =Aves ) #AIC 17716.1
fit2 <- glmer(Abundancia ~ Especie + AMBIENTE+ (1|Sitio), family = poisson, data =Aves )           #AIC 18241.0

