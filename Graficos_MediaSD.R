
library(readxl)
library(tidyverse)
library(ggplot2)


# RIQUEZA DE ESPECIES: Media de sitios por ambiente y sd

#PolaRRR

#invierno

TotalInv1 <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv")%>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL"))) %>% 
  dplyr::select(AMBIENTE, Sitio, Especie, N_individuos) %>% 
  group_by(AMBIENTE, Sitio, Especie) %>% 
  summarise(N_individuos = sum(N_individuos)) %>% 
  mutate(N_individuos = ifelse(N_individuos > 0, 1, 0)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Especie, values_from = N_individuos, values_fill = 0) %>% 
  pivot_longer(BLANQUILLO:DIUCA, names_to = "Especie", values_to = "Presencia") %>% 
  group_by(Especie, AMBIENTE) %>% 
  summarise(N_Sitios = sum(Presencia))%>% 
  mutate(Especie= str_replace(Especie, "PELICANO", "PELICANO COMÚN"),
         Especie= str_replace(Especie, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Especie= str_replace(Especie, "LILEN", "LILE")) %>%  
  dplyr::filter(Especie %in% c("GARUMA","PELICANO COMÚN","CORMORAN","QUELTEHUE", "BLANQUILLO","JOTE CABEZA NEGRA", "ZORZAL","PALOMA"))

TotalInv1 <- TotalInv1 %>% 
  mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO","VERDE", "PLAYA INTERVENIDA","ROQUERIO INTERVENIDO", "PLAYA NATURAL")) %>% 
  mutate(Especie=fct_relevel(Especie, "GARUMA","PELICANO COMÚN","CORMORAN","QUELTEHUE", "BLANQUILLO","JOTE CABEZA NEGRA", "ZORZAL","PALOMA"))

#700x500
ggplot(TotalInv1, aes(x = Especie, y = N_Sitios)) + 
  geom_col(aes(fill = Especie), width = 1) + 
  facet_wrap(~AMBIENTE, ncol=3) +
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "bottom") + 
  coord_polar(clip="off")+  ylab(NULL)

## Primavera
AMB_nombres <-read_rds("Occdata_occu.rds") %>% dplyr::select(Sitio, AMBIENTE)

TotalPrim1 <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")
TotalPrim1 <- left_join(TotalPrim1, AMB_nombres) 

TotalPrim1 <- TotalPrim1 %>% dplyr::select(AMBIENTE, Sitio, Especie, N_individuos) %>% 
  dplyr::select(AMBIENTE, Sitio, Especie, N_individuos) %>% 
  group_by(AMBIENTE, Sitio, Especie) %>% 
  summarise(N_individuos = sum(N_individuos)) %>% 
  mutate(N_individuos = ifelse(N_individuos > 0, 1, 0)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Especie, values_from = N_individuos, values_fill = 0) %>% 
  pivot_longer(CHINCOL:CACHUDITO, names_to = "Especie", values_to = "Presencia") %>% 
  group_by(Especie, AMBIENTE) %>% 
  summarise(N_Sitios = sum(Presencia))%>% 
  mutate(Especie= str_replace(Especie, "PELICANO", "PELICANO COMÚN"),
         Especie= str_replace(Especie, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Especie= str_replace(Especie, "LILEN", "LILE")) %>% 
  mutate(AMBIENTE=str_replace(AMBIENTE,"ROCA INTERVENIDA", "ROQUERIO INTERVENIDO"),
         AMBIENTE=str_replace(AMBIENTE,"ROCA NATURAL", "ROQUERIO NATURAL")) %>% 
  dplyr::filter(Especie %in% c("ZARAPITO","GARUMA","FRANKLIN","CHURRETE COSTERO","ZORZAL","CHERCAN","PELICANO COMÚN" ,"GOLONDRINA DORSO NEGRO"))

BirdNames <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdNames.csv") %>% dplyr::select(-X1)
BirdNames <- BirdNames %>%   rename(Cientifico = Especie, Especie =Nombre)

TotalPrim1 <- left_join(x=TotalPrim1, y= BirdNames) 

TotalPrim1 <- TotalPrim1 %>% 
  mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO","VERDE", "PLAYA INTERVENIDA","ROQUERIO INTERVENIDO", "PLAYA NATURAL")) %>% 
  mutate(Especie=fct_relevel(Especie, "ZARAPITO","GARUMA","FRANKLIN","CHURRETE COSTERO","ZORZAL","CHERCAN","PELICANO COMÚN" ,"GOLONDRINA DORSO NEGRO"))

#700x500
ggplot(TotalPrim1, aes(x = Especie, y = N_Sitios)) + 
  geom_col(aes(fill = Especie), width = 1) + 
  facet_wrap(~AMBIENTE, ncol=3) +
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "bottom") + 
  coord_polar(clip="off")+  ylab(NULL)

##############################################################################
# ABUNDANCIA: Media de sitios por ambiente y sd

## Invierno
TotalInv2 <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv") %>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))
TotalInv2 <- TotalInv2 %>% dplyr::select(AMBIENTE, Sitio, Especie, N_individuos) %>% 
  mutate(Especie= str_replace(Especie, "PELICANO", "PELICANO COMÚN"),
         Especie= str_replace(Especie, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Especie= str_replace(Especie, "LILEN", "LILE")) %>% 
  group_by(AMBIENTE,Sitio, Especie) %>% 
  summarise(N_total = mean(N_individuos))%>% ungroup() %>% 
  dplyr::filter(Especie %in% c("GARUMA","PELICANO COMÚN","CORMORAN","QUELTEHUE", "BLANQUILLO","JOTE CABEZA NEGRA", "ZORZAL","PALOMA", "GAVIOTA"))

BirdNames <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdNames.csv") %>% dplyr::select(-X1)
BirdNames <- BirdNames %>%   rename(Cientifico = Especie, Especie =Nombre)

TotalInv2 <- left_join(x=TotalInv2, y= BirdNames) 

TotalInv2 <- TotalInv2 %>% 
  mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO","VERDE", "PLAYA INTERVENIDA","ROQUERIO INTERVENIDO", "PLAYA NATURAL")) %>% 
  mutate(Especie=fct_relevel(Especie, "GARUMA","PELICANO COMÚN","CORMORAN","QUELTEHUE","PALOMA",  "BLANQUILLO","JOTE CABEZA NEGRA", "ZORZAL", "GAVIOTA"))

ggplot(TotalInv2, aes(x=AMBIENTE, y=N_total)) + 
  geom_boxplot() + 
  facet_wrap(~Especie, ncol=5, scales = "free_y")+ theme_bw()+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))+ ylab("Número individuos")


## Primavera
AMB_nombres <-read_rds("Occdata_occu.rds") %>% dplyr::select(Sitio, AMBIENTE)

TotalPrim2 <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")
TotalPrim2 <- left_join(TotalPrim2, AMB_nombres) 

TotalPrim2 <- TotalPrim2 %>% dplyr::select(AMBIENTE, Sitio, Especie, N_individuos) %>% 
  mutate(Especie= str_replace(Especie, "PELICANO", "PELICANO COMÚN"),
         Especie= str_replace(Especie, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Especie= str_replace(Especie, "LILEN", "LILE")) %>% 
  mutate(AMBIENTE=str_replace(AMBIENTE,"ROCA INTERVENIDA", "ROQUERIO INTERVENIDO"),
         AMBIENTE=str_replace(AMBIENTE,"ROCA NATURAL", "ROQUERIO NATURAL")) %>% 
  group_by(AMBIENTE,Sitio, Especie) %>% 
  summarise(N_total = mean(N_individuos))%>% ungroup() %>% 
  dplyr::filter(Especie %in% c("ZARAPITO","GARUMA","FRANKLIN","CHURRETE COSTERO","ZORZAL","CHERCAN","PELICANO COMÚN" ,"GOLONDRINA DORSO NEGRO","PALOMA","GAVIOTA"))

BirdNames <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdNames.csv") %>% dplyr::select(-X1)
BirdNames <- BirdNames %>%   rename(Cientifico = Especie, Especie =Nombre)

TotalPrim2 <- left_join(x=TotalPrim2, y= BirdNames) 

TotalPrim2 <- TotalPrim2 %>% 
  mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO","VERDE", "PLAYA INTERVENIDA","ROQUERIO INTERVENIDO", "PLAYA NATURAL")) %>% 
  mutate(Especie=fct_relevel(Especie, "ZARAPITO","GARUMA","FRANKLIN","CHURRETE COSTERO","PALOMA","ZORZAL","CHERCAN","PELICANO COMÚN" ,"GOLONDRINA DORSO NEGRO", "GAVIOTA"))

ggplot(TotalPrim2, aes(x=AMBIENTE, y=N_total)) + 
  geom_boxplot() + 
  facet_wrap(~Especie, ncol=5, scales = "free_y")+ theme_bw()+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))+ ylab("Número individuos")


############################################################

# OCUPANCIA: Media de sitios por ambiente y sd

## Invierno
BirdNames <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdNames.csv") %>% dplyr::select(-X1)
BirdNames <- BirdNames %>%   rename(Cientifico = Especie, Especie =Nombre)

RespPorSitioInv <- readRDS("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/PorSitioInv.rds")
TotalInv3 <- RespPorSitioInv %>% pivot_longer(cols=Cathartes_aura:Zonotrichia_capensis, values_to="Pred") %>% 
    rename(Cientifico=name) %>% rename(AMBIENTE=Ambiente)

TotalInv3<-left_join(x=TotalInv3, y= BirdNames) 

TotalInv3 <- TotalInv3 %>% dplyr::select(AMBIENTE,Sitio, Especie, Pred) %>% 
  mutate(Especie= str_replace(Especie, "PELICANO", "PELICANO COMÚN"),
         Especie= str_replace(Especie, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Especie= str_replace(Especie, "LILEN", "LILE")) %>%
  mutate(AMBIENTE=str_replace(AMBIENTE,"ROCA INTERVENIDA", "ROQUERIO INTERVENIDO"),
         AMBIENTE=str_replace(AMBIENTE,"ROCA NATURAL", "ROQUERIO NATURAL")) %>% 
  group_by(AMBIENTE, Sitio, Especie)%>% 
  summarise(MediaPred = mean(Pred))%>% ungroup() %>% 
  dplyr::filter(Especie %in% c("SCUA","JOTE CABEZA ROJA","DIUCA","CORMORAN", "PIQUERO","QUELTEHUE", "PALOMA", "GAVIOTA"))


TotalInv3 <-TotalInv3  %>% 
  mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO","VERDE", "PLAYA INTERVENIDA","ROQUERIO INTERVENIDO", "PLAYA NATURAL")) %>% 
  mutate(Especie=fct_relevel(Especie, "SCUA","JOTE CABEZA ROJA","DIUCA", "PALOMA","CORMORAN", "PIQUERO","QUELTEHUE", "GAVIOTA"))

ggplot(TotalInv3, aes(x=AMBIENTE, y=MediaPred)) + 
  geom_boxplot() + 
  facet_wrap(~Especie, ncol=4)+ theme_bw()+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))+ ylab("Predicción de ocupancia")


## Primavera

BirdNames <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdNames.csv") %>% dplyr::select(-X1)
BirdNames <- BirdNames %>%   rename(Cientifico = Especie, Especie =Nombre)

RespPorSitioPrim <- readRDS("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/PorSitioPrim.rds")
TotalPrim3 <- RespPorSitioPrim %>% pivot_longer(cols=Anairetes_parulus:Zonotrichia_capensis, values_to="Pred") %>% 
  rename(Cientifico=name) %>% rename(AMBIENTE=Ambiente)

TotalPrim3<-left_join(x=TotalPrim3, y= BirdNames) 

TotalPrim3 <- TotalPrim3 %>% dplyr::select(AMBIENTE,Sitio, Especie, Pred) %>% 
  mutate(Especie= str_replace(Especie, "PELICANO", "PELICANO COMÚN"),
         Especie= str_replace(Especie, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Especie= str_replace(Especie, "LILEN", "LILE")) %>%
  mutate(AMBIENTE=str_replace(AMBIENTE,"ROCA INTERVENIDA", "ROQUERIO INTERVENIDO"),
         AMBIENTE=str_replace(AMBIENTE,"ROCA NATURAL", "ROQUERIO NATURAL")) %>% 
  group_by(AMBIENTE, Sitio, Especie)%>% 
  summarise(MediaPred = mean(Pred))%>% ungroup() %>% 
  dplyr::filter(Especie %in% c("CHURRETE COMUN","CHURRETE COSTERO","FRANKLIN","CACHUDITO","MIRLO","ZARAPITO","PALOMA", "GAVIOTA"))


TotalPrim3 <-TotalPrim3  %>% 
  mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO","VERDE", "PLAYA INTERVENIDA","ROQUERIO INTERVENIDO", "PLAYA NATURAL")) %>% 
  mutate(Especie=fct_relevel(Especie, "CHURRETE COMUN","CHURRETE COSTERO","FRANKLIN","PALOMA","CACHUDITO","MIRLO","ZARAPITO", "GAVIOTA"))

ggplot(TotalPrim3, aes(x=AMBIENTE, y=MediaPred)) + 
  geom_boxplot() + 
  facet_wrap(~Especie, ncol=4)+ theme_bw()+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))+ ylab("Predicción de ocupancia")
##

