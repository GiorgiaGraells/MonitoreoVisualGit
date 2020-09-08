
library(readxl)
library(tidyverse)
library(ggplot2)


# ABUNDANCIA: Media de sitios por ambiente y sd

## Invierno
TotalInv1 <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv") %>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))
TotalInv1 <- TotalInv1 %>% dplyr::select(AMBIENTE, Sitio, Especie, N_individuos) %>% 
    mutate(Especie= str_replace(Especie, "PELICANO", "PELICANO COMÚN"),
         Especie= str_replace(Especie, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Especie= str_replace(Especie, "LILEN", "LILE")) %>% 
  group_by(AMBIENTE,Sitio, Especie) %>% 
  summarise(N_total = mean(N_individuos))%>% ungroup() %>% 
  dplyr::filter(Especie %in% c("GARUMA","PELICANO COMÚN","CORMORAN","QUELTEHUE", "BLANQUILLO","JOTE CABEZA NEGRA", "ZORZAL","PALOMA"))

BirdNames <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdNames.csv") %>% dplyr::select(-X1)
BirdNames <- BirdNames %>%   rename(Cientifico = Especie, Especie =Nombre)

TotalInv1 <- left_join(x=TotalInv1, y= BirdNames) 

TotalInv1 <- TotalInv1 %>% 
  mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO","VERDE", "ROQUERIO INTERVENIDO","PLAYA INTERVENIDA",  "ROQUERIO NATURAL")) %>% 
  mutate(Especie=fct_relevel(Especie, "GARUMA","PELICANO COMÚN","CORMORAN","QUELTEHUE",  "BLANQUILLO","JOTE CABEZA NEGRA", "ZORZAL","PALOMA"))

ggplot(TotalInv1, aes(x=AMBIENTE, y=N_total)) + 
  geom_boxplot() + 
  facet_wrap(~Especie, ncol=4, scales = "free_y")+ theme_bw()+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))+ ylab("Número individuos")


## Primavera
AMB_nombres <-read_rds("Occdata_occu.rds") %>% dplyr::select(Sitio, AMBIENTE)

TotalPrim1 <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")
TotalPrim1 <- left_join(TotalPrim1, AMB_nombres) 

TotalPrim1 <- TotalPrim1 %>% dplyr::select(AMBIENTE, Sitio, Especie, N_individuos) %>% 
  mutate(Especie= str_replace(Especie, "PELICANO", "PELICANO COMÚN"),
         Especie= str_replace(Especie, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Especie= str_replace(Especie, "LILEN", "LILE")) %>% 
  group_by(AMBIENTE,Sitio, Especie) %>% 
  summarise(N_total = mean(N_individuos))%>% ungroup() %>% 
  dplyr::filter(Especie %in% c("ZARAPITO","GARUMA","FRANKLIN","CHURRETE COSTERO","ZORZAL","CHERCAN","PELICANO COMÚN" ,"GOLONDRINA DORSO NEGRO", "PALOMA", "GAVIOTA"))

BirdNames <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdNames.csv") %>% dplyr::select(-X1)
BirdNames <- BirdNames %>%   rename(Cientifico = Especie, Especie =Nombre)

TotalPrim1 <- left_join(x=TotalPrim1, y= BirdNames) 

TotalPrim1 <- TotalPrim1 %>% 
  mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO","VERDE", "ROQUERIO INTERVENIDO","PLAYA INTERVENIDA",  "ROQUERIO NATURAL")) %>% 
  mutate(Especie=fct_relevel(Especie, "ZARAPITO","GARUMA","FRANKLIN","CHURRETE COSTERO","PALOMA","ZORZAL","CHERCAN","PELICANO COMÚN" ,"GOLONDRINA DORSO NEGRO",  "GAVIOTA"))

ggplot(TotalPrim1, aes(x=AMBIENTE, y=N_total)) + 
  geom_boxplot() + 
  facet_wrap(~Especie, ncol=5, scales = "free_y")+ theme_bw()+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))+ ylab("Número individuos")

##############################################################################

# OCUPANCIA: Media de sitios por ambiente y sd
#
ResultadosInv <- readRDS("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/ResultadosInv.rds")

ResultadosInv <- ResultadosInv %>% mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO", "VERDE", "ROCA INTERVENIDA", "PLAYA INTERVENIDA", "PLAYA NATURAL"))
ggplot(ResultadosInv, aes(x=AMBIENTE, y=Pred))+ geom_point(aes(color = Spp)) + geom_errorbar(aes(ymax = Pred + SE, ymin = Pred - SE, color = Spp)) + ylim(c(0,1.05))+
  xlab("Ambientes")+ylab("Predicción de presencia") + facet_wrap(~Spp)+theme_classic()+ theme(axis.text.x = element_text(angle=70, vjust= 1, hjust=1), legend.position = "none")
#

## Invierno
TotalInv1 <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv") %>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))
TotalInv1 <- TotalInv1 %>% dplyr::select(AMBIENTE, Sitio, Especie, N_individuos) %>% 
  mutate(Especie= str_replace(Especie, "PELICANO", "PELICANO COMÚN"),
         Especie= str_replace(Especie, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Especie= str_replace(Especie, "LILEN", "LILE")) %>% 
  group_by(AMBIENTE,Sitio, Especie) %>% 
  summarise(N_total = mean(N_individuos))%>% ungroup() %>% 
  dplyr::filter(Especie %in% c("GARUMA","PELICANO COMÚN","CORMORAN","QUELTEHUE", "BLANQUILLO","JOTE CABEZA NEGRA", "ZORZAL","PALOMA"))

BirdNames <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdNames.csv") %>% dplyr::select(-X1)
BirdNames <- BirdNames %>%   rename(Cientifico = Especie, Especie =Nombre)

TotalInv1 <- left_join(x=TotalInv1, y= BirdNames) 

TotalInv1 <- TotalInv1 %>% 
  mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO","VERDE", "ROQUERIO INTERVENIDO","PLAYA INTERVENIDA",  "ROQUERIO NATURAL")) %>% 
  mutate(Especie=fct_relevel(Especie, "GARUMA","PELICANO COMÚN","CORMORAN","QUELTEHUE",  "BLANQUILLO","JOTE CABEZA NEGRA", "ZORZAL","PALOMA"))

ggplot(TotalInv1, aes(x=AMBIENTE, y=N_total)) + 
  geom_boxplot() + 
  facet_wrap(~Especie, ncol=4, scales = "free_y")+ theme_bw()+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))+ ylab("Número individuos")

#
ResultadosPrim <- readRDS("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/ResultadosPrim.rds")
ResultadosPrim <- ResultadosPrim %>% mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO", "VERDE", "ROCA INTERVENIDA", "PLAYA INTERVENIDA", "PLAYA NATURAL"))
ggplot(ResultadosPrim, aes(x=AMBIENTE, y=Pred))+ geom_point(aes(color = Spp)) + geom_errorbar(aes(ymax = Pred + SE, ymin = Pred - SE, color = Spp)) + ylim(c(0,1.05))+
  xlab("Ambientes")+ylab("Predicción de presencia") + facet_wrap(~Spp)+theme_classic()+ theme(axis.text.x = element_text(angle=70, vjust= 1, hjust=1), legend.position = "none")
#
## Primavera
AMB_nombres <-read_rds("Occdata_occu.rds") %>% dplyr::select(Sitio, AMBIENTE)

TotalPrim1 <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")
TotalPrim1 <- left_join(TotalPrim1, AMB_nombres) 


TotalPrim1 <- TotalPrim1 %>% dplyr::select(AMBIENTE, Sitio, Especie, N_individuos) %>% 
  mutate(Especie= str_replace(Especie, "PELICANO", "PELICANO COMÚN"),
         Especie= str_replace(Especie, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Especie= str_replace(Especie, "LILEN", "LILE")) %>% 
  group_by(AMBIENTE,Sitio, Especie) %>% 
  summarise(N_total = mean(N_individuos))%>% ungroup() %>% 
  dplyr::filter(Especie %in% c("ZARAPITO","GARUMA","FRANKLIN","CHURRETE COSTERO","ZORZAL","CHERCAN","PELICANO COMÚN" ,"GOLONDRINA DORSO NEGRO", "PALOMA", "GAVIOTA"))

BirdNames <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdNames.csv") %>% dplyr::select(-X1)
BirdNames <- BirdNames %>%   rename(Cientifico = Especie, Especie =Nombre)

TotalPrim1 <- left_join(x=TotalPrim1, y= BirdNames) 

TotalPrim1 <- TotalPrim1 %>% 
  mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO","VERDE", "ROQUERIO INTERVENIDO","PLAYA INTERVENIDA",  "ROQUERIO NATURAL")) %>% 
  mutate(Especie=fct_relevel(Especie, "ZARAPITO","GARUMA","FRANKLIN","CHURRETE COSTERO","PALOMA","ZORZAL","CHERCAN","PELICANO COMÚN" ,"GOLONDRINA DORSO NEGRO",  "GAVIOTA"))

ggplot(TotalPrim1, aes(x=AMBIENTE, y=N_total)) + 
  geom_boxplot() + 
  facet_wrap(~Especie, ncol=5, scales = "free_y")+ theme_bw()+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))+ ylab("Número individuos")
