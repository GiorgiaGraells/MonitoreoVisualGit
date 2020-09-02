#Comportamiento aves por ambiente

library(readxl)
library(tidyverse)
library(ggplot2)


## Invierno

ComportamientoInv <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv") %>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))
ComportamientoInv <- ComportamientoInv %>% dplyr::select(AMBIENTE, Sitio, Especie, Comportamiento) %>% 
  mutate(Comportamiento=as.character(Comportamiento),
         Comportamiento= str_replace(Comportamiento, "1", "En el agua"), #Nadando o caminando en el agua
         Comportamiento= str_replace(Comportamiento, "2", "En el suelo"), #-tierra, pasto, arena, roca
         Comportamiento= str_replace(Comportamiento, "3", "Entre el follaje"), #-en árbol o arbusto
         Comportamiento= str_replace(Comportamiento, "4", "Posado en altura"), #-poste o cable
         Comportamiento= str_replace(Comportamiento, "5", "En el aire"), #Volando o planeando
         Comportamiento= str_replace(Comportamiento, "6", "En un nido"),
         Comportamiento= str_replace(Comportamiento, "7", "A altura media"), #, roca o similar
         Comportamiento= str_replace(Comportamiento, "8", "Otro")) %>% # 8 = anidando o de lejos 
  mutate(Especie= str_replace(Especie, "PELICANO", "PELICANO COMÚN"),
         Especie= str_replace(Especie, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Especie= str_replace(Especie, "LILEN", "LILE")) %>% 
  group_by(AMBIENTE, Especie, Comportamiento) %>% summarise(Total_ind =n()) %>% ungroup() %>% dplyr::filter(Total_ind >5)


BirdNames <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdNames.csv") %>% dplyr::select(-X1)
BirdNames <- BirdNames %>%   rename(Cientifico = Especie, Especie =Nombre)

ComportamientoInv <- left_join(x=ComportamientoInv, y= BirdNames) 

ComportamientoInv <- ComportamientoInv %>% 
  mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO","VERDE", "ROQUERIO INTERVENIDO","PLAYA INTERVENIDA",  "ROQUERIO NATURAL")) %>% 
  mutate(Especie=fct_relevel(Especie, "BLANQUILLO", "CORMORAN", "GARUMA", "GAVIOTA", "PELICANO COMÚN", "QUELTEHUE", "CHINCOL", "PALOMA", "PICAFLOR", "CHERCAN", "ZORZAL"))

ggplot(ComportamientoInv) + 
  geom_col(aes(x=Especie, y=Total_ind, fill=Comportamiento)) + 
  facet_wrap(~AMBIENTE, ncol=2)+ theme_bw()+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))+ ylab("Número individuos")

## Primavera

AMB_nombres <-read_rds("Occdata_occu.rds") %>% dplyr::select(Sitio, AMBIENTE)

ComportamientoPrim <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")%>% 
  rename(Nombre =Especie)
ComportamientoPrim <- left_join(ComportamientoPrim, AMB_nombres)

ComportamientoPrim <- ComportamientoPrim %>% dplyr::select(AMBIENTE, Nombre, Comportamiento) %>% 
  mutate(Comportamiento=as.character(Comportamiento),
         Comportamiento= str_replace(Comportamiento, "1", "En el agua"), #Nadando o caminando en el agua
         Comportamiento= str_replace(Comportamiento, "2", "En el suelo"), #-tierra, pasto, arena, roca
         Comportamiento= str_replace(Comportamiento, "3", "Entre el follaje"), #-en árbol o arbusto
         Comportamiento= str_replace(Comportamiento, "4", "Posado en altura"), #-poste o cable
         Comportamiento= str_replace(Comportamiento, "5", "En el aire"), #Volando o planeando
         Comportamiento= str_replace(Comportamiento, "6", "En un nido"),
         Comportamiento= str_replace(Comportamiento, "7", "A altura media"), #, roca o similar
         Comportamiento= str_replace(Comportamiento, "8", "Otro")) %>% # 8 = anidando o de lejos 
   mutate(Nombre= str_replace(Nombre, "PELICANO", "PELICANO COMÚN"),
         Nombre= str_replace(Nombre, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Nombre= str_replace(Nombre, "LILEN", "LILE"),
         Nombre= str_replace(Nombre, "CAHUIL", "GAVIOTA CAHUIL")) %>% 
  mutate(AMBIENTE= str_replace(AMBIENTE, "ROCA INTERVENIDA", "ROQUERIO INTERVENIDO"),
         AMBIENTE= str_replace(AMBIENTE, "ROCA NATURAL", "ROQUERIO NATURAL")) %>% 
  group_by(AMBIENTE, Nombre, Comportamiento) %>% summarise(Total_ind =n()) %>% ungroup() %>% dplyr::filter(Total_ind >5) %>% 
  rename(Especie =Nombre)


BirdNames <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdNames.csv")%>% dplyr::select(-X1)%>%   
  rename(Cientifico = Especie, Especie =Nombre)


ComportamientoPrim <- left_join(x=ComportamientoPrim, y= BirdNames) 

ComportamientoPrim <- ComportamientoPrim %>% 
  mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO","VERDE", "ROQUERIO INTERVENIDO","PLAYA INTERVENIDA",  "ROQUERIO NATURAL")) #%>% 
  #mutate(Especie=fct_relevel(Especie, "BLANQUILLO", "CORMORAN", "GARUMA", "GAVIOTA", "PELICANO COMÚN", "QUELTEHUE", "CHINCOL", "PALOMA", "PICAFLOR", "CHERCAN", "ZORZAL"))

ggplot(ComportamientoPrim) + 
  geom_col(aes(x=Especie, y=Total_ind, fill=Comportamiento)) + 
  facet_wrap(~AMBIENTE, ncol=2)+ theme_bw()+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))+ ylab("Número individuos")
