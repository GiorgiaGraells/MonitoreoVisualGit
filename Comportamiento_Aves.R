#Comportamiento aves por ambiente
# Comportamiento oactividades registradas de aves en terreno

library(readxl)
library(tidyverse)
library(ggplot2)


## Invierno
# Winter: Pelecanus thagus, *Larus dominicanus*, *Coragyps atratus*, *Larosterna inca*, *Leucophaeus modestus*, *Columba livia*, *Turdus falcklandii*, and Sephanoides sephaniodes. 

ComportamientoInv <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv") %>% 
  dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL"))) %>% 
  mutate(Especie= str_replace(Especie, "PELICANO", "PELICANO COMÚN"),
         Especie= str_replace(Especie, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Especie= str_replace(Especie, "LILEN", "LILE")) %>% 
  dplyr::filter(Especie %in% c("PELICANO COMÚN","GAVIOTA", "JOTE CABEZA NEGRA","MONJA", "GARUMA", "PALOMA","ZORZAL", "PICAFLOR")) %>% 
  dplyr::select(AMBIENTE, Especie, Comportamiento) 
  
#Cambio al inglés
ComportamientoInv <- ComportamientoInv %>% 
    mutate(Comportamiento=as.character(Comportamiento),
         Comportamiento= str_replace(Comportamiento, "1", "On the water"), #Nadando o caminando en el agua
         Comportamiento= str_replace(Comportamiento, "2", "Walking"), #-tierra, pasto, arena, roca
         Comportamiento= str_replace(Comportamiento, "3", "In the foliage"), #-en árbol o arbusto
         Comportamiento= str_replace(Comportamiento, "4", "Perching"), #-poste o cable
         Comportamiento= str_replace(Comportamiento, "5", "Flying"), #Volando o planeando
         Comportamiento= str_replace(Comportamiento, "6", "Nesting"),
         Comportamiento= str_replace(Comportamiento, "7", "Standing over a thing"), #, roca o similar
         Comportamiento= str_replace(Comportamiento, "8", "Other")) %>% 
 mutate(AMBIENTE= str_replace(AMBIENTE, "URBANO", "Urban"),
        AMBIENTE= str_replace(AMBIENTE, "VERDE", "Green area"),
        AMBIENTE= str_replace(AMBIENTE, "ROQUERIO INTERVENIDO", "Modified rocky shore"),
        AMBIENTE= str_replace(AMBIENTE, "PLAYA INTERVENIDA", "Urbanized beach"),
        AMBIENTE= str_replace(AMBIENTE, "ROQUERIO NATURAL", "Natural rocky shore"),
        AMBIENTE= str_replace(AMBIENTE,"PLAYA NATURAL", "Natural beach")) %>% 
  group_by( Especie, AMBIENTE, Comportamiento) %>% summarise(Total_ind =n()) %>% ungroup()



BirdNames <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdNames.csv") %>% dplyr::select(-X1)
BirdNames <- BirdNames %>%   rename(Cientifico = Especie, Especie =Nombre) %>% dplyr::select(-Habitat)

ComportamientoInv <- left_join(x=ComportamientoInv, y= BirdNames) 

ComportamientoInv <- ComportamientoInv %>% 
 mutate(AMBIENTE=fct_relevel(AMBIENTE, "Urban", "Green area", "Modified rocky shore", "Urbanized beach", "Natural rocky shore")) %>% 
  mutate(Cientifico=fct_relevel(Cientifico, "Leucophaeus_modestus","Larus_dominicanus","Coragyps_atratus","Larosterna_inca","Columba_livia","Pelecanus_thagus",       
                                 "Sephanoides_sephaniodes", "Turdus_falcklandii" ))

saveRDS(ComportamientoInv, "ComportamientoInv.rds")

ggplot(ComportamientoInv) + 
  geom_col(aes(x=AMBIENTE, y=Total_ind, fill=Comportamiento), color="black") + theme_classic()+ facet_wrap(~Cientifico, ncol=4, scales = "free_y")+  
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))+ ylab("Número individuos") +xlab(" ")+
  scale_fill_manual(aesthetics = "fill", values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c'), name="Bird behavior")

## Primavera
# Spring: *Leucophaeus pipixcan*, *Larus dominicanus*, *Columba livia*, *Larosterna inca*, *Phalacrocorax bougainvillii*, and *Pelecanus thagus*, 

ComportamientoPrim <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")%>% 
  rename(Nombre =Especie)
AMB_nombres <-read_rds("Occdata_occu.rds") %>% dplyr::select(Sitio, AMBIENTE)
ComportamientoPrim <- left_join(ComportamientoPrim, AMB_nombres)

ComportamientoPrim <- ComportamientoPrim %>% 
  dplyr::select(AMBIENTE, Nombre, Comportamiento) %>% 
  mutate(Nombre= str_replace(Nombre, "PELICANO", "PELICANO COMÚN"),
         Nombre= str_replace(Nombre, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Nombre= str_replace(Nombre, "LILEN", "LILE"),
         Nombre= str_replace(Nombre, "CAHUIL", "GAVIOTA CAHUIL"))  %>% 
  dplyr::filter(Nombre %in% c("FRANKLIN","GAVIOTA","PALOMA","MONJA","GUANAY","PELICANO COMÚN"))%>% 
  rename(Especie =Nombre)

ComportamientoPrim <- ComportamientoPrim %>% 
  mutate(Comportamiento=as.character(Comportamiento),
         Comportamiento= str_replace(Comportamiento, "1", "On the water"), #Nadando o caminando en el agua
         Comportamiento= str_replace(Comportamiento, "2", "Walking"), #-tierra, pasto, arena, roca
         Comportamiento= str_replace(Comportamiento, "3", "In the foliage"), #-en árbol o arbusto
         Comportamiento= str_replace(Comportamiento, "4", "Perching"), #-poste o cable
         Comportamiento= str_replace(Comportamiento, "5", "Flying"), #Volando o planeando
         Comportamiento= str_replace(Comportamiento, "6", "Nesting"),
         Comportamiento= str_replace(Comportamiento, "7", "Standing over a thing"), #, roca o similar
         Comportamiento= str_replace(Comportamiento, "8", "Other")) %>% 
  mutate(AMBIENTE= str_replace(AMBIENTE, "URBANO", "Urban"),
         AMBIENTE= str_replace(AMBIENTE, "VERDE", "Green area"),
         AMBIENTE= str_replace(AMBIENTE, "ROCA INTERVENIDA", "Modified rocky shore"),
         AMBIENTE= str_replace(AMBIENTE, "PLAYA INTERVENIDA", "Urbanized beach"),
         AMBIENTE= str_replace(AMBIENTE, "ROCA NATURAL", "Natural rocky shore"),
         AMBIENTE= str_replace(AMBIENTE,"PLAYA NATURAL", "Natural beach")) %>% 
  group_by( Especie, AMBIENTE, Comportamiento) %>% summarise(Total_ind =n()) %>% ungroup()
  

BirdNames <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdNames.csv") %>% dplyr::select(-X1)
BirdNames <- BirdNames %>%   rename(Cientifico = Especie, Especie =Nombre) %>% dplyr::select(-Habitat)

ComportamientoPrim <- left_join(x=ComportamientoPrim, y= BirdNames) 

ComportamientoPrim <- ComportamientoPrim %>% 
  mutate(AMBIENTE=fct_relevel(AMBIENTE, "Urban", "Green area", "Modified rocky shore", "Urbanized beach", "Natural rocky shore")) #%>% 
  #mutate(Cientifico=fct_relevel(Cientifico, "Leucophaeus_modestus","Larus_dominicanus","Coragyps_atratus","Larosterna_inca","Columba_livia","Pelecanus_thagus",       
         #                       "Sephanoides_sephaniodes", "Turdus_falcklandii" ))

saveRDS(ComportamientoPrim, "ComportamientoPrim.rds")

ggplot(ComportamientoPrim) + 
  geom_col(aes(x=AMBIENTE, y=Total_ind, fill=Comportamiento), color="black") + theme_classic()+ facet_wrap(~Cientifico, ncol=3, scales = "free_y")+  
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))+ ylab("Número individuos") +xlab(" ")+
  scale_fill_manual(aesthetics = "fill", values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00'), name="Bird behavior")
    
