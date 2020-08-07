#Base de datos rasgos funcionales
#

library(tidyverse)
library(readr)
library(readxl)
library(taxize)


setwd("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit")

#Se toman los nombres de las 59sp totales de ambas temporadas desde el archivo Bird guilts.xlsx preparado con nombre comun y cientifico

Bird_sp <- read_excel("Bird guilts.xlsx") %>% dplyr::select(-Alimentacion)%>% dplyr::select(-Habitat)

BirdFuncDat <- read_delim("BirdFuncDat.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(Especie=Scientific) %>% mutate(Especie= str_replace(Especie, "Larus pipixcan", "Leucophaeus pipixcan"),
                                        Especie= str_replace(Especie, "Larus modestus", "Leucophaeus modestus"),
                                        Especie= str_replace(Especie, "Buteo polyosoma", "Geranoaetus polyosoma"),
                                        Especie= str_replace(Especie, "Casmerodius albus", "Ardea alba"),
                                        Especie= str_replace(Especie, "Larus maculipennis", "Chroicocephalus maculipennis"),
                                        Especie= str_replace(Especie, "Sterna elegans", "Thalasseus elegans"),
                                        Especie= str_replace(Especie, "Carduelis barbata", "Spinus barbatus"),
                                        Especie= str_replace(Especie, "Catharacta chilensis", "Stercorarius chilensis")) 
  


BirdFunc <- left_join(Bird_sp, BirdFuncDat)

saveRDS(BirdFunc, "BirdFunc.rds")
