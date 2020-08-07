# Ensamble aves definido por rasgos funcionales

library(ggrepel)
library(tidyverse)
library(stringr)
library(MuMIn)
library(caret)
library(readr)

setwd("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit")



#Informacion de ensambles

Todo_NMDS <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Analisis_Occu_punto/Todo_NMDS.csv")
Hull <- Todo_NMDS %>% group_by(AMBIENTE, Estacion) %>% slice(chull(MDS1, MDS2))


# Informacion rasgos funcionales
BirdFunc <- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdFunc.rds")
colnames(BirdFunc) <- make.names(colnames(BirdFunc))

Species <- read_csv("Species.csv") %>%   rename(Nombre =Especies)
Species <- full_join(x=Species, y= BirdFunc)

Species_hull <- Species %>% group_by(Diet.5Cat) %>% dplyr::filter(!is.na(MDS1), !is.na(MDS2))%>% slice(chull(MDS1, MDS2))#####


# Grafico con especies

ggplot(Todo_NMDS, aes(x = MDS1, y = MDS2)) + 
  geom_polygon(data = Hull, aes(color = AMBIENTE, fill = AMBIENTE), alpha = 0.2)  + 
  facet_wrap(~Estacion) + 
  geom_point(data = Species, aes(color=Diet.5Cat)) + 
  ggrepel::geom_text_repel(data = Species, aes(label = Nombre), size=2)+
  theme_bw()



###
ggplot(Todo_NMDS, aes(x = MDS1, y = MDS2)) + 
  geom_polygon(data = Hull, aes(color = AMBIENTE), alpha = 0.2)  + 
  facet_wrap(~Estacion) + 
  geom_point(data = Species, size=1, aes(color=Diet.5Cat)) + 
  geom_polygon(data=Species_hull,  aes(color=Diet.5Cat, lty = Diet.5Cat, fill=Diet.5Cat), alpha = 0.1)+
  ggrepel::geom_text_repel(data = Species, aes(label = Nombre), size=2)+
  theme_bw()



################################3

data_ocu <-readRDS("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Occdata_occu.rds")

PredOccuSitio <- readRDS("~/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Analisis_Occu_punto/PredOccuSitio.rds")


