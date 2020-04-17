#Vegan -Simper

#disimilaridad segun bray-curtis... aporte de sp a cada grupo definido

library(readxl)
library(tidyverse)
library(vegan)
library(MASS)
library(ggplot2)
library(knitr)
library(parallel)

AvesInv <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/Muestreo Aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves.csv") %>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))
AvesPrim <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")
Amb<- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/Analisis_Occu_punto/Occdata_ocu.rds")


Inv <- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/Analisis_Occu_punto/Occdata_regInv.rds")
#Dissimilaridad entre grupos

Inv_dist <-vegdist(Inv, method="bray", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = FALSE)

Inv_anosim <- with(Amb, anosim(Inv_dist, AMBIENTE))
summary(Inv_anosim)
plot(Inv_anosim)


