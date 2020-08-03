#figura construcción para presentación

library(viridis)
library(tidyverse)

setwd("G:/Mi unidad/Documentos Yoryi/Documents yoryi/Doctorado Tesis/Monitoreo aves/Analisis_Occu_punto")
construccion <- read_rds("Capa_construccion.rds")
plot(construccion)

#es necesario cortar con mascara Valparaíso
#crop o st_crop? según clase del objeto:

class(construccion) #este es un raster, por lo q se usa crop

Borde_costa_mascara <- read_rds("Borde_costa_masc.rds")
e<- extent(Borde_costa_mascara)


const_valpo <- crop(construccion,e)
values(const_valpo)<- ifelse(is.na(values(const_valpo)),0,1)

#para igualar las resoluciones a la mejor de estas dos capas

Borde_costa_mascara<- resample(Borde_costa_mascara, const_valpo, method="ngb")

const_valpo <- const_valpo*Borde_costa_mascara

plot(const_valpo)
