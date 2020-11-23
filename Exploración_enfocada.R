#Exploracion datos monitoreo visual aves

#reorden y vegan con preguntas generales Stefan tras reunion 18 agosot 2020

library(readxl)
library(tidyverse)
library(vegan)
library(MASS)
library(ggplot2)
library(knitr)
library(ggrepel)

#####################################
# PRIMERA PARTE: RIQUEZA DE ESPECIES
#####################################

#Preparacion datos
BirdNames <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdNames.csv") %>% dplyr::select(-X1)

#Riqueza invierno
AvesInv <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv") %>% 
  dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))%>% rename(Nombre =Especie)
AvesInv <- AvesInv %>% group_by(Sitio, Nombre, AMBIENTE) %>% summarise(n=sum(N_individuos)) %>% 
  mutate(Nombre= str_replace(Nombre, "PELICANO", "PELICANO COMÚN"),
         Nombre= str_replace(Nombre, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Nombre= str_replace(Nombre, "LILEN", "LILE"))

AvesInv <- left_join(x=AvesInv, y= BirdNames) 

#Grafico riqueza especies invierno
AvesInv_riq <- AvesInv %>% group_by(Sitio, AMBIENTE, Habitat) %>% summarise(Riqueza = n())
AvesInv_riq <- AvesInv_riq %>% 
  mutate(AMBIENTE=fct_relevel(AMBIENTE, "VERDE", "URBANO", "ROQUERIO NATURAL","ROQUERIO INTERVENIDO","PLAYA NATURAL","PLAYA INTERVENIDA"))

Riqueza_Prom <- AvesInv_riq %>% group_by(AMBIENTE, Habitat) %>% summarise(Riqueza = mean(Riqueza))

ggplot(AvesInv_riq, aes(x=Sitio, y=Riqueza)) + geom_col(aes(fill=Habitat), position="dodge") + facet_wrap(~AMBIENTE, scales = "free_x", ncol=2)+
  scale_fill_manual(aesthetics = c("fill", "color"),values = c( '#5ab4ac','#d8b365')) +  theme_bw()+ ylab("Riqueza de especies invierno")+
  xlab("Sitios de muestreo")+   theme(axis.text.x=element_blank()) + geom_hline(data = Riqueza_Prom,aes(yintercept = Riqueza, color = Habitat), lty=2)


ggplot(AvesInv_riq, aes(x=Habitat, y=Riqueza)) + geom_boxplot(aes(fill=Habitat), position="dodge", notch = T) + facet_wrap(~AMBIENTE, scales = "free_x", ncol=2)+
  scale_fill_manual(aesthetics = c("fill", "color"),values = c( '#5ab4ac','#d8b365')) +  theme_bw()+ ylab("Riqueza de especies invierno")+
  xlab("Sitios de muestreo")+   theme(axis.text.x=element_blank()) + geom_hline(data = Riqueza_Prom,aes(yintercept = Riqueza, color = Habitat), lty=2)


# Riqueza primavera

AMB_nombres <-read_rds("Occdata_occu.rds") %>% dplyr::select(Sitio, AMBIENTE)

AvesPrim <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")%>% 
  rename(Nombre =Especie) 
AvesPrim <- left_join(AvesPrim, AMB_nombres)
AvesPrim <- AvesPrim %>% group_by(Sitio, Nombre, AMBIENTE) %>% summarise(n=sum(N_individuos)) %>% 
  mutate(Nombre= str_replace(Nombre, "PELICANO", "PELICANO COMÚN"),
         Nombre= str_replace(Nombre, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Nombre= str_replace(Nombre, "LILEN", "LILE"),
         Nombre= str_replace(Nombre, "CAHUIL", "GAVIOTA CAHUIL")) %>% 
  mutate(AMBIENTE= str_replace(AMBIENTE, "ROCA INTERVENIDA", "ROQUERIO INTERVENIDO"),
         AMBIENTE= str_replace(AMBIENTE, "ROCA NATURAL", "ROQUERIO NATURAL"))

AvesPrim <- left_join(x=AvesPrim, y= BirdNames) 

#Grafico riqueza especies primavera
AvesPrim_riq <- AvesPrim %>% group_by(Sitio, AMBIENTE, Habitat) %>% summarise(Riqueza = n())
AvesPrim_riq <- AvesPrim_riq %>% 
  mutate(AMBIENTE=fct_relevel(AMBIENTE, "VERDE", "URBANO", "ROQUERIO NATURAL","ROQUERIO INTERVENIDO","PLAYA NATURAL","PLAYA INTERVENIDA"))

Riqueza_Prom <- AvesPrim_riq %>% group_by(AMBIENTE, Habitat) %>% summarise(Riqueza = mean(Riqueza))


AvesRiqTotal <- bind_rows((AvesInv_riq %>% mutate(Temporada = "Invierno")),(AvesPrim_riq %>% mutate(Temporada = "Primavera"))) %>% 
  mutate(AMBIENTE = fct_relevel(AMBIENTE,"PLAYA NATURAL", "PLAYA INTERVENIDA", "ROQUERIO NATURAL", "ROQUERIO INTERVENIDO", "VERDE"))


ggplot(AvesRiqTotal, aes(x=Temporada, y=Riqueza)) + geom_boxplot(aes(fill=Habitat), position="dodge", notch = F) + facet_wrap(~AMBIENTE, scales = "free_x", ncol=2)+
  scale_fill_manual(aesthetics = c("fill", "color"),values = c( '#5ab4ac','#d8b365')) +  theme_bw()+ ylab("Riqueza de especies invierno")+
  xlab("Sitios de muestreo") + geom_hline(data = Riqueza_Prom,aes(yintercept = Riqueza, color = Habitat), lty=2)

  ggplot(AvesPrim_riq, aes(x=Sitio, y=Riqueza)) + geom_col(aes(fill=Habitat), position="dodge") + facet_wrap(~AMBIENTE, scales = "free_x", ncol=2)+
  scale_fill_manual(aesthetics = c("fill", "color"),values = c( '#5ab4ac','#d8b365')) +  theme_bw()+ ylab("Riqueza de especies primavera")+
  xlab("Sitios de muestreo")+ theme(axis.text.x=element_blank()) + geom_hline(data = Riqueza_Prom,aes(yintercept = Riqueza, color = Habitat), lty=2)




#####################################
# SEGUNDA PARTE: ORDINATION ANALISIS- para presencia sp, abundancias y prob ocupancia
#####################################

#############################################################################################################################################
# A.  Riqueza especies: modificacion a matriz, ocupando solo valores de 0 y 1
#funcion metaMDS tb estandariza las abundancias, considera solo composicion de especies

#Preparacion de datos con datos de presencia ausencia de especies por sitio
AvesInv <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv") %>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))
AvesInv <- AvesInv %>% group_by(Sitio, Especie) %>% summarise(n=sum(N_individuos)) %>% spread(key = Especie, value = n, fill=0) %>% ungroup
AvesInv <- AvesInv %>% dplyr::select(-Sitio) %>% mutate_all(~ifelse(.x > 0, 1, 0))

AvesPrim <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")
AvesPrim <- AvesPrim %>% group_by(Sitio, Especie) %>% summarise(n=sum(N_individuos)) %>% spread(key = Especie, value = n, fill=0) %>% ungroup
AvesPrim <-AvesPrim %>% dplyr::select(-Sitio) %>% mutate_all(~ifelse(.x > 0, 1, 0))

#Datos ambientales  incluidos buffers
Amb<- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Occdata_occu.rds")
Amb <- Amb %>% dplyr::select(-Sitio)%>% 
  dplyr::select(CobVeg, AMBIENTE, Distancia_rio, Altura, `Buffer_2200_Bosque Nativo`, `Buffer_2200_Cultivos`, `Buffer_2200_Grava`, `Buffer_2200_Oceano`, `Buffer_2200_Pastizales`, `Buffer_2200_Matorrales`, `Buffer_2200_Sup impermeables`, `Buffer_2200_Suelo arenoso`, `Buffer_2200_Plantación de árboles`) %>% 
  rename( `Cobertura vegetal` =CobVeg) %>% rename( `Distancia a río` =Distancia_rio) %>% 
  rename( `Bosque nativo` =`Buffer_2200_Bosque Nativo`) %>% rename( Cultivos =`Buffer_2200_Cultivos`) %>% 
  rename( Grava =`Buffer_2200_Grava`) %>% rename( Oceano =`Buffer_2200_Oceano`) %>% 
  rename( Pastizales =`Buffer_2200_Pastizales`) %>% rename( `Matorrales` =`Buffer_2200_Matorrales`) %>% 
  rename( `Superficies impermeables` =`Buffer_2200_Sup impermeables`) %>% rename( `Suelo arenoso` =`Buffer_2200_Suelo arenoso`) %>% 
  rename( `Plantacion de arboles` =`Buffer_2200_Plantación de árboles`)


##### 1. METAMDS Non-metric Multidimensional scaling- NMDS
#metaMDS: estandariza, genera matriz a partir de vegdist, genera monoMDS

### Invierno
Inv_nmds1_bray <- metaMDS(AvesInv, distance="bray", k=2, try=20, trymax=100, autotransform=FALSE, wascores=FALSE, trace=1, plot=FALSE) 
#try: num de randomizaciones para soluciones estable
#autotransform: wisconsin double standarization para datos grandes de abundancia
##### stress= 0.2101384   

plot(Inv_nmds1_bray, type = "n")
points(Inv_nmds1_bray, display = "sites") #revisar para graficar con color por grupo
ordispider(Inv_nmds1_bray, groups = Amb$AMBIENTE, col=1:6, label = FALSE)

#########################################################
#Función para sacar los datos y ocupar ggplot
TidyNMDS <- function(MDS, COVS){
  DF <- COVS
  DF2 <- MDS$points %>% as.data.frame()
  DF <- bind_cols(DF2, DF)
  return(DF = DF)
}
#########################################################

#Funcion para grafico NMDS en ggplot
Tidy_Inv_nmds1_bray <- TidyNMDS(MDS = Inv_nmds1_bray, COVS = Amb)
#plot 600x500
ggplot(Tidy_Inv_nmds1_bray)+geom_point(aes(x=MDS1, y=MDS2, color=AMBIENTE))+theme_classic()+ scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33'))

#stress 500x500
stressplot(Inv_nmds1_bray)
#buen fit con alto valor de ajuste

## test: ANOSIM
#ejemplo en PAST3: perm=9999, bray-curtis index similarity
#respuestas: meanrankwithin, meanrankbetween, R, p
# R=0 no hay dif entre grupos,  cercano a 1 mas grande las diferencias entre grupos
#pairwise: dif entre grupos

Diss_Inv1 <- vegdist(AvesInv, method="bray", binary=TRUE, diag=TRUE, upper=TRUE, na.rm = FALSE)
#binary TRUE estandariza presencia/ausencia /revisar metodo de distancia
Anosim_Diss_Inv1<-anosim(Diss_Inv1, grouping=Amb$AMBIENTE, permutations = 9999, distance = "bray", 
                   strata = NULL, parallel = getOption("mc.cores")) #tengo 4 cores en este pc
summary(Anosim_Diss_Inv1)
#plot 144x600
plot(Anosim_Diss_Inv1) ##ojo con notchs

### Primavera
Prim_nmds1_bray <- metaMDS(AvesPrim, distance="bray", k=2, try=20, trymax=100, autotransform=FALSE, wascores=FALSE, trace=1, plot=FALSE) 
#stress= 0.1947774

#plot(Prim_nmds1_bray, type = "n")
#points(Prim_nmds1_bray, display = "sites") #revisar para graficar con color por grupo
#ordispider(Prim_nmds1_bray,groups = Amb$AMBIENTE, col=1:6, label = TRUE)

#grafico con ggplot funcion nueva
Tidy_Prim_nmds1_bray <- TidyNMDS(MDS = Prim_nmds1_bray, COVS = Amb)
ggplot(Tidy_Prim_nmds1_bray)+geom_point(aes(x=MDS1, y=MDS2, color=AMBIENTE))+theme_classic()+ scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33'))

##stres
stressplot(Prim_nmds1_bray)

## Test: ANOSIM
Diss_Prim1 <- vegdist(AvesPrim, method="bray", binary=TRUE, diag=TRUE, upper=TRUE, na.rm = FALSE)

Anosim_Diss_Prim1 <-anosim(Diss_Prim1, grouping=Amb$AMBIENTE, permutations = 9999, distance = "bray", strata = NULL,
                   parallel = getOption("mc.cores"))
summary(Anosim_Diss_Prim1)
plot(Anosim_Diss_Prim1) #1400x600


##### 2. SIMPER:
#   Simper gives the contribution of each species to overall dissimilarities, but these
# are caused by variation in species abundances, and only partly by differences among groups.
#   The function displays most important species for each pair of groups. These species contribute at
# least to 70 % of the differences between groups.
#   The method gives the contribution of each species to overall dissimilarities, but these
# are caused by variation in species abundances, and only partly by differences among groups

Simper_Inv1 <- simper(AvesInv, group = Amb$AMBIENTE)
Sum <-summary(Simper_Inv1, ordered = TRUE, digits = max(3,getOption("digits") - 3))

DF <- data.frame(Comparacion_amb = names(Sum))
Comp1 <- list()
for(i in 1:nrow(DF)){
  Comp1[[i]] <- Sum[[i]] %>% mutate(Next=lag(cumsum), Especie = rownames(Sum[[i]]), Next = ifelse(is.na(Next), 0, Next)) %>% dplyr::filter(Next < 0.15) %>% dplyr::select(Especie, average, cumsum)
  Comp1[[i]]$Comparacion_amb <- DF$Comparacion_amb[i]
}

ResumenSimper_Inv1 <- Comp1 %>% reduce(bind_rows) %>% 
  mutate(Aporte_dif= round(average*100,0)) %>% 
  dplyr::select(Comparacion_amb, Especie,Aporte_dif, cumsum) 

write_csv(ResumenSimper_Inv1, "ResumenSimper_Inv1.csv")

#  
  
Simper_Prim1 <- simper(AvesPrim, group=Amb$AMBIENTE)
Sum <-summary(Simper_Prim1, ordered = TRUE, digits = max(3,getOption("digits") - 3))

DF <- data.frame(Comparacion_amb = names(Sum))
Comp1 <- list()
for(i in 1:nrow(DF)){
  Comp1[[i]] <- Sum[[i]] %>% mutate(Next=lag(cumsum), Especie = rownames(Sum[[i]]), Next = ifelse(is.na(Next), 0, Next)) %>% dplyr::filter(Next < 0.15) %>% dplyr::select(Especie, average, cumsum)
  Comp1[[i]]$Comparacion_amb <- DF$Comparacion_amb[i]
}

ResumenSimper_Prim1 <- Comp1 %>% reduce(bind_rows) %>% 
  mutate(Aporte_dif= round(average*100,0)) %>% 
  dplyr::select(Comparacion_amb, Especie,Aporte_dif, cumsum) 

write_csv(ResumenSimper_Prim1, "ResumenSimper_Prim1.csv")

##### ENVIT: Fits an Environmental Vector or Factor onto an Ordination
# data= Amb
# srt(Amb): 13 variables
# se usa el resultado del mnds, basado en bray curtis

###################################################
TidyEnvFit <- function(ENVFit, alpha = 0.05){
  DF <- ENVFit$vectors$arrows %>% as.data.frame()
  DF$Factor <- rownames(DF)
  DF$Pval <-ENVFit$vectors$pvals
  DF <- DF %>% dplyr::filter(Pval < alpha)
  return(DF = DF)
}
###################################################
EnvfitInv <- envfit(Inv_nmds1_bray~ `Cobertura vegetal`+ `Distancia a río`+Altura+ `Bosque nativo`+ Cultivos+ Grava + Oceano +Pastizales+ Matorrales+`Superficies impermeables` + `Suelo arenoso`+ `Plantacion de arboles`, data=Amb, perm=9999)

#plot(Inv_nmds1_bray)
#plot(EnvfitInv, p.max = 0.01)

Tidy_EnvfitInv <- TidyEnvFit(EnvfitInv)

library(ggplot2)
ggplot(Tidy_Inv_nmds1_bray)+
  geom_segment(data=Tidy_EnvfitInv, x = 0, y = 0, aes(xend =NMDS1 , yend = NMDS2), arrow = arrow(length = unit(0.2, "cm")))+
  geom_text_repel(data=Tidy_EnvfitInv, aes(x=NMDS1, y=NMDS2, label=Factor), seed = 10)+
  geom_point(aes(x=MDS1, y=MDS2, color=AMBIENTE))+
    theme_classic()+ scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33'))

#Direccion del vector indica hacia donde cammbia mas rapido la variable
#largo del vector es proporcional a la correlacion (fuerza del gradiente)

#poner las especies permitiria ver la respues de la especie sobre los vectore

EnvfitPrim <- envfit(Prim_nmds1_bray~ `Cobertura vegetal`+ `Distancia a río`+Altura+ `Bosque nativo`+ Cultivos+ Grava + Oceano +Pastizales+ Matorrales+`Superficies impermeables` + `Suelo arenoso`+ `Plantacion de arboles`, data=Amb, perm=9999)
Tidy_EnvfitPrim <- TidyEnvFit(EnvfitPrim)

ggplot(Tidy_Prim_nmds1_bray)+
  geom_segment(data=Tidy_EnvfitPrim, x = 0, y = 0, aes(xend =NMDS1 , yend = NMDS2), arrow = arrow(length = unit(0.2, "cm")))+
  geom_text_repel(data=Tidy_EnvfitPrim, aes(x=NMDS1, y=NMDS2, label=Factor), seed = 10)+
  geom_point(aes(x=MDS1, y=MDS2, color=AMBIENTE))+
  theme_classic()+ scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33'))


#############################################################################################################################################
# ABUNDANCIAS: Modificacion de funcion metaMDS para considderar abundancias de especies por sitio

#preparacion de datos 

AvesInv2 <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv") %>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))
AvesInv2 <- AvesInv2 %>% group_by(Sitio, Especie) %>% summarise(n=sum(N_individuos)) %>% spread(key = Especie, value = n, fill=0) %>% ungroup
AvesInv2 <- AvesInv2 %>% dplyr::select(-Sitio)

AvesPrim2 <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")
AvesPrim2 <- AvesPrim2 %>% group_by(Sitio, Especie) %>% summarise(n=sum(N_individuos)) %>% spread(key = Especie, value = n, fill=0) %>% ungroup
AvesPrim2 <-AvesPrim2 %>% dplyr::select(-Sitio) 

#Datos ambientales  incluidos buffers
Amb<- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Occdata_occu.rds")
Amb <- Amb %>% dplyr::select(-Sitio)%>% 
  dplyr::select(CobVeg, AMBIENTE, Distancia_rio, Altura, `Buffer_2200_Bosque Nativo`, `Buffer_2200_Cultivos`, `Buffer_2200_Grava`, `Buffer_2200_Oceano`, `Buffer_2200_Pastizales`, `Buffer_2200_Matorrales`, `Buffer_2200_Sup impermeables`, `Buffer_2200_Suelo arenoso`, `Buffer_2200_Plantación de árboles`) %>% 
  rename( `Cobertura vegetal` =CobVeg) %>% rename( `Distancia a río` =Distancia_rio) %>% 
  rename( `Bosque nativo` =`Buffer_2200_Bosque Nativo`) %>% rename( Cultivos =`Buffer_2200_Cultivos`) %>% 
  rename( Grava =`Buffer_2200_Grava`) %>% rename( Oceano =`Buffer_2200_Oceano`) %>% 
  rename( Pastizales =`Buffer_2200_Pastizales`) %>% rename( `Matorrales` =`Buffer_2200_Matorrales`) %>% 
  rename( `Superficies impermeables` =`Buffer_2200_Sup impermeables`) %>% rename( `Suelo arenoso` =`Buffer_2200_Suelo arenoso`) %>% 
  rename( `Plantacion de arboles` =`Buffer_2200_Plantación de árboles`)


##### METAMDS: Non-metric Multidimensional scaling- NMDS

#Invierno
Inv_nmds2_bray <- metaMDS(AvesInv2,autotransform = TRUE, distance="bray", k=2, try=20, trymax=100, 
                          wascores=FALSE, trace=1, plot=FALSE) #stress= 0.1284733

#plot(Inv_nmds2_bray, type = "n")
#points(Inv_nmds2_bray, display = "sites") #revisar para graficar con color por grupo
#ordispider(Inv_nmds2_bray,groups = Amb$AMBIENTE, col=1:6, label = FALSE)

#Funcion para grafico NMDS en ggplot
Tidy_Inv_nmds2_bray <- TidyNMDS(MDS = Inv_nmds2_bray, COVS = Amb)
ggplot(Tidy_Inv_nmds2_bray)+geom_point(aes(x=MDS1, y=MDS2, color=AMBIENTE))+theme_classic()+ scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33'))

#stress
stressplot(Inv_nmds2_bray)
#buen fit con alto valor de ajuste

## test: ANOSIM
#ejemplo en PAST3: perm=9999, bray-curtis index similarity
#respuestas: meanrankwithin, meanrankbetween, R, p
# R=0 no hay dif entre grupos,  cercano a 1 mas grande las diferencias entre grupos
#pairwise: dif entre grupos

Diss_Inv2 <- vegdist(AvesInv2, method="bray", binary=FALSE, diag=TRUE, upper=TRUE, na.rm = FALSE)
#con base de datos por abundancia

Anosim_Diss_Inv2<-anosim(Diss_Inv2, grouping=Amb$AMBIENTE, permutations = 9999, distance = "bray", 
                         strata = NULL, parallel = getOption("mc.cores")) #tengo 4 cores en este pc
summary(Anosim_Diss_Inv2)
plot(Anosim_Diss_Inv2) ##ojo con notchs


##Primavera
Prim_nmds2_bray <- metaMDS(AvesPrim2,autotransform = TRUE, distance="bray", k=2, try=20, trymax=100, 
                          wascores=FALSE, trace=1, plot=FALSE) #stress= 0.1284733

#plot(Inv_nmds2_bray, type = "n")
#points(Inv_nmds2_bray, display = "sites") #revisar para graficar con color por grupo
#ordispider(Inv_nmds2_bray,groups = Amb$AMBIENTE, col=1:6, label = FALSE)

#Funcion para grafico NMDS en ggplot
Tidy_Prim_nmds2_bray <- TidyNMDS(MDS = Prim_nmds2_bray, COVS = Amb)
ggplot(Tidy_Prim_nmds2_bray)+geom_point(aes(x=MDS1, y=MDS2, color=AMBIENTE))+theme_classic()+ scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33'))

#stress: 0.1571846 
stressplot(Prim_nmds2_bray)
#buen fit con alto valor de ajuste

## test: ANOSIM
#ejemplo en PAST3: perm=9999, bray-curtis index similarity
#respuestas: meanrankwithin, meanrankbetween, R, p
# R=0 no hay dif entre grupos,  cercano a 1 mas grande las diferencias entre grupos
#pairwise: dif entre grupos

Diss_Prim2 <- vegdist(AvesPrim2, method="bray", binary=FALSE, diag=TRUE, upper=TRUE, na.rm = FALSE)
#con base de datos por abundancia

Anosim_Diss_Prim2<-anosim(Diss_Prim2, grouping=Amb$AMBIENTE, permutations = 9999, distance = "bray", 
                         strata = NULL, parallel = getOption("mc.cores")) #tengo 4 cores en este pc
summary(Anosim_Diss_Prim2)
plot(Anosim_Diss_Prim2) ##ojo con notchs

##### SIMPER:gives the contribution of each species to overall dissimilarities, but these
#          are caused by variation in species abundances, and only partly by differences among groups.

#  The function displays most important species for each pair of groups. These species contribute at
#  least to 70 % of the differences between groups.

Simper_Inv2 <- simper(AvesInv2, group = Amb$AMBIENTE)
Sum <-summary(Simper_Inv2, ordered = TRUE, digits = max(3,getOption("digits") - 3))

DF <- data.frame(Comparacion_amb = names(Sum))
Comp1 <- list()
for(i in 1:nrow(DF)){
  Comp1[[i]] <- Sum[[i]] %>% mutate(Next=lag(cumsum), Especie = rownames(Sum[[i]]), Next = ifelse(is.na(Next), 0, Next)) %>% dplyr::filter(Next < 0.15) %>% dplyr::select(Especie, average, cumsum)
  Comp1[[i]]$Comparacion_amb <- DF$Comparacion_amb[i]
}

ResumenSimper_Inv2 <- Comp1 %>% reduce(bind_rows) %>% 
  mutate(Aporte_dif= round(average*100,0)) %>% 
  dplyr::select(Comparacion_amb, Especie,Aporte_dif, cumsum)

write_csv(ResumenSimper_Inv2, "ResumenSimper_Inv2.csv")

#  

Simper_Prim2 <- simper(AvesPrim2, group=Amb$AMBIENTE)
Sum <-summary(Simper_Prim2,ordered = TRUE, digits = max(3,getOption("digits") - 3))

DF <- data.frame(Comparacion_amb = names(Sum))
Comp1 <- list()
for(i in 1:nrow(DF)){
  Comp1[[i]] <- Sum[[i]] %>% mutate(Next=lag(cumsum), Especie = rownames(Sum[[i]]), Next = ifelse(is.na(Next), 0, Next)) %>% dplyr::filter(Next < 0.15) %>% dplyr::select(Especie, average, cumsum)
  Comp1[[i]]$Comparacion_amb <- DF$Comparacion_amb[i]
}

ResumenSimper_Prim2 <- Comp1 %>% reduce(bind_rows) %>% 
  mutate(Aporte_dif= round(average*100,0)) %>% 
  dplyr::select(Comparacion_amb, Especie,Aporte_dif, cumsum) 

write_csv(ResumenSimper_Prim2, "ResumenSimper_Prim2.csv")

##### ENVFIT: Fits an Environmental Vector or Factor onto an Ordination

EnvfitInv2 <- envfit(Inv_nmds2_bray~ `Cobertura vegetal`+ `Distancia a río`+Altura+ `Bosque nativo`+ Cultivos+ Grava + Oceano +Pastizales+ Matorrales+`Superficies impermeables` + `Suelo arenoso`+ `Plantacion de arboles`, data=Amb, perm=9999)
Tidy_EnvfitInv2 <- TidyEnvFit(EnvfitInv2)

ggplot(Tidy_Inv_nmds2_bray)+
  geom_segment(data=Tidy_EnvfitInv2, x = 0, y = 0, aes(xend =NMDS1 , yend = NMDS2), arrow = arrow(length = unit(0.2, "cm")))+
  geom_text_repel(data=Tidy_EnvfitInv2, aes(x=NMDS1, y=NMDS2, label=Factor), seed = 10)+
  geom_point(aes(x=MDS1, y=MDS2, color=AMBIENTE))+
  theme_classic()+ scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33'))


EnvfitPrim2 <- envfit(Prim_nmds2_bray~ `Cobertura vegetal`+ `Distancia a río`+Altura+ `Bosque nativo`+ Cultivos+ Grava + Oceano +Pastizales+ Matorrales+`Superficies impermeables` + `Suelo arenoso`+ `Plantacion de arboles`, data=Amb, perm=9999)
Tidy_EnvfitPrim2 <- TidyEnvFit(EnvfitPrim2)

ggplot(Tidy_Prim_nmds2_bray)+
  geom_segment(data=Tidy_EnvfitPrim2, x = 0, y = 0, aes(xend =NMDS1 , yend = NMDS2), arrow = arrow(length = unit(0.2, "cm")))+
  geom_text_repel(data=Tidy_EnvfitPrim2, aes(x=NMDS1, y=NMDS2, label=Factor), seed = 10)+
  geom_point(aes(x=MDS1, y=MDS2, color=AMBIENTE))+
  theme_classic()+ scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33'))



##############EXTRA EXTRA
data(varespec)
data(varechem)
sol <- bioenv(wisconsin(varespec) ~ log(N) + P + K + Ca + pH + Al, varechem)
sol
summary(sol)


Bioenv1prim <- bioenv(wisconsin(AvesPrim2)~ `Cobertura vegetal`+ `Distancia a río`+Altura+ `Bosque nativo`+ Cultivos+ Grava + Oceano +Pastizales+ Matorrales+`Superficies impermeables` + `Suelo arenoso`+ `Plantacion de arboles`, Amb)
summary(Bioenv1prim)


#############################################################################################################################################
# OCUPANCIAS: ocupando probabilidad de presencia por sitio

#preparacion de datos

PorSitioInv <- readRDS("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/PorSitioInv.rds") %>% 
  dplyr::select(-Ambiente) %>% dplyr::select(-Sitio)
PorSitioPrim <- readRDS("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/PorSitioPrim.rds") %>%
  dplyr::select(-Ambiente) %>% dplyr::select(-Sitio)


##### METAMDS: Non-metric Multidimensional scaling- NMDS

#Invierno
Inv_nmds3_bray <- metaMDS(PorSitioInv,autotransform = FALSE, distance="bray", k=2, try=20, trymax=100, 
                          wascores=FALSE, trace=1, plot=FALSE) #stress= 0.1284733


#plot(Inv_nmds2_bray, type = "n")
#points(Inv_nmds2_bray, display = "sites") #revisar para graficar con color por grupo
#ordispider(Inv_nmds2_bray,groups = Amb$AMBIENTE, col=1:6, label = FALSE)

#Funcion para grafico NMDS en ggplot
Tidy_Inv_nmds3_bray <- TidyNMDS(MDS = Inv_nmds3_bray, COVS = Amb)
ggplot(Tidy_Inv_nmds3_bray)+geom_point(aes(x=MDS1, y=MDS2, color=AMBIENTE))+theme_classic()+ scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33'))

#stress
stressplot(Inv_nmds3_bray)
#buen fit con alto valor de ajuste

## test: ANOSIM
#ejemplo en PAST3: perm=9999, bray-curtis index similarity
#respuestas: meanrankwithin, meanrankbetween, R, p
# R=0 no hay dif entre grupos,  cercano a 1 mas grande las diferencias entre grupos
#pairwise: dif entre grupos

Diss_Inv3 <- vegdist(PorSitioInv, method="bray", binary=FALSE, diag=TRUE, upper=TRUE, na.rm = FALSE)
#con base de datos por abundancia

Anosim_Diss_Inv3<-anosim(Diss_Inv3, grouping=Amb$AMBIENTE, permutations = 9999, distance = "bray", 
                         strata = NULL, parallel = getOption("mc.cores")) #tengo 4 cores en este pc
summary(Anosim_Diss_Inv3)
plot(Anosim_Diss_Inv3) ##ojo con notchs


##Primavera
Prim_nmds3_bray <- metaMDS(PorSitioPrim,autotransform = FALSE, distance="bray", k=2, try=20, trymax=100, 
                          wascores=FALSE, trace=1, plot=FALSE) #stress= 0.1284733


#plot(Inv_nmds2_bray, type = "n")
#points(Inv_nmds2_bray, display = "sites") #revisar para graficar con color por grupo
#ordispider(Inv_nmds2_bray,groups = Amb$AMBIENTE, col=1:6, label = FALSE)

#Funcion para grafico NMDS en ggplot
Tidy_Prim_nmds3_bray <- TidyNMDS(MDS = Prim_nmds3_bray, COVS = Amb)
ggplot(Tidy_Prim_nmds3_bray)+geom_point(aes(x=MDS1, y=MDS2, color=AMBIENTE))+theme_classic()+ scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33'))

#stress
stressplot(Prim_nmds3_bray)
#buen fit con alto valor de ajuste

## test: ANOSIM
#ejemplo en PAST3: perm=9999, bray-curtis index similarity
#respuestas: meanrankwithin, meanrankbetween, R, p
# R=0 no hay dif entre grupos,  cercano a 1 mas grande las diferencias entre grupos
#pairwise: dif entre grupos

Diss_Prim3 <- vegdist(PorSitioPrim, method="bray", binary=FALSE, diag=TRUE, upper=TRUE, na.rm = FALSE)
#con base de datos por abundancia

Anosim_Diss_Prim3<-anosim(Diss_Prim3, grouping=Amb$AMBIENTE, permutations = 9999, distance = "bray", 
                         strata = NULL, parallel = getOption("mc.cores")) #tengo 4 cores en este pc
summary(Anosim_Diss_Prim3)
plot(Anosim_Diss_Prim3) ##ojo con notchs



##### SIMPER:gives the contribution of each species to overall dissimilarities, but these
#          are caused by variation in species abundances, and only partly by differences among groups.

#  The function displays most important species for each pair of groups. These species contribute at
#  least to 70 % of the differences between groups.

Simper_Inv3 <- simper(PorSitioInv, group = Amb$AMBIENTE)
Sum <-summary(Simper_Inv3, ordered = TRUE, digits = max(3,getOption("digits") - 3))

DF <- data.frame(Comparacion_amb = names(Sum))
Comp1 <- list()
for(i in 1:nrow(DF)){
  Comp1[[i]] <- Sum[[i]] %>% mutate(Next=lag(cumsum), Especie = rownames(Sum[[i]]), Next = ifelse(is.na(Next), 0, Next)) %>% dplyr::filter(Next < 0.15) %>% dplyr::select(Especie, average, cumsum)
  Comp1[[i]]$Comparacion_amb <- DF$Comparacion_amb[i]
}

ResumenSimper_Inv3 <- Comp1 %>% reduce(bind_rows) %>% 
  mutate(Aporte_dif= round(average*100,0)) %>% 
  dplyr::select(Comparacion_amb, Especie,Aporte_dif, cumsum) %>% left_join(BirdNames)

write_csv(ResumenSimper_Inv3, "ResumenSimper_Inv3.csv")

#  

Simper_Prim3 <- simper(PorSitioPrim, group=Amb$AMBIENTE)
Sum <-summary(Simper_Prim3)

DF <- data.frame(Comparacion_amb = names(Sum))
Comp1 <- list()
for(i in 1:nrow(DF)){
  Comp1[[i]] <- Sum[[i]] %>% mutate(Next=lag(cumsum), Especie = rownames(Sum[[i]]), Next = ifelse(is.na(Next), 0, Next)) %>% dplyr::filter(Next < 0.15) %>% dplyr::select(Especie, average, cumsum)
  Comp1[[i]]$Comparacion_amb <- DF$Comparacion_amb[i]
}

ResumenSimper_Prim3 <- Comp1 %>% reduce(bind_rows) %>% 
  mutate(Aporte_dif= round(average*100,0)) %>% 
  dplyr::select(Comparacion_amb, Especie,Aporte_dif, cumsum)  %>% left_join(BirdNames)

write_csv(ResumenSimper_Prim3, "ResumenSimper_Prim3.csv")


##### ENVFIT: Fits an Environmental Vector or Factor onto an Ordination

#EnvfitInv <- envfit(ord=ord3Inv, env=Amb, permutations = 999, strata = NULL)

EnvfitInv3 <- envfit(Inv_nmds3_bray~ `Cobertura vegetal`+ `Distancia a río`+Altura+ `Bosque nativo`+ Cultivos+ Grava + Oceano +Pastizales+ Matorrales+`Superficies impermeables` + `Suelo arenoso`+ `Plantacion de arboles`, data=Amb, perm=9999)
Tidy_EnvfitInv3 <- TidyEnvFit(EnvfitInv3)

ggplot(Tidy_Inv_nmds3_bray)+
  geom_segment(data=Tidy_EnvfitInv3, x = 0, y = 0, aes(xend =NMDS1 , yend = NMDS2), arrow = arrow(length = unit(0.2, "cm")))+
  geom_text_repel(data=Tidy_EnvfitInv3, aes(x=NMDS1, y=NMDS2, label=Factor), seed = 10)+
  geom_point(aes(x=MDS1, y=MDS2, color=AMBIENTE))+
  theme_classic()+ scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33'))


EnvfitPrim3 <- envfit(Prim_nmds3_bray~ `Cobertura vegetal`+ `Distancia a río`+Altura+ `Bosque nativo`+ Cultivos+ Grava + Oceano +Pastizales+ Matorrales+`Superficies impermeables` + `Suelo arenoso`+ `Plantacion de arboles`, data=Amb, perm=9999)
Tidy_EnvfitPrim3 <- TidyEnvFit(EnvfitPrim3)

ggplot(Tidy_Prim_nmds3_bray)+
  geom_segment(data=Tidy_EnvfitPrim3, x = 0, y = 0, aes(xend =NMDS1 , yend = NMDS2), arrow = arrow(length = unit(0.2, "cm")))+
  geom_text_repel(data=Tidy_EnvfitPrim3, aes(x=NMDS1, y=NMDS2, label=Factor), seed = 10)+
  geom_point(aes(x=MDS1, y=MDS2, color=AMBIENTE))+
  theme_classic()+ scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33'))

