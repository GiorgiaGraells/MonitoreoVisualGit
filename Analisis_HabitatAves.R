# ANALISIS HABITAT AVES- desde septiembre 2020
#Se utiliza la abundancia corregida por deteccion

library(readxl)
library(tidyverse)
library(vegan)
library(MASS)
library(ggplot2)
library(ggrepel)
###########################################
#preparacion de datos registro aves

AvesInv2 <- read_rds("AbundInv_Corregido.rds") 
AvesInv2 <-AvesInv2 %>% ungroup() %>% dplyr::select(-Sitio) %>% mutate_all(~replace(., is.na(.), 0)) %>% sqrt()

AvesPrim2 <- read_rds("AbundPrim_Corregido.rds")
AvesPrim2 <-AvesPrim2 %>% ungroup() %>% dplyr::select(-Sitio)%>% mutate_all(~replace(., is.na(.), 0)) %>% sqrt()

#Datos ambientales  incluidos buffers

Amb<- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Occdata_occu.rds")
Amb <- Amb %>% 
  dplyr::select(Sitio,CobVeg, AMBIENTE, Distancia_rio, Altura, `Buffer_2200_Bosque Nativo`, `Buffer_2200_Cultivos`, `Buffer_2200_Grava`, `Buffer_2200_Oceano`, `Buffer_2200_Pastizales`, `Buffer_2200_Matorrales`, `Buffer_2200_Sup impermeables`, `Buffer_2200_Suelo arenoso`, `Buffer_2200_Plantación de árboles`) %>% 
  rename( `Cobertura vegetal` =CobVeg) %>% rename( `Distancia a río` =Distancia_rio) %>% 
  rename( `Bosque nativo` =`Buffer_2200_Bosque Nativo`) %>% rename( Cultivos =`Buffer_2200_Cultivos`) %>% 
  rename( Grava =`Buffer_2200_Grava`) %>% rename( Oceano =`Buffer_2200_Oceano`) %>% 
  rename( Pastizales =`Buffer_2200_Pastizales`) %>% rename( `Matorrales` =`Buffer_2200_Matorrales`) %>% 
  rename( `Superficies impermeables` =`Buffer_2200_Sup impermeables`) %>% rename( `Suelo arenoso` =`Buffer_2200_Suelo arenoso`) %>% 
  rename( `Plantacion de arboles` =`Buffer_2200_Plantación de árboles`)

rownames(Amb) <- Amb$Sitio

Amb <- Amb %>% dplyr::select(-Sitio)


#############################################################################################################################################
# ANALISIS CON DATOS DE ABUNDANCIAS CORREGIDAS: Modificacion de funcion metaMDS para considderar abundancias de especies por sitio

##### METAMDS: Non-metric Multidimensional scaling- NMDS

#Invierno
Inv_nmds2_bray <- metaMDS(AvesInv2, autotransform = FALSE, distance="bray", k=2, try=20, trymax=100,  wascores=FALSE, trace=1, plot=FALSE) 

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
Summ_AnosimInv <-summary(Anosim_Diss_Inv2)
plot(Anosim_Diss_Inv2) ##ojo con notchs

##Primavera
Prim_nmds2_bray <- metaMDS(AvesPrim2,autotransform = FALSE, distance="bray", k=2, try=20, trymax=100, 
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

###################################################
TidyEnvFit <- function(ENVFit, alpha = 0.05){
  DF <- ENVFit$vectors$arrows %>% as.data.frame()
  DF$Factor <- rownames(DF)
  DF$Pval <-ENVFit$vectors$pvals
  DF <- DF %>% dplyr::filter(Pval < alpha)
  return(DF = DF)
}
###################################################
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

#######################
#Ordistep:

#Invierno
mod0 <- rda(AvesInv2 ~ 1, Amb)  # Model with intercept only
mod1 <- rda(AvesInv2 ~ ., Amb)  # Model with all explanatory variables

## With scope present, the default direction is "both"
mod <- ordistep(mod0, scope = formula(mod1)) ###################### Step: AvesInv2 ~ AMBIENTE + Cultivos + `Plantacion de arboles`
mod
## summary table of steps
mod$anova

#Primavera
mod0 <- rda(AvesPrim2 ~ 1, Amb)  # Model with intercept only
mod1 <- rda(AvesPrim2 ~ ., Amb)  # Model with all explanatory variables

## With scope present, the default direction is "both"
mod <- ordistep(mod0, scope = formula(mod1)) #########################  Step: AvesPrim2 ~ AMBIENTE + `Bosque nativo` 
mod
## summary table of steps
mod$anova
