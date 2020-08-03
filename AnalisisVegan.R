#Vegan 

#Exploración datos y analisis preliiminares
#PAQUETE VEGAN

library(readxl)
library(tidyverse)
library(vegan)
library(MASS)
library(ggplot2)
library(knitr)

AvesInv <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/Muestreo Aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves.csv") %>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))
AvesPrim <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")

###### Modificando data frame

AvesInv <- AvesInv %>% group_by(Sitio, Especie) %>% summarise(n=sum(N_individuos)) %>% spread(key = Especie, value = n, fill=0) %>% ungroup
rownames(AvesInv) <-AvesInv$Sitio
AvesInv <- AvesInv %>% dplyr::select(-Sitio)

AvesPrim <- AvesPrim %>% group_by(Sitio, Especie) %>% summarise(n=sum(N_individuos)) %>% spread(key = Especie, value = n, fill=0) %>% ungroup
rownames(AvesPrim) <-AvesPrim$Sitio
AvesPrim <- AvesPrim %>% dplyr::select(-Sitio)

Amb<- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/Analisis_Occu_punto/Occdata_ocu.rds")
Sitio_Amb <- Amb %>% dplyr::select(Sitio, AMBIENTE)

#Inv_amb <- AvesInv %>% left_join(Sitio_Amb) %>% group_by(AMBIENTE, Especie) %>% summarise(n=sum(N_individuos))%>% spread(key = Especie, value = n, fill=0) %>% ungroup
#Prim_amb <- AvesPrim %>% left_join(Sitio_Amb) %>% group_by(AMBIENTE, Especie) %>% summarise(n=sum(N_individuos))%>% spread(key = Especie, value = n, fill=0) %>% ungroup

#write.csv(Inv_amb, "AvesInv_porAmb.csv")
#write.csv(Prim_amb, "AvesPrim_porAmb.csv")


############inspeccion datos

#plot(Amb, gap=0, panel=panel.smooth)

###################################
#VEGAN
#DIVERSIDAD

Inv.div<-diversity(AvesInv, index="shannon")
Prim.div <-diversity(AvesPrim, index="shannon")
Div<-cbind(Inv.div,Prim.div)
Sitios <- rownames(Div)
Div <- Div %>%  as.data.frame() %>% mutate(Sitio = Sitios, diferencia=Inv.div-Prim.div, Cambio = case_when(diferencia <=0~ "Aumenta", diferencia>0~"Disminuye"))

#Diferencias en diversidad inv-prim
ggplot(Div, aes(x=reorder(Sitio, diferencia)))+ geom_linerange(aes(ymin=Inv.div, ymax=Prim.div, color =Cambio))+ theme_classic() + theme(axis.text.x = element_text(size = 10, angle = 70, hjust = 1))+xlab("Sitios")+ylab("Diversidad")
#############

Ambiente <- Amb%>% dplyr::select(c(Sitio, AMBIENTE))

DiversidadAves <- Div %>% full_join(Ambiente)
DiversidadAmb<- DiversidadAves %>% group_by(AMBIENTE) %>% summarise(PromDivInv=mean(Inv.div), SDdivInv=sd(Inv.div), PromDivPrim=mean(Prim.div), SDdivPrim=sd(Prim.div)) 

ggplot(DiversidadAmb, aes(x=reorder(AMBIENTE,PromDivInv), y=PromDivInv), size=3) + geom_point() +geom_errorbar(aes(ymin = PromDivInv-SDdivInv, ymax = PromDivInv+SDdivInv), size=0.1) + theme_classic() + theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) + xlab("Ambientes") + ylab("Diversidad Invierno")
ggplot(DiversidadAmb, aes(x=reorder(AMBIENTE,PromDivPrim), y=PromDivPrim), size=3) + geom_point() +geom_errorbar(aes(ymin = PromDivPrim-SDdivPrim, ymax = PromDivPrim+SDdivPrim), size=0.1) + theme_classic() + theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) + xlab("Ambientes") + ylab("Diversidad Primavera")

#grafico general
ggplot(Div, aes(x=reorder(Sitios,-Inv.div), y=Inv.div)) +geom_col()+ theme_classic() + theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +geom_hline(yintercept=(mean(Inv.div)), lty=2)+ xlab("Sitios") + ylab("Diversidad Invierno")
ggplot(Div, aes(x=reorder(Sitios,-Prim.div), y=Prim.div)) +geom_col()+ theme_classic() + theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1))+geom_hline(yintercept=(mean(Prim.div)), lty=2) + xlab("Sitios") + ylab("Diversidad Primavera")

###########################################
#RIQUEZA ACUMUMULADA
spInv <- specaccum(AvesInv)
spPrim <-specaccum(AvesPrim)
plot(spInv, ylab="Riqueza de especies acumulada invierno", xlab="Sitios")
plot(spPrim, ylab="Riqueza de especies acumulada primavera", xlab="Sitios")

min(spInv$richness)
max(spInv$richness)

min(spPrim$richness)
max(spPrim$richness)

#############################################
#Detrended Correspondence Analysis and Basic Reciprocal Averaging
# mod <- decorana(Aves)
# plot(mod, cex = 0.5, cols = c(2,4))

############################
#Non-metric Multidimensional scaling- NMDS
#Invierno
vare.dis.Inv <- vegdist(AvesInv)
vare.mds0.Inv <- isoMDS(vare.dis.Inv)

# Shepard plot where ordination distances are plotted against community dissimilarities
stressplot(vare.mds0.Inv, vare.dis.Inv)
ordiplot(vare.mds0.Inv, type = "t", cex = 0.5)

vare.mds.Inv <- metaMDS(AvesInv, trace = FALSE)
plot(vare.mds.Inv, type = "t", display = "sites")

##Primavera
vare.dis.Prim <- vegdist(AvesPrim)
vare.mds0.Prim <- isoMDS(vare.dis.Prim)

# Shepard plot where ordination distances are plotted against community dissimilarities
stressplot(vare.mds0.Prim, vare.dis.Prim)
ordiplot(vare.mds0.Prim, type = "t", cex = 0.5, arrows=TRUE)

vare.mds.Prim <- metaMDS(AvesPrim, trace = FALSE)
plot(vare.mds.Prim, type = "t", display = "sites")


############################
#Comparing ordinations: Procrustes rotation
#How much did we gain with using metaMDS instead of default isoMDS?

# tmp <- wisconsin(sqrt(Aves))
# dis <- vegdist(tmp)
# vare.mds0 <- isoMDS(dis, trace = 0)
# pro <- procrustes(vare.mds, vare.mds0)
# pro
# plot(pro)
# plot(pro, kind = 2)

###########################
#Community dissimilarities

#rev hell or norm
#dis <- vegdist(decostand(Aves, "hell"), "euclid")

#...................


#cluster analysis- "single", "complete", or "average" type

#invierno
dis.Inv <- vegdist(AvesInv)
cluster.Inv <- hclust(dis.Inv, "complete")
plot(cluster.Inv)
rect.hclust(cluster.Inv, 3)
grp <- cutree(cluster.Inv, 3)

#primavera
dis.Prim <- vegdist(AvesPrim)
cluster.Prim <- hclust(dis.Prim, "complete")
plot(cluster.Prim)
rect.hclust(cluster.Prim, 3)
grp <- cutree(cluster.Prim, 3)


#dendrograma
# wa <- scores(AvesInv, display = "sites", choices = 1)
# den <- as.dendrogram(cluster.Inv)
# oden <- reorder(den, wa, mean)
# 
# op <- par(mfrow=c(2,1), mar=c(3,5,1,2)+.1)
# plot(den)
# plot(oden)
# par(op)
# 

##############
#Eigenvector methods -PCA CA

#Principal components analysis can be run as:
vare.pca <- rda(AvesPrim)
vare.pca

plot(vare.pca)

biplot(vare.pca) # incluye flecha
biplot(vare.pca, scaling = -1)

#Standardizing all species to unit variance, or using correlation coefficients instead
# of covariances will give a more balanced ordination:
vare.pca <- rda(Aves, scale = TRUE)
vare.pca

plot(vare.pca, scaling = 3)
dim(AvesPrim)

#####
#Correspondence analysis is very similar to pca:
vare.ca <- cca(Aves)
vare.ca
plot(vare.ca)

#modificando para ggplot
Ambiente <- read_rds("Occdata_ocu.rds") %>% dplyr::select(c(Sitio, AMBIENTE))
CA <- as.data.frame(vare.ca$CA$u) %>% dplyr::select(CA1, CA2) %>% mutate(Sitio = rownames(vare.ca$CA$u)) %>% full_join(Ambiente) %>% mutate(AMBIENTE=ifelse(is.na(AMBIENTE),"VERDE", AMBIENTE))

library(ggrepel)
set.seed(1990)
ggplot(CA, aes(x = CA1, y = CA2, label=Sitio)) + geom_hline(yintercept = 0, lty = 2) + geom_vline(xintercept = 0, lty = 2) + theme_classic() + geom_point(aes(color=AMBIENTE), size=3) + scale_x_continuous(limits= c(-5, 2))

#############
#Detrended correspondence analysis/ quizas no es necesario
vare.dca <- decorana(Aves)
vare.dca
plot(vare.dca, display="sites", cex = 0.5)

##################
#################  
# Environmental interpretation (imagen 900x600)
#inv
vare.mds <- metaMDS(AvesInv, trace = FALSE)
ef <- envfit(ord=vare.mds, env=Amb, permu = 999)
ef
plot(vare.mds, display = "sites")
plot(ef, p.max = 0.05)

#para las variables significativas
ef <- envfit(vare.mds~ CobVeg + Distancia_Costa+Altura, Amb, perm=999) #Variables significativas <0.001
plot(vare.mds, display="site")
plot(ef)

#prim
vare.mds2 <-metaMDS(AvesPrim,trace=FALSE)
ef2 <- envfit(ord=vare.mds, env=Amb, permu = 999) #Variables significativas <0.001
ef2
plot(vare.mds2, display = "sites")
plot(ef2, p.max = 0.05)
#para variables significativas
ef2 <- envfit(vare.mds2~ CobVeg + Distancia_Costa+Altura, Amb, perm=999) #Variables significativas <0.001
plot(vare.mds2, display="site")
plot(ef2)


#plot(ef)
tmp <- with(Amb, ordisurf(vare.mds, CobVeg, add = TRUE))
with(Amb, ordisurf(vare.mds, Altura, add = TRUE, col = "blue"))
with(Amb, ordisurf(vare.mds, Distancia_Costa, add = TRUE, col = "green"))


#con ca para factores: variables categoricas
vare.ca35 <- cca(Aves35)
ef <- envfit(vare.ca35, Amb, permutations = 999)
plot(vare.ca35)
plot(ef)

plot(vare.ca35, display = "sites", type = "p")
plot(ef, p.max = 0.05)

plot(vare.ca35, display = "sites", type = "p")
with(Amb, ordiellipse(vare.ca35, AMBIENTE, kind = "se", conf = 0.95))
with(Amb, ordispider(vare.ca35, AMBIENTE, col = "blue", label= FALSE))
with(Amb, ordihull(vare.ca35, AMBIENTE, col="blue", lty=2))

##############################
#constrain ordination. 

mod1 <- cca(AvesInv ~ ., Amb)
mod0 <- cca(AvesInv ~ 1, Amb)
mod1
plot(procrustes(cca(AvesInv), mod1))
mod <- step(mod0, scope = formula(mod1), test = "perm")
#Step:  AIC=229.01, Pr(>F)=0.0055
#AvesInv ~ AMBIENTE + Altura+ Distancia a la costa+ CobVeg + Distancia a caminos
####

mod1 <- cca(AvesPrim ~ ., Amb)
mod0 <- cca(AvesPrim ~ 1, Amb)
mod1
plot(procrustes(cca(AvesPrim), mod1))
mod <- step(mod0, scope = formula(mod1), test = "perm")
#Step:  AIC=242.27, Pr(>F)=0.055
#AvesInv ~ AMBIENTE + Altura + Distancia a la costa
####

############################################## 
#seleccion de variables ambientales
# ORDISTEP-SELECCION DE VARIABLES .- DIRECCION BOTH CON RDA, considera abundancia por especie por sitio

#Invierno
mod0 <- rda(AvesInv ~ 1, Amb)
mod1 <- rda(AvesInv~ ., Amb)

## With scope present, the default direction is "both"
mod <- ordistep(mod0, direction="both", scope = formula(mod1),permutations = how(nperm = 999))
mod
## summary table of steps
mod$anova


#Primavera
mod0 <- rda(AvesPrim ~ 1, Amb)
mod1 <- rda(AvesPrim~ ., Amb)

## With scope present, the default direction is "both"
mod <- ordistep(mod0, scope = formula(mod1), permutations = how(nperm = 999))
mod
## summary table of steps
mod$anova
