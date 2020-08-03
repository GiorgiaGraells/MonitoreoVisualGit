
## Clusters

#### Cluster por ocupancia (Ocupación predicha por sitio muestreado en base a modelos definidos anteriormente)
library(tidygraph)
library(ggdendro)

###################
#Cluster primavera
OccuPrim37_final <- readRDS("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Analisis_Occu_punto/OccuPrim37_final.rds")

Dist_Prim <- dist(x = t(OccuPrim37_final$fit))
Cluster_Prim <- hclust(Dist_Prim)

#plot(Cluster_Prim, labels = Nombres)
ggdendrogram(Cluster_Prim)


###################
#Cluster invierno
OccuInv37_final <- readRDS("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Analisis_Occu_punto/OccuInv37_final.rds")

Dist_Inv <- dist(x = t(OccuInv37_final$fit))
Cluster_Inv <- hclust(Dist_Inv)

#plot(Cluster_Inv, labels = Nombres)
ggdendrogram(Cluster_Inv)


##################
#Cluster rasgos funcionales




#### Con bootstraping

library(pvclust)
## multiscale bootstrap resampling (non-parallel)
boston.pv <- pvclust(OccuInv37_final$fit, nboot=5000, parallel=FALSE, method.dist = "euclidian", method.hclust = "complete")


plot(boston.pv)
pvrect(boston.pv, pv = "si")
pvrect(boston.pv, pv = "au")
pvrect(boston.pv, pv = "bp")
