# Prediccion de ocupancia para ambientes 

library(tidyverse)
library(stringr)
library(unmarked)
library(MuMIn)
library(caret)

setwd("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Analisis_Occu_punto")

## Funcion nueva ocupancia:

batchoccu2 <- function(pres, sitecov, obscov, spp, form, SppNames = NULL, dredge = FALSE) {
  if(is.null(SppNames)){
    SppNames <- paste("species", 1:spp, sep =".")
  }
  secuencia <- c(1:spp)*(ncol(pres)/spp)
  secuencia2<-secuencia-(secuencia[1]-1)
  models <- vector('list', spp)
  fit <- matrix(NA, nrow(pres), spp)
  Mods <- list()
  if(is.null(SppNames)){
    colnames(fit) <- paste("species", 1:spp, sep =".")
  }else if(class(SppNames) == "character"){
    colnames(fit) <- SppNames
  }
  if (dredge == FALSE) {
    for(i in 1:length(secuencia)) {
      data <- pres[, secuencia2[i]:secuencia[i]]
      data2 <- unmarkedFrameOccu(y = data, siteCovs = sitecov, obsCovs = obscov)
      try({
        models[[i]] <- occu(as.formula(form), data2)
      }, silent = T)
      try({
        fit[, i] <- suppressWarnings(predict(models[[i]], type = "state", newdata = sitecov))$Predicted
      }, silent = T)
      Mods = NULL
      print(paste("Species", as.character(i), "ready!"))
    }
  }
  else {
    for(i in 1:length(secuencia)) {
      data <- pres[, secuencia2[i]:secuencia[i]]
      data2 <- unmarkedFrameOccu(y = data, siteCovs = sitecov, obsCovs = obscov)
      try({
        #Partimos en dos Detección y occupancia
        form <- as.character(form)
        Div <- str_squish(form) %>% str_remove_all(" ")  %>% stringr::str_split(pattern = "~", simplify = T)
        
        ### Separamos dos formulas Occupancia y Deteccion
        
        Det <- Div[length(Div) - 1]
        
        VarDet <- str_split(Det, "\\+", simplify = T) %>% as.character()
        
        Fs <- list()
        
        for(x in 1:(length(VarDet) + 1)){
          if(x == (length(VarDet) + 1)){
            Formulas <- data.frame(Form = "~1 ~ 1", AICc = NA)
            Formulas$AICc[j] <- try(MuMIn::AICc(occu(as.formula("~1 ~1"), data2)), silent = T)
          }else{
            Test <- combn(VarDet, x, simplify = F)
            Formulas <- data.frame(Form = rep(NA, length(Test)), AICc = rep(NA, length(Test)))
            for(j in 1:length(Test)){
              Temp <- paste("~", paste(Test[[j]], collapse = " + "), "~ 1") 
              Formulas$Form[j] <- Temp
              Temp <- as.formula(Temp)
              Formulas$AICc[j] <- try(MuMIn::AICc(occu(Temp, data2)), silent = T) 
            }
          }
          
          Fs[[x]] <- suppressWarnings(Formulas %>% mutate(AICc = as.numeric(AICc)) %>% dplyr::filter(!is.na(AICc)) %>% arrange(AICc))
        }
        
        Fs <- suppressWarnings(purrr::reduce(Fs, bind_rows) %>% arrange(AICc))
        
        Selected <- Fs$Form[1] %>% str_split("~", simplify = T) %>% as.character()
        Selected <- Selected[length(Selected) - 1] %>% str_squish()
        
        
        Occup <- Div[length(Div)]
        
        VarOccup <- str_split(Occup, "\\+", simplify = T) %>% as.character()
        
        Fs <- list()
        
        for(x in 1:(length(VarOccup) + 1)){
          if(x == (length(VarOccup) + 1)){
            Formulas <- data.frame(Form = paste("~",Selected, "~ 1"), AICc = NA)
            Formulas$AICc[j] <- try(MuMIn::AICc(occu(as.formula(paste("~",Selected, "~ 1")), data2)), silent = T)
          }else{
            Test <- combn(VarOccup, x, simplify = F)
            Formulas <- data.frame(Form = rep(NA, length(Test)), AICc = rep(NA, length(Test)))
            for(j in 1:length(Test)){
              Temp <- paste("~", Selected, "~", paste(Test[[j]], collapse = " + ")) 
              Formulas$Form[j] <- Temp
              Temp <- as.formula(Temp)
              Formulas$AICc[j] <- try(MuMIn::AICc(occu(Temp, data2)), silent = T) 
            }
          }
          
          Fs[[x]] <- suppressWarnings(Formulas %>% mutate(AICc = as.numeric(AICc)) %>% dplyr::filter(!is.na(AICc)) %>% arrange(AICc))
        }
        
        Fs <- suppressWarnings(purrr::reduce(Fs, bind_rows) %>% arrange(AICc))
        
        Mods[[i]] <- Fs
        
        Best <- Fs$Form[1]
        
        models[[i]] <- occu(as.formula(Best), data2)
        #dredged <- suppressWarnings(dredge(occu(form, data2)))
        # select the first model and evaluate
        #models[[i]] <- eval(getCall(dredged, 1))
        
      }, silent = T)
      try({
        #predictions for the best model
        fit[, i] <- suppressWarnings(predict(models[[i]], type = "state", newdata = sitecov))$Predicted
      }, silent = T)
      
      print(paste("Species", as.character(i), "ready!"))
    }
  }
  if(is.null(SppNames)){
    names(models) <- paste("species", 1:spp, sep =".")
  }else if(class(SppNames) == "character"){
    names(models) <- SppNames
  }
  
  if(is.null(SppNames)){
    names(Mods) <- paste("species", 1:spp, sep =".")
  }else if(class(SppNames) == "character" & !is.null(Mods)){
    names(Mods) <- SppNames
  }
  
  cond <- sapply(models, function(x) !is.null(x))
  models <- models[cond]
  fit <- fit[,cond]
  Not <- SppNames[!(cond)]
  if(sum(!cond) >= 1){
    message(paste("species", paste(Not, collapse = ", "), "did not converge, try with less variables"))
  }
  result <- list(Covs = sitecov, models = models, fit = fit, Mods = Mods)
  class(result)<- "batchoccupancy"
  return(result)
}

#Nueva funcion ocupancia con ambiente forzado como variable de ocupancia
batchoccu3 <- function(pres, sitecov, obscov, spp, form, SppNames = NULL, dredge = FALSE) {
  if(is.null(SppNames)){
    SppNames <- paste("species", 1:spp, sep =".")
  }
  secuencia <- c(1:spp)*(ncol(pres)/spp)
  secuencia2<-secuencia-(secuencia[1]-1)
  models <- vector('list', spp)
  fit <- matrix(NA, nrow(pres), spp)
  Mods <- list()
  if(is.null(SppNames)){
    colnames(fit) <- paste("species", 1:spp, sep =".")
  }else if(class(SppNames) == "character"){
    colnames(fit) <- SppNames
  }
  if (dredge == FALSE) {
    for(i in 1:length(secuencia)) {
      data <- pres[, secuencia2[i]:secuencia[i]]
      data2 <- unmarkedFrameOccu(y = data, siteCovs = sitecov, obsCovs = obscov)
      try({
        models[[i]] <- occu(as.formula(form), data2)
      }, silent = T)
      try({
        fit[, i] <- suppressWarnings(predict(models[[i]], type = "state", newdata = sitecov))$Predicted
      }, silent = T)
      Mods = NULL
      print(paste("Species", as.character(i), "ready!"))
    }
  }
  else {
    for(i in 1:length(secuencia)) {
      data <- pres[, secuencia2[i]:secuencia[i]]
      data2 <- unmarkedFrameOccu(y = data, siteCovs = sitecov, obsCovs = obscov)
      try({
        #Partimos en dos Detección y occupancia
        form <- as.character(form)
        Div <- str_squish(form) %>% str_remove_all(" ")  %>% stringr::str_split(pattern = "~", simplify = T)
        
        ### Separamos dos formulas Occupancia y Deteccion
        
        Det <- Div[length(Div) - 1]
        
        VarDet <- str_split(Det, "\\+", simplify = T) %>% as.character()
        
        Fs <- list()
        
        for(x in 1:(length(VarDet) + 1)){
          if(x == (length(VarDet) + 1)){
            Formulas <- data.frame(Form = "~1 ~ 1", AICc = NA)
            Formulas$AICc[j] <- try(MuMIn::AICc(occu(as.formula("~1 ~1"), data2)), silent = T)
          }else{
            Test <- combn(VarDet, x, simplify = F)
            Formulas <- data.frame(Form = rep(NA, length(Test)), AICc = rep(NA, length(Test)))
            for(j in 1:length(Test)){
              Temp <- paste("~", paste(Test[[j]], collapse = " + "), "~ 1") 
              Formulas$Form[j] <- Temp
              Temp <- as.formula(Temp)
              Formulas$AICc[j] <- try(MuMIn::AICc(occu(Temp, data2)), silent = T) 
            }
          }
          
          Fs[[x]] <- suppressWarnings(Formulas %>% mutate(AICc = as.numeric(AICc)) %>% dplyr::filter(!is.na(AICc)) %>% arrange(AICc))
        }
        
        Fs <- suppressWarnings(purrr::reduce(Fs, bind_rows) %>% arrange(AICc))
        
        Selected <- Fs$Form[1] %>% str_split("~", simplify = T) %>% as.character()
        Selected <- Selected[length(Selected) - 1] %>% str_squish()
        
        
        Occup <- Div[length(Div)]
        
        VarOccup <- str_split(Occup, "\\+", simplify = T) %>% as.character()
        
        Fs <- list()
        
        for(x in 1:(length(VarOccup) + 1)){
          if(x == (length(VarOccup) + 1)){
            Formulas <- data.frame(Form = paste("~",Selected, "~ 1"), AICc = NA)
            Formulas$AICc[j] <- try(MuMIn::AICc(occu(as.formula(paste("~",Selected, "~ 1")), data2)), silent = T)
          }else{
            Test <- combn(VarOccup, x, simplify = F)
            Formulas <- data.frame(Form = rep(NA, length(Test)), AICc = rep(NA, length(Test)))
            for(j in 1:length(Test)){
              Temp <- paste("~", Selected, "~", paste(Test[[j]], collapse = " + ")) 
              Formulas$Form[j] <- Temp
              Temp <- as.formula(Temp)
              Formulas$AICc[j] <- try(MuMIn::AICc(occu(Temp, data2)), silent = T) 
            }
          }
          
          Fs[[x]] <- suppressWarnings(Formulas %>% mutate(AICc = as.numeric(AICc)) %>% dplyr::filter(!is.na(AICc)) %>% arrange(AICc))
        }
        
        Fs <- suppressWarnings(purrr::reduce(Fs, bind_rows) %>% arrange(AICc)) %>% dplyr::filter(str_detect(Form, "AMBIENTE"))
        Mods[[i]] <- Fs
        
        Best <- Fs$Form[1]
        
        models[[i]] <- occu(as.formula(Best), data2)
        #dredged <- suppressWarnings(dredge(occu(form, data2)))
        # select the first model and evaluate
        #models[[i]] <- eval(getCall(dredged, 1))
        
      }, silent = T)
      try({
        #predictions for the best model
        fit[, i] <- suppressWarnings(predict(models[[i]], type = "state", newdata = sitecov))$Predicted
      }, silent = T)
      
      print(paste("Species", as.character(i), "ready!"))
    }
  }
  if(is.null(SppNames)){
    names(models) <- paste("species", 1:spp, sep =".")
  }else if(class(SppNames) == "character"){
    names(models) <- SppNames
  }
  
  if(is.null(SppNames)){
    names(Mods) <- paste("species", 1:spp, sep =".")
  }else if(class(SppNames) == "character" & !is.null(Mods)){
    names(Mods) <- SppNames
  }
  
  cond <- sapply(models, function(x) !is.null(x))
  models <- models[cond]
  fit <- fit[,cond]
  Not <- SppNames[!(cond)]
  if(sum(!cond) >= 1){
    message(paste("species", paste(Not, collapse = ", "), "did not converge, try with less variables"))
  }
  result <- list(Covs = sitecov, models = models, fit = fit, Mods = Mods)
  class(result)<- "batchoccupancy"
  return(result)
}



#Ocupancia para todas las especies en primavera

data_det <-read_rds("Occdata_detPrim.rds")

data_ocu <-read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Occdata_occu.rds")
data_ocu <- data_ocu %>% dplyr::select(-Sitio)
colnames(data_ocu) <- str_replace_all(colnames(data_ocu), pattern = " ", "_")

data_reg <-read_rds("Occdata_regPRIM.rds")


Spp <- colnames(data_reg) %>% str_remove_all("\\d") %>% unique()

#Nuevos_Datos <- data.frame(AMBIENTE = unique(data_ocu$AMBIENTE), Pred = NA, SE = NA, Spp = NA, Up = NA, Down = NA)
Nuevos_Datos <- data_ocu %>% group_by(AMBIENTE) %>% summarise_all(mean) %>% mutate(Pred = NA, SE = NA, Spp = NA, Up = NA, Down = NA, Modelo = NA)

ResultadosPrim <- list()

PorSitio <- data.frame(Sitio = read_rds("Occdata_ocu.rds")$Sitio, Ambiente = data_ocu$AMBIENTE)


for(i in 1:length(Spp)){
  data_reg_temp <- data_reg %>% dplyr::select(starts_with(Spp[i]))
  
  data_reg_temp <- data_reg_temp[,1:3]
  
  message("Ajustando el modelo")
  OccuPrim_temp <- batchoccu3(pres = data_reg_temp, sitecov = data_ocu, obscov = data_det, spp=1,  form= "~ Temperatura +Humedad ~ CobVeg +AMBIENTE+ Altura + Distancia_rio+ Buffer_2200_Pastizales+Buffer_2200_Matorrales + Buffer_2200_Oceano+ Buffer_2200_Sup_impermeables+ Buffer_2200_Suelo_arenoso+ Buffer_2200_Cultivos + Buffer_2200_Plantación_de_árboles ", dredge=TRUE, SppNames =Spp[i])
  
  Nuevos_Datos$Spp <- Spp[i]

  Nuevos_Datos_Temp <- as.data.frame(Nuevos_Datos)
  
  PorSitio <-  PorSitio %>% mutate(Spp = NA)
  PorSitio$Spp <- OccuPrim_temp$fit
  colnames(PorSitio)[i + 2] <- Spp[i]
  
  message(paste("Prediciendo occupancia", Spp[i]))
  
  Nuevos_Datos_Temp$Pred <- predict(OccuPrim_temp$models[[1]], type = "state", newdata = Nuevos_Datos_Temp)$Predicted
  Nuevos_Datos_Temp$Modelo <- OccuPrim_temp$Mods[[1]]$Form[1]
  
  message("Prediciendo SE")
  
  Nuevos_Datos_Temp$SE <- predict(OccuPrim_temp$models[[1]], type = "state", newdata = Nuevos_Datos_Temp)$SE
  
  message("Prediciendo Limites")
  
  Nuevos_Datos_Temp$Up <- predict(OccuPrim_temp$models[[1]], type = "state", newdata = Nuevos_Datos_Temp)$upper
  
  Nuevos_Datos_Temp$Down <- predict(OccuPrim_temp$models[[1]], type = "state", newdata = Nuevos_Datos_Temp)$lower
  ResultadosPrim[[i]] <- Nuevos_Datos_Temp
  message(i)
}

ResultadosPrim <- ResultadosPrim %>% reduce(bind_rows)
PorSitioPrim <- PorSitio

saveRDS(ResultadosPrim, "ResultadosPrim.rds")
saveRDS(PorSitioPrim, "PorSitioPrim.rds")

##############################################


#Ocupancia para todas las especies en invierno

data_det <-read_rds("Occdata_detInv.rds")

data_ocu <-read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Occdata_occu.rds")
data_ocu <- data_ocu %>% dplyr::select(-Sitio)
colnames(data_ocu) <- str_replace_all(colnames(data_ocu), pattern = " ", "_")

data_reg <-read_rds("Occdata_regInv.rds")


Spp <- colnames(data_reg) %>% str_remove_all("\\d") %>% unique()

#Nuevos_Datos <- data.frame(AMBIENTE = unique(data_ocu$AMBIENTE), Pred = NA, SE = NA, Spp = NA, Up = NA, Down = NA)
Nuevos_Datos <- data_ocu %>% group_by(AMBIENTE) %>% summarise_all(mean) %>% mutate(Pred = NA, SE = NA, Spp = NA, Up = NA, Down = NA)

ResultadosInv <- list()

PorSitioInv <- data.frame(Sitio = read_rds("Occdata_ocu.rds")$Sitio, Ambiente = data_ocu$AMBIENTE)



for(i in 1:length(Spp)){
  data_reg_temp <- data_reg %>% dplyr::select(starts_with(Spp[i]))
  
  data_reg_temp <- data_reg_temp[,1:3]
  
  message("Ajustando el modelo")
  OccuInv_temp <- batchoccu3(pres = data_reg_temp, sitecov = data_ocu, obscov = data_det, spp=1,  form= "~ Temperatura +Humedad ~ CobVeg +AMBIENTE+ Altura + Distancia_rio+ Buffer_2200_Pastizales+Buffer_2200_Matorrales + Buffer_2200_Oceano+ Buffer_2200_Sup_impermeables+ Buffer_2200_Suelo_arenoso+ Buffer_2200_Cultivos + Buffer_2200_Plantación_de_árboles ", dredge=TRUE, SppNames =Spp[i])
  
  Nuevos_Datos$Spp <- Spp[i]
  
  Nuevos_Datos_Temp <- as.data.frame(Nuevos_Datos)
  
  PorSitioInv <-  PorSitioInv %>% mutate(Spp = NA)
  PorSitioInv$Spp <- OccuInv_temp$fit
  colnames(PorSitioInv)[i + 2] <- Spp[i]
  
  message(paste("Prediciendo occupancia", Spp[i]))
  
  Nuevos_Datos_Temp$Pred <- predict(OccuInv_temp$models[[1]], type = "state", newdata = Nuevos_Datos_Temp)$Predicted
  Nuevos_Datos_Temp$Modelo <- OccuInv_temp$Mods[[1]]$Form[1]
  
  message("Prediciendo SE")
  
  Nuevos_Datos_Temp$SE <- predict(OccuInv_temp$models[[1]], type = "state", newdata = Nuevos_Datos_Temp)$SE
  
  message("Prediciendo Limites")
  
  Nuevos_Datos_Temp$Up <- predict(OccuInv_temp$models[[1]], type = "state", newdata = Nuevos_Datos_Temp)$upper
  
  Nuevos_Datos_Temp$Down <- predict(OccuInv_temp$models[[1]], type = "state", newdata = Nuevos_Datos_Temp)$lower
  ResultadosInv[[i]] <- Nuevos_Datos_Temp
  message(i)
}


ResultadosInv <- ResultadosInv %>% reduce(bind_rows)

saveRDS(ResultadosInv, "ResultadosInv.rds")
saveRDS(PorSitioInv, "PorSitioInv.rds")


###

###################################################################### Visualizando
#PRIM
ResultadosPrim <- ResultadosPrim %>% mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO", "VERDE", "ROCA INTERVENIDA", "PLAYA INTERVENIDA", "PLAYA NATURAL"))
ggplot(Resultados, aes(x=AMBIENTE, y=Pred))+ geom_point(aes(color = Spp)) + geom_errorbar(aes(ymax = Pred + SE, ymin = Pred - SE, color = Spp)) + ylim(c(0,1.05))+
  xlab("Ambientes")+ylab("Predicción de presencia") + facet_wrap(~Spp)+theme_classic()+ theme(axis.text.x = element_text(angle=70, vjust= 1, hjust=1), legend.position = "none")

#INV
ResultadosInv <- ResultadosInv %>% mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO", "VERDE", "ROCA INTERVENIDA", "PLAYA INTERVENIDA", "PLAYA NATURAL"))
ggplot(ResultadosInv, aes(x=AMBIENTE, y=Pred))+ geom_point(aes(color = Spp)) + geom_errorbar(aes(ymax = Pred + SE, ymin = Pred - SE, color = Spp)) + ylim(c(0,1.05))+
  xlab("Ambientes")+ylab("Predicción de presencia") + facet_wrap(~Spp)+theme_classic()+ theme(axis.text.x = element_text(angle=70, vjust= 1, hjust=1), legend.position = "none")


#########################################################
#Agrupacion de estas especies segun su ocupacion de ambientes


###############################
#Inicio analisis vegan

library(vegan)

#modificando el orden de columnas para analisis vegan

#Prim
PredOccuAmb_Prim <- Resultados %>%  dplyr::select(-SE) %>% dplyr::select(-Up) %>% dplyr::select(-Down)%>%  
  group_by(AMBIENTE,Spp) %>% spread(key = Spp, value = Pred, fill=0) %>% ungroup

#saco columna de ambientes, pero queda en el orde de intervencion humana que se utilizo para graficar:urbano/verde/roca-int/playa-int/playa-nat/roca-nat
rownames(PredOccuAmb_Prim) <-PredOccuAmb_Prim$AMBIENTE
PredOccuAmb_Prim <- PredOccuAmb_Prim %>% dplyr::select(-AMBIENTE)

#Inv
PredOccuAmb_Inv <- ResultadosInv %>%  dplyr::select(-SE) %>% dplyr::select(-Up) %>% dplyr::select(-Down)%>%  
  group_by(AMBIENTE,Spp) %>% spread(key = Spp, value = Pred, fill=0) %>% ungroup

#saco columna de ambientes, pero queda en el orde de intervencion humana que se utilizo para graficar:urbano/verde/roca-int/playa-int/playa-nat/roca-nat
rownames(PredOccuAmb_Inv) <-PredOccuAmb_Inv$AMBIENTE
PredOccuAmb_Inv <- PredOccuAmb_Inv %>% dplyr::select(-AMBIENTE)



#################
#para juntar la ocupancia de invierrno y primavera para poder comparar entre ellas

#Uniendo ambas temporadas orden invierno-primavera
PredOccuSitio <- bind_rows(PorSitio, PorSitioInv)

#saco columna de ambientes, pero queda en el orden de intervencion humana que se utilizo para graficar:urbano/verde/roca-int/playa-int/playa-nat/roca-nat
PredOccuSitio <- PredOccuSitio %>% dplyr::select(-Ambiente) %>% dplyr::select(-Sitio)

#Reemplazando NA por valores dde cero
PredOccuSitio[is.na(PredOccuSitio)] = 0

############variables ambientales
Amb<- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/Analisis_Occu_punto/Occdata_ocu.rds")
Sitio_Amb <- Amb %>% dplyr::select(Sitio, AMBIENTE)
Sitio_Amb <- bind_rows(Sitio_Amb,Sitio_Amb)

Sitio_Amb$Estacion <- rep(c("Primavera", "Invierno"), each = 36)

#########################
#ordination by NMDS
library(vegan)
library(ggrepel)
NMDSPredOccuSitio <- metaMDS(PredOccuSitio, distance = "bray", k = 2) #K  num axes
#Se espera q stress sea menor a 0.2
#visualizacion

Todo_NMDS <- Sitio_Amb %>% bind_cols((NMDSPredOccuSitio$points %>% as_tibble()))

Hull <- Todo_NMDS %>% group_by(AMBIENTE, Estacion) %>% slice(chull(MDS1, MDS2))

Species <- NMDSPredOccuSitio$species %>% as_tibble() %>% mutate(Especies = rownames(NMDSPredOccuSitio$species))

#ggplot(Todo_NMDS, aes(x = MDS1, y = MDS2)) + geom_density2d(aes(color = AMBIENTE))+ geom_point(aes(color = AMBIENTE, shape = Estacion), size = 5) + theme_bw()

#todo junot sin separacion estacional
ggplot(Todo_NMDS, aes(x = MDS1, y = MDS2)) + geom_point(aes(color = AMBIENTE), size = 3) + 
   theme_bw()

ggplot(Todo_NMDS, aes(x = MDS1, y = MDS2)) + geom_point(aes(color = AMBIENTE, shape = Estacion), size = 3) + 
  geom_polygon(data = Hull, aes(color = AMBIENTE, lty = Estacion, fill = AMBIENTE), alpha = 0.1) + theme_bw()

ggplot(Todo_NMDS, aes(x = MDS1, y = MDS2)) + geom_point(aes(color = AMBIENTE, shape = Estacion), size = 3) + 
  geom_polygon(data = Hull, aes(color = AMBIENTE,  fill = AMBIENTE), alpha = 0.1) + theme_bw() + facet_wrap(~Estacion)

# Con especies

ggplot(Todo_NMDS, aes(x = MDS1, y = MDS2)) + geom_point(aes(color = AMBIENTE, shape = Estacion), size = 5) +
  geom_polygon(data = Hull, aes(color = AMBIENTE, lty = Estacion, fill = AMBIENTE), alpha = 0.3) + theme_bw() + 
  facet_wrap(~Estacion) + geom_point(data = Species) + ggrepel::geom_text_repel(data = Species, aes(label = Especies))

#ggplot(Todo_NMDS, aes(x = MDS1, y = MDS2)) + geom_density2d(aes(color = AMBIENTE))+ geom_point(aes(color = AMBIENTE, shape = Estacion), size = 5) + theme_bw() + facet_wrap(~Estacion)


