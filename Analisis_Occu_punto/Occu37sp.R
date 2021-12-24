# Función ocupancia nueva junio
#Se trabajo con 37 especies, todas presentes en ambas temporadas 
#Se evalúa ocupancia utilizando bufferes generados en MapaYuryi.R


library(tidyverse)
library(stringr)
library(unmarked)
library(MuMIn)
library(caret)
library(foreach)
library(parallel)
library(doParallel)
library(doSNOW)


setwd("Analisis_Occu_punto/")
##########################

####INVIERNO###

## OCUPANCIA PARA LAS 37 especies 

data_reg <- read_rds("Occdata_regInv.rds")

colnames(data_reg) <- str_replace_all(colnames(data_reg), " ", "_")

#seleccion de 37 especies
data_reg <-data_reg %>% dplyr::select(starts_with("Podiceps_occipitalis"), starts_with("Troglodytes_aedon"),starts_with("Zonotrichia_capensis"), starts_with("Cinclodes_oustaleti"), 
                                                           starts_with("Cinclodes_patagonicus"), starts_with("Cinclodes_nigrofumosus"), starts_with("Lessonia_rufa"), starts_with("Phalacrocorax_brasilianus"), starts_with("Myiopsitta_monachus"),
                                                           starts_with("Diuca_diuca"), starts_with("Leucophaeus_pipixcan"), starts_with("Leucophaeus_modestus"), starts_with("Larus_dominicanus"), starts_with("Tachycineta_meyeni"), 
                                                           starts_with("Pygochelidon_cyanoleuca"),  starts_with("Passer_domesticus"),  starts_with("Phalacrocorax_bougainvillii"),  starts_with("Nycticorax_nycticorax"), starts_with("Coragyps_atratus"), 
                                                           starts_with("Cathartes_aura"), starts_with("Phalacrocorax_gaimardi"), starts_with("Molothrus_bonariensis"), starts_with("Larosterna_inca"),  starts_with("Columba_livia"), 
                                                           starts_with("Pelecanus_thagus"), starts_with("Himantopus_mexicanus"), starts_with("Haematopus_palliatus"), starts_with("Haematopus_ater"), starts_with("Sula_variegata"), 
                                                           starts_with("Vanellus_chilensis"),  starts_with("Phytotoma_rara"),  starts_with("Fulica_armillata"), starts_with("Mimus_thenca"),  starts_with("Milvago_chimango"),  
                                                           starts_with("Curaeus_curaeus"),   starts_with("Zenaida_auriculata"), starts_with ("Turdus_falcklandii"))

data_det <-read_rds("Occdata_detInv.rds")

data_ocu <-read_rds("Occdata_occu.rds")
data_ocu <- data_ocu %>% select(-Sitio)
colnames(data_ocu) <- str_replace_all(colnames(data_ocu), pattern = " ", "_")

data_ocu2 <- data_ocu %>% mutate_if(is.numeric, scale) #centro y escalamiento de variables-media 0, varianza1

#Info para ocupancia

# pres = data_reg
# sitecov = data_ocu
# obscov = data_det
# spp=37
# form= "~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_otros+ Presencia_pescadores ~ AMBIENTE + CobVeg + Distancia_Costa + Altura + Distancia_construccion + Distancia_camino+ Distancia_Agua+ Distancia_rio+ Buffer_2200_Bosque_Nativo+  Buffer_2200_Plantación_de_árboles+ Buffer_2200_Pastizales+Buffer_2200_Matorrales+ Buffer_2200_Humedales+  Buffer_2200_Lagos+ Buffer_2200_Ríos + Buffer_2200_Oceano+ Buffer_2200_Sup_impermeables+ Buffer_2200_Suelo_arenoso +Buffer_2200_Rocas+ Buffer_2200_Grava+ Buffer_2200_Reservorios"
# dredge=TRUE
# SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" )


  
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
        
        print(paste("Starting to fit detection models for species", i, "of", length(secuencia)))
        
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
              gc()
            }
          }
          
          Fs[[x]] <- suppressWarnings(Formulas %>% mutate(AICc = as.numeric(AICc)) %>% dplyr::filter(!is.na(AICc)) %>% arrange(AICc))
          message(paste("finished for", x, "number of variables"))
        }
        
        Fs <- suppressWarnings(purrr::reduce(Fs, bind_rows) %>% arrange(AICc))
        
        Selected <- Fs$Form[1] %>% str_split("~", simplify = T) %>% as.character()
        Selected <- Selected[length(Selected) - 1] %>% str_squish()
        
        print(paste("Detection model for species", i, "is", Selected))
        
        Occup <- Div[length(Div)]
        
        VarOccup <- str_split(Occup, "\\+", simplify = T) %>% as.character()
        
        Fs <- list()
        
        print(paste("Starting to fit occupancy models for species", i, "of", length(secuencia)))
        
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
              if((j %% 100) == 0){
                message(paste(j, "of", length(Test), "Ready"))
                gc()
              }
            }
          }
          
          Fs[[x]] <- suppressWarnings(Formulas %>% mutate(AICc = as.numeric(AICc)) %>% dplyr::filter(!is.na(AICc)) %>% arrange(AICc))
          message(paste("finished for", x, "number of variables", Sys.time()))
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


batchoccu2par <- function(pres, sitecov, obscov, spp, form, SppNames = NULL, dredge = FALSE, ncor = 2){
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
        
        print(paste("Starting to fit detection models for species", i, "of", length(secuencia)))
        
        for(x in 1:(length(VarDet) + 1)){
          if(x == (length(VarDet) + 1)){
            Formulas[[length(VarDet) + 1]] <- data.frame(Form = "~1 ~ 1", AICc = NA)
            Formulas$AICc <- try(MuMIn::AICc(occu(as.formula("~1 ~1"), data2)), silent = T)
          }else{
            Test <- combn(VarDet, x, simplify = F)
            cl <- makeCluster(ncor)
            registerDoParallel(cl)
            Formulas <- foreach(j=1:length(Test), .packages = c("unmarked", "MuMIn", "dplyr"), .combine = bind_rows) %dopar%{
              Formulas <- data.frame(Form = NA, AICc = NA)
              Temp <- paste("~", paste(Test[[j]], collapse = " + "), "~ 1") 
              Formulas$Form <- Temp
              Temp <- as.formula(Temp)
              Formulas$AICc <- try(MuMIn::AICc(occu(Temp, data2)), silent = T)
              Formulas$AICc <- as.numeric(Formulas$AICc)
              gc()
              Formulas
            }
            stopCluster(cl)
          }
          
          Fs[[x]] <- suppressWarnings(Formulas %>% mutate(AICc = as.numeric(AICc)) %>% dplyr::filter(!is.na(AICc)) %>% arrange(AICc))
          message(paste("finished for", x, "number of variables"))
        }
        
        message("Joining dataframes")
        
        Fs <- suppressWarnings(purrr::reduce(Fs, bind_rows) %>% arrange(AICc))
        
        message("Selecting best detection model model")
        
        Selected <- Fs$Form[1] %>% str_split("~", simplify = T) %>% as.character()
        Selected <- Selected[length(Selected) - 1] %>% str_squish()
        
        print(paste("Detection model for species", i, "is", Selected))
        
        Occup <- Div[length(Div)]
        
        VarOccup <- str_split(Occup, "\\+", simplify = T) %>% as.character()
        
        Fs <- list()
        
        print(paste("Starting to fit occupancy models for species", i, "of", length(secuencia)))
        
        for(x in 1:(length(VarOccup) + 1)){
          if(x == (length(VarOccup) + 1)){
            Formulas <- data.frame(Form = paste("~",Selected, "~ 1"), AICc = NA)
            Formulas$AICc[j] <- try(MuMIn::AICc(occu(as.formula(paste("~",Selected, "~ 1")), data2)), silent = T)
          }else{
            Test <- combn(VarOccup, x, simplify = F)
            Formulas <- data.frame(Form = rep(NA, length(Test)), AICc = rep(NA, length(Test)))
            cl <- makeCluster(ncor)
            registerDoParallel(cl)
            foreach(j=1:length(Test), .packages = c("unmarked", "MuMIn")) %dopar%{
              Temp <- paste("~", Selected, "~", paste(Test[[j]], collapse = " + ")) 
              Formulas$Form[j] <- Temp
              Temp <- as.formula(Temp)
              Formulas$AICc[j] <- try(MuMIn::AICc(occu(Temp, data2)), silent = T) 
              if((j %% 100) == 0){
                gc()
              }
            }
            stopCluster(cl)
          }
          
          Fs[[x]] <- suppressWarnings(Formulas %>% mutate(AICc = as.numeric(AICc)) %>% dplyr::filter(!is.na(AICc)) %>% arrange(AICc))
          message(paste("finished for", x, "number of variables", Sys.time()))
        }
        
        Fs <- suppressWarnings(purrr::reduce(Fs, bind_rows) %>% arrange(AICc))
        
        Mods[[i]] <- Fs
        
        
        Best <- Fs$Form[1]
        
        models[[i]] <- occu(as.formula(Best), data2)
        
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



#####################

#VARIABLES AMBIENTALES DE LOS SITIOS DE MUESTREO
#OccuInv37_AMB <- batchoccu2par(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form= "~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_otros ~ AMBIENTE + CobVeg + Distancia_Costa + Altura + Distancia_construccion + Distancia_camino", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL"), ncor = 4)

####################################################################################################
#FUNCION DE OCUPANCIA POR GRUPOS DE VARIABLES-DISTANCIA DE BUFFER INVIERNO/// ESCALAS DE EFECTO


# BUFFER 30 M DE LOS SITIOS DE MUESTREO
OccuInv37_30 <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, 
                              spp=37 , 
                              form= "~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_otros ~ Buffer_30_Pastizales+Buffer_30_Matorrales+ Buffer_30_Sup_impermeables+ Buffer_30_Oceano+ Buffer_30_Suelo_arenoso+Buffer_30_Rocas+ Buffer_30_Grava", 
                              dredge=TRUE,  
                              SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL"))

#AICC por especie
# MuMIn::AICc(OccuInv37$models$GOLONDRINA_DORSO_NEGRO)
# MuMIn::AICc(OccuInv37$models$HUAIRAVO)
# MuMIn::AICc(OccuInv37$models$MIRLO)
# MuMIn::AICc(OccuInv37$models$PERRITO)
# MuMIn::AICc(OccuInv37$models$PILPILEN_NEGRO)
# MuMIn::AICc(OccuInv37$models$QUELTEHUE)
# MuMIn::AICc(OccuInv37$models$TAGUA_COMUN)

#BUFFER 600M DE LOS SITIOS DE MUESTREO
OccuInv37_600 <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form= "~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_otros ~ Buffer_600_Bosque_Nativo + Buffer_600_Pastizales+Buffer_600_Matorrales+ Buffer_600_Humedales+ Buffer_600_Sup_impermeables+ Buffer_600_Oceano+ Buffer_600_Suelo_arenoso+Buffer_600_Rocas+ Buffer_600_Grava", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" ))

#BUFFER 1100M DE LOS SITIOS DE MUESTREO
OccuInv37_1100 <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form= "~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_otros ~ Buffer_1100_Bosque_Nativo + Buffer_1100_Pastizales+Buffer_1100_Matorrales+ Buffer_1100_Humedales+ Buffer_1100_Sup_impermeables+ Buffer_1100_Oceano+ Buffer_1100_Suelo_arenoso+Buffer_1100_Rocas+ Buffer_1100_Grava ", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" ))

#BUFFER 1700M DE LOS SITIOS DE MUESTREO
OccuInv37_1700 <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form= "~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_otros ~ Buffer_1700_Bosque_Nativo+Buffer_1700_Pastizales+Buffer_1700_Matorrales+ Buffer_1700_Humedales+ Buffer_1700_Sup_impermeables+ Buffer_1700_Oceano+ Buffer_1700_Suelo_arenoso + Buffer_1700_Rocas+ Buffer_1700_Grava", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" ))

#BUFFER 2200M DE LOS SITIOS DE MUESTREO
OccuInv37_2200 <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form= "~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_otros ~ Buffer_2200_Bosque_Nativo+Buffer_2200_Pastizales+Buffer_2200_Matorrales+ Buffer_2200_Humedales+ Buffer_2200_Sup_impermeables+ Buffer_2200_Oceano+ Buffer_2200_Suelo_arenoso +Buffer_2200_Rocas+ Buffer_2200_Grava", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" ))

#BUFFER 2800M DE LOS SITIOS DE MUESTREO
OccuInv37_2800 <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form= "~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_otros ~ Buffer_2800_Bosque_Nativo + Buffer_2800_Pastizale s+Buffer_2800_Matorrales + Buffer_2800_Humedales+ Buffer_2800_Sup_impermeables + Buffer_2800_Oceano+ Buffer_2800_Suelo_arenoso + Buffer_2800_Rocas+ Buffer_2800_Grava", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" ))

#BUFFER 3300M DE LOS SITIOS DE MUESTREO
OccuInv37_3300 <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form= "~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_otros ~ Buffer_3300_Bosque_Nativo+Buffer_3300_Pastizales+Buffer_3300_Matorrales+ Buffer_3300_Humedales+ Buffer_3300_Sup_impermeables+ Buffer_3300_Oceano+ Buffer_3300_Suelo_arenoso +Buffer_3300_Rocas+ Buffer_3300_Grava", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" ))

#BUFFER 3900M DE LOS SITIOS DE MUESTREO
OccuInv37_3900 <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form= "~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_otros ~ Buffer_3900_Bosque_Nativo+Buffer_3900_Pastizales+Buffer_3900_Matorrales+ Buffer_3900_Humedales+ Buffer_3900_Sup_impermeables+ Buffer_3900_Oceano+ Buffer_3900_Suelo_arenoso + Buffer_3900_Rocas+ Buffer_3900_Grava", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" ))

#BUFFER 4400M DE LOS SITIOS DE MUESTREO
OccuInv37_4400 <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form= "~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_otros ~ Buffer_4400_Bosque_Nativo+Buffer_4400_Pastizales+Buffer_4400_Matorrales+ Buffer_4400_Humedales+ Buffer_4400_Sup_impermeables+ Buffer_4400_Oceano+ Buffer_4400_Suelo_arenoso + Buffer_4400_Rocas+ Buffer_4400_Grava", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL"))

#BUFFER 50000M DE LOS SITIOS DE MUESTREO
OccuInv37_5000 <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form= "~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_otros+ Presencia_pescadores ~ Buffer_5000_Bosque_Nativo+Buffer_5000_Pastizales+Buffer_5000_Matorrales+ Buffer_5000_Humedales+ Buffer_5000_Sup_impermeables+ Buffer_5000_Oceano+ Buffer_5000_Suelo_arenoso + Buffer_5000_Rocas+ Buffer_5000_Grava", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" ))

########################
#para generar data frame con resumen de modelos para todas las especies y todos los buffers

Distancias <- round(seq(from = 30, to = 5000, length.out = 10), -2)
Distancias[1] <- 30
SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" )

Mods <- list(OccuInv37_30, OccuInv37_600, OccuInv37_1100, OccuInv37_1700, OccuInv37_2200, OccuInv37_2800, OccuInv37_3300, OccuInv37_3900, OccuInv37_4400, OccuInv37_5000)

Todos <- list()

Presences_by_sp <- list.files(path = "Final_Ocurrences/", pattern = ".rds", full.names = T)

Nclust <- floor(parallel::detectCores()/4)
cl <- makeSOCKcluster(Nclust)
registerDoSNOW(cl)

ntasks <- length(Presences_by_sp)
pb <- tkProgressBar(max=ntasks)
progress <- function(n) setTkProgressBar(pb, n)
opts <- list(progress=progress)


Todos <- foreach(i = 1:length(Distancias), .packages = c("sf","dplyr", "stringr", "raster", "maxnet", "terra", "dismo", "magrittr"), .options.snow=opts, .combine = bind_rows()) %dopar% {
  Temp <- Mods[[i]]
  PorSpp <- list()
  for(j in 1:length(SppNames)){
    Temp2 <- Temp$models[[SppNames[j]]]
    Temp3 <- data.frame(Especies=rep(NA,length(Temp$models[[SppNames[j]]]@estimates@estimates$state@estimates)), Parametro=rep(NA,length(Temp$models[[SppNames[j]]]@estimates@estimates$state@estimates)), Estimador=rep(NA,length(Temp$models[[SppNames[j]]]@estimates@estimates$state@estimates)), SE=rep(NA,length(Temp$models[[SppNames[j]]]@estimates@estimates$state@estimates)), Distancia=rep(NA,length(Temp$models[[SppNames[j]]]@estimates@estimates$state@estimates)), p = NA, AICc = NA)
    Temp3$Especies <- SppNames[j]
    Temp3$Parametro <- Temp$models[[SppNames[j]]]@estimates@estimates$state@estimates %>% names() %>% str_remove_all(pattern = paste0("Buffer_",Distancias[i], "_"))
    Temp3$Estimador <- Temp$models[[SppNames[j]]]@estimates@estimates$state@estimates %>% as.numeric() 
    Temp3$Distancia <- Distancias[i]
    Temp3$AICc <- AICc(Temp2)
    PorSpp[[j]] <- Temp3
  }
  PorSpp %>% purrr::reduce(bind_rows)
  message(paste("lista distancia", Distancias[i]))
}

write_csv(Todos, "Resumen_ocuInv_buffer.csv")


#Grafico para todas las especies y variables x distancia
library(ggplot2)
library(tidyverse)
Todos <- Todos %>% filter(Parametro!= "(Intercept)")
Todos <- Todos %>% mutate(Origen= case_when(Especies %in% c("BLANQUILLO","CHURRETE_CHICO","CHURRETE_COMUN","CHURRETE_COSTERO","CORMORAN","GAVIOTA_FRANKLIN","GAVIOTA_GARUMA","GAVIOTA","GUANAY","HUAIRAVO","LILE","GAVIOTIN_MONJA","PELICANO","PERRITO","PILPILEN","PILPILEN_NEGRO","PIQUERO","QUELTEHUE","TAGUA_COMUN")~"Marino",
                                            Especies %in% c("CHERCAN","CHINCOL","COLEGIAL","COTORRA_ARGENTINA","DIUCA","GOLONDRINA_CHILENA","GOLONDRINA_DORSO_NEGRO", "GORRION","JOTE_CABEZA_NEGRA","JOTE_CABEZA_ROJA","MIRLO","PALOMA","RARA","TENCA","TIUQUE","TORDO","TORTOLA","ZORZAL")~"Terrestre"))

ggplot(Todos, aes(x=Distancia, y=Estimador)) + geom_line(aes(color=Parametro)) + facet_wrap(~Especies, scales = "free_y")
ggplot(Todos, aes(x=Distancia, y=Estimador, group=Especies)) + geom_line(aes(color=Origen)) + facet_wrap(~Parametro)


#ESPECIES MARINAS
Marino <- Todos %>% filter(Origen=="Marino") %>% 
  mutate(Parametro=fct_relevel(Parametro, "Bosque_Nativo", "Matorrales", "Pastizales", "Humedales", "Sup_impermeables", "Suelo_arenoso", "Grava", "Rocas", "Oceano"))
ggplot(Marino, aes(x=Distancia, y=Estimador)) + geom_line(aes(color=Especies)) + facet_wrap(~Parametro)

#ESPECIES TERRESTRES
Terrestre <- Todos %>% filter(Origen=="Terrestre") %>% 
  mutate(Parametro=fct_relevel(Parametro, "Bosque_Nativo", "Matorrales", "Pastizales", "Humedales", "Sup_impermeables", "Suelo_arenoso", "Grava", "Rocas", "Oceano"))
ggplot(Terrestre, aes(x=Distancia, y=Estimador)) + geom_line(aes(color=Especies)) + facet_wrap(~Parametro)

write_csv(Todos, "Resumen_ocuInv_buffer.csv")

###################################################################
#########################################################

#seleccion de variables : aproximaciones de la misma variable, con buffer 2200 m

#invierno
#Rio
OccuInv37_RIO <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form= "~ Temperatura +Humedad+ DirViento +RapViento+ Agua ~ Distancia_rio + Buffer_2200_Ríos", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" ))

#Mar
OccuInv37_MAR <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form= "~ Temperatura +Humedad+ DirViento +RapViento+ Agua ~ Buffer_2200_Oceano +  Distancia_Costa", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" ))

#Agua
OccuInv37_AGUA <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form= "~ Temperatura +Humedad+ DirViento +RapViento+ Agua ~ Distancia_Agua + Buffer_2200_Lagos + Buffer_2200_Reservorios+ Buffer_2200_Humedales", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" ))


#primavera
#Rio
OccuPrimv37_RIO <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form= "~ Temperatura +Humedad+ DirViento +RapViento+ Agua ~ Distancia_rio + Buffer_2200_Ríos", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" ))
#aparerce mas buffer_rios
#Mar
OccuPrim37_MAR <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form= "~ Temperatura +Humedad+ DirViento +RapViento+ Agua ~ Buffer_2200_Oceano +  Distancia_Costa", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" ))

#Agua
OccuPrim37_AGUA <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form= "~ Temperatura +Humedad+ DirViento +RapViento+ Agua ~ Distancia_Agua + Buffer_2200_Lagos + Buffer_2200_Reservorios+ Buffer_2200_Humedales", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" ))





###############################################
##############################################
#Ocupancia utilizando variables ambientale y considerando el % de cobertura vegetal en una escala de 2200m diametro

## OCUPANCIA PARA LAS 37 especies 

OccuInv37_final <-     batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form= "~ Temperatura +Humedad+ DirViento +RapViento+ Agua ~ CobVeg + Altura + Distancia_Agua+ Distancia_rio+ Buffer_2200_Pastizales+Buffer_2200_Matorrales + Buffer_2200_Oceano+ Buffer_2200_Sup_impermeables+ Buffer_2200_Suelo_arenoso +Buffer_2200_Grava +Buffer_2200_Cultivos+ Buffer_2200_Bosque_Nativo", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" ))
##

setwd("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Analisis_Occu_punto")

data_reg <- read_rds("Occdata_regPRIM.rds")

colnames(data_reg) <- str_replace_all(colnames(data_reg), " ", "_")

#seleccion de 37 especies
data_reg <-data_reg %>% dplyr::select(starts_with("BLANQUILLO"), starts_with("CHERCAN"),starts_with("CHINCOL"), starts_with("CHURRETE_CHICO"), 
                                      starts_with("CHURRETE_COMUN"), starts_with("CHURRETE_COSTERO"), starts_with("COLEGIAL"), starts_with("CORMORAN"), starts_with("COTORRA_ARGENTINA"),
                                      starts_with("DIUCA"),starts_with ("FRANKLIN"),starts_with("GARUMA"), starts_with("GAVIOTA"), starts_with("GOLONDRINA_CHILENA"), 
                                      starts_with("GOLONDRINA_DORSO_NEGRO"),  starts_with("GORRION"),  starts_with("GUANAY"),  starts_with("HUAIRAVO"),starts_with("JOTE_CABEZA_NEGRA"), 
                                      starts_with("JOTE_CABEZA_ROJA"), starts_with("LILE"), starts_with("MIRLO"), starts_with("MONJA"),  starts_with("PALOMA"), 
                                      starts_with("PELICANO"), starts_with("PERRITO"), starts_with("PILPILEN"), starts_with("PILPILEN_NEGRO"), starts_with("PIQUERO"), 
                                      starts_with("QUELTEHUE"),  starts_with("RARA"),  starts_with("TAGUA_COMUN"), starts_with("TENCA"),  starts_with("TIUQUE"),  
                                      starts_with("TORDO"),   starts_with("TORTOLA"), starts_with ("ZORZAL"))

data_det <-read_rds("Occdata_detPrim.rds")

data_ocu <-read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Occdata_occu.rds")
data_ocu <- data_ocu %>% select(-Sitio)
colnames(data_ocu) <- str_replace_all(colnames(data_ocu), pattern = " ", "_")



OccuPrim37_final <-     batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=37 , form=  "~ Temperatura +Humedad+ DirViento +RapViento+ Agua ~ CobVeg + Altura + Distancia_Agua+ Distancia_rio+ Buffer_2200_Pastizales+Buffer_2200_Matorrales + Buffer_2200_Oceano+ Buffer_2200_Sup_impermeables+ Buffer_2200_Suelo_arenoso +Buffer_2200_Grava +Buffer_2200_Cultivos+ Buffer_2200_Bosque_Nativo", dredge=TRUE,  SppNames = c("BLANQUILLO", "CHERCAN", "CHINCOL",  "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "COLEGIAL", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GAVIOTA", "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION",  "GUANAY",  "HUAIRAVO",  "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",   "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA",   "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",   "TORTOLA",  "ZORZAL" ))


saveRDS(OccuPrim37_final, "OccuPrim37_final.rds")

###############################################



#INVIERNO: Extraccion de parametros para generar un data frame con los estimadores por especie segun ocupancia por estacion
OccuInv37_final <- readRDS("Analisis_Occu_punto/OccuInv37_final.rds")


SppNames = OccuInv37_final$models %>% names()
Mods <- OccuInv37_final$models
PorSpp <- list()

for(j in 1:length(SppNames)){
    Temp2 <- Mods[[SppNames[j]]]
    Temp3 <- data.frame(Especies=rep(NA,length(Mods[[SppNames[j]]]@estimates@estimates$state@estimates)), Parametro=rep(NA,length(Mods[[SppNames[j]]]@estimates@estimates$state@estimates)), Estimador=rep(NA,length(Mods[[SppNames[j]]]@estimates@estimates$state@estimates)), SE=rep(NA,length(Mods[[SppNames[j]]]@estimates@estimates$state@estimates)), p = NA, AICc = NA)
    Temp3$Especies <- SppNames[j]
    Temp3$Parametro <- Mods[[SppNames[j]]]@estimates@estimates$state@estimates %>% names() %>% str_remove_all(pattern = paste0("Buffer_",2200, "_"))
    Temp3$Estimador <- Mods[[SppNames[j]]]@estimates@estimates$state@estimates %>% as.numeric() 
    try({Temp3$AICc <- AICc(Temp2)})
    PorSpp[[j]] <- Temp3
}
PorSppInv <- PorSpp %>% purrr::reduce(bind_rows)
saveRDS(PorSppInv, "PorSppInv.rds")

#PRIMAVERA: Extraccion de parametros para generar un data frame con los estimadores por especie segun ocupancia por estacion

OccuPrim37_final <- readRDS("Analisis_Occu_punto/OccuPrim37_final.rds")

SppNames = OccuPrim37_final$models %>% names()
Mods <- OccuPrim37_final$models
PorSpp <- list()

for(j in 1:length(SppNames)){
  Temp2 <- Mods[[SppNames[j]]]
  Temp3 <- data.frame(Especies=rep(NA,length(Mods[[SppNames[j]]]@estimates@estimates$state@estimates)), Parametro=rep(NA,length(Mods[[SppNames[j]]]@estimates@estimates$state@estimates)), Estimador=rep(NA,length(Mods[[SppNames[j]]]@estimates@estimates$state@estimates)), SE=rep(NA,length(Mods[[SppNames[j]]]@estimates@estimates$state@estimates)), p = NA, AICc = NA)
  Temp3$Especies <- SppNames[j]
  Temp3$Parametro <- Mods[[SppNames[j]]]@estimates@estimates$state@estimates %>% names() %>% str_remove_all(pattern = paste0("Buffer_",2200, "_"))
  Temp3$Estimador <- Mods[[SppNames[j]]]@estimates@estimates$state@estimates %>% as.numeric() 
  try({Temp3$AICc <- AICc(Temp2)})
  PorSpp[[j]] <- Temp3
}
PorSppPrim <- PorSpp %>% purrr::reduce(bind_rows)
saveRDS(PorSppPrim, "PorSppPrim.rds")

##############

#  Visualizacion ocupancia ambas temporadas con varibles 2200 buffer
# ambas temporadas

PorSppPrim <-read_rds("PorSppPrim.rds")
PorSppInv <-read_rds("PorSppInv.rds")


PorSppInv$Temporada <- "Invierno"
PorSppPrim$Temporada <- "Primavera"

PorSppTodo <- bind_rows(PorSppInv, PorSppPrim) %>% dplyr::filter(Parametro != "(Intercept)")
PorSppTodo <- PorSppTodo %>% mutate(Origen= case_when(Especies %in% c("BLANQUILLO","CHURRETE_CHICO","CHURRETE_COMUN","CHURRETE_COSTERO","CORMORAN","GAVIOTA_FRANKLIN","GAVIOTA_GARUMA","GAVIOTA","GUANAY","HUAIRAVO","LILE","GAVIOTIN_MONJA","PELICANO","PERRITO","PILPILEN","PILPILEN_NEGRO","PIQUERO","QUELTEHUE","TAGUA_COMUN")~"Aves marinas",
                                            Especies %in% c("CHERCAN","CHINCOL","COLEGIAL","COTORRA_ARGENTINA","DIUCA","GOLONDRINA_CHILENA","GOLONDRINA_DORSO_NEGRO", "GORRION","JOTE_CABEZA_NEGRA","JOTE_CABEZA_ROJA","MIRLO","PALOMA","RARA","TENCA","TIUQUE","TORDO","TORTOLA","ZORZAL")~"Aves terrestres"))

PorSppTodo <- PorSppTodo %>% mutate(Origen= case_when(Especies %in% c("BLANQUILLO","CHURRETE_CHICO","CHURRETE_COMUN","CHURRETE_COSTERO","CORMORAN","GAVIOTA_FRANKLIN","GAVIOTA_GARUMA","GAVIOTA","GUANAY","HUAIRAVO","LILE","GAVIOTIN_MONJA","PELICANO","PERRITO","PILPILEN","PILPILEN_NEGRO","PIQUERO","QUELTEHUE","TAGUA_COMUN")~"Aves marinas",
                                                      Especies %in% c("CHERCAN","CHINCOL","COLEGIAL","COTORRA_ARGENTINA","DIUCA","GOLONDRINA_CHILENA","GOLONDRINA_DORSO_NEGRO", "GORRION","JOTE_CABEZA_NEGRA","JOTE_CABEZA_ROJA","MIRLO","PALOMA","RARA","TENCA","TIUQUE","TORDO","TORTOLA","ZORZAL")~"Aves terrestres"))



#ggplot(PorSppTodo, aes(x = Estimador, y = Parametro)) +  geom_boxplot(aes(color=Temporada))+
#  facet_grid(cols=vars(Temporada)) +geom_vline(xintercept = 0, lty=3)+ theme_bw()

ggplot(PorSppTodo, aes(x = Estimador, y = Parametro)) +  geom_boxplot(aes(color=Temporada))+
  geom_vline(xintercept = 0, lty=3)+facet_grid(Origen~Temporada)+ theme_bw()+   ylab("Parametros")
###min

ggplot(PorSppTodo, aes(x = Estimador, y = reorder(Parametro, Estimador, min))) +  geom_boxplot(aes(color=Temporada))+
  geom_vline(xintercept = 0, lty=3)+facet_grid(Origen~Temporada)+ theme_bw()+ ylab("Variables ocupancia")

### Rango

FUNS <- function(x){
  abs(diff(range(x)))
}


meta_ocupancia<- ggplot(PorSppTodo, aes(x = Estimador, y = fct_reorder(Parametro, Estimador, FUNS, .desc = T))) +  geom_boxplot(aes(color=Temporada))+
  geom_vline(xintercept = 0, lty=3)+facet_grid(Origen~Temporada)+ theme_bw()+ ylab("Variables ocupancia")
 


saveRDS(meta_ocupancia, "meta_ocupancia.rds")

plot(meta_ocupancia)






