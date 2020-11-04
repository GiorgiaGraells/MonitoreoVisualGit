#Occupancia Especifica para especies 

#prediccion a partir de varibales ambientales
#se sacó del modelo la cob vegetal porque no pudoo ser 

library(tidyverse)
library(stringr)
library(unmarked)
library(MuMIn)
library(caret)

#### INVIERNO

data_det <-read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Analisis_Occu_punto/Occdata_detInv.rds")

data_ocu <-read_rds("Occdata_occu.rds")
data_ocu <- data_ocu %>% dplyr::select(-Sitio)
colnames(data_ocu) <- str_replace_all(colnames(data_ocu), pattern = " ", "_")

Simper <-read_csv("ResumenSimper_Inv2.csv")
Simper <- Simper$Especie %>% as.character %>% unique()
data_reg <-read_rds("Occdata_regInv.rds") %>% dplyr::select(starts_with(Simper))

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


Spp <- data_reg %>% dplyr::select(starts_with(Simper)) %>% 
  colnames() %>% str_remove_all("1")%>% str_remove_all("2") %>% str_remove_all("3") %>% unique()

OccuInv <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=8,  
                       form= "~ Temperatura +Humedad+ DirViento +RapViento+ Agua ~  Distancia_rio+ Altura + Buffer_2200_Bosque_Nativo+ Buffer_2200_Cultivos + Buffer_2200_Grava+ Buffer_2200_Oceano + Buffer_2200_Pastizales + Buffer_2200_Matorrales + Buffer_2200_Sup_impermeables+ Buffer_2200_Suelo_arenoso +  Buffer_2200_Plantación_de_árboles", 
                       dredge=TRUE, SppNames = Spp)
saveRDS(OccuInv, "ModelosOccuInv_Ms.rds")
##############
SppNames = OccuInv$models %>% names()
Mods <- OccuInv$models
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
Resultado_OccuInv_Ms <- PorSpp %>% purrr::reduce(bind_rows)
saveRDS(Resultado_OccuInv_Ms, "Resultado_OccuInv_Ms.rds")

###############################################################

#### PRIMAVERA

data_det <-read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Analisis_Occu_punto/Occdata_detPrim.rds")

data_ocu <-read_rds("Occdata_occu.rds")
data_ocu <- data_ocu %>% dplyr::select(-Sitio)
colnames(data_ocu) <- str_replace_all(colnames(data_ocu), pattern = " ", "_")

Simper <-read_csv("ResumenSimper_Prim2.csv")
Simper <- Simper$Especie %>% as.character %>% unique()
data_reg <-read_rds("Occdata_regPRIM.rds")%>% dplyr::select(starts_with(Simper))

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


Spp <- data_reg %>% dplyr::select(starts_with(Simper)) %>% 
  colnames() %>% str_remove_all("1")%>% str_remove_all("2") %>% str_remove_all("3") %>% unique()

OccuPrim <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=6,  
                      form= "~ Temperatura +Humedad+ DirViento +RapViento+ Agua ~ CobVeg + Distancia_rio+ Altura + Buffer_2200_Bosque_Nativo+ Buffer_2200_Cultivos + Buffer_2200_Grava+ Buffer_2200_Oceano + Buffer_2200_Pastizales + Buffer_2200_Matorrales + Buffer_2200_Sup_impermeables+ Buffer_2200_Suelo_arenoso +  Buffer_2200_Plantación_de_árboles", 
                      dredge=TRUE, SppNames = Spp)
saveRDS(OccuPrim, "ModelosOccuPrim_Ms.rds")
  ##############
SppNames = OccuPrim$models %>% names()
Mods <- OccuPrim$models
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
Resultado_OccuPrim_Ms <- PorSpp %>% purrr::reduce(bind_rows)
saveRDS(Resultado_OccuPrim_Ms, "Resultado_OccuPrim_Ms.rds")


##########################
# PREDICCIONES
# Con capas ambientales de buffers, más altura y distancia a rios, que fueron capas seleccionadas para la ocupancia.

library(raster)

CapaAmb <- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/Capas respaldo/Capas_Proporcion_small.rds")
#plot(CapaAmb)

#cambio de crs de Alt al mismo de CapaAmb
Alt <- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Analisis_Occu_punto/Alt.rds")
Alt <- raster::projectRaster(Alt, CapaAmb)
Alt <- Alt %>% crop(CapaAmb[[1]]) %>%  mask(CapaAmb[[1]])

Variables <- CapaAmb

Variables <- addLayer(Variables, Alt)
names(Variables) <- c("Buffer_2200_Oceano", "Buffer_2200_Cultivos", "Buffer_2200_Bosque_Nativo", "Buffer_2200_Plantación_de_árboles", 
                      "Buffer_2200_Pastizales", "Buffer_2200_Matorrales", "Buffer_2200_Humedales", "Buffer_2200_Lagos", "Buffer_2200_Reservorios", 
                      "Buffer_2200_Ríos", "Buffer_2200_Sup_impermeables", "Buffer_2200_Suelo_arenoso", "Buffer_2200_Rocas", 
                      "Buffer_2200_Grava", "Buffer_2200_Nieve", "Buffer_2200_Nubes", "Buffer_2200_Salar", "Buffer_2200_Hielo", "Altura")

##### Predicciones Invierno a partir de especies seleccionadas por simper
PredInv_Pelecanus_thagus <- predict(Variables, OccuInv$models$Pelecanus_thagus, type = "state")
saveRDS(PredInv_Pelecanus_thagus, "PredInv_Pelecanus_thagus.rds")

PredInv_Larus_dominicanus <- predict(Variables, OccuInv$models$Larus_dominicanus, type = "state")
saveRDS(PredInv_Larus_dominicanus, "PredInv_Larus_dominicanus.rds")

PredInv_Coragyps_atratus <- predict(Variables, OccuInv$models$Coragyps_atratus, type = "state")
saveRDS(PredInv_Coragyps_atratus, "PredInv_Coragyps_atratus.rds")

PredInv_Larosterna_inca <- predict(Variables, OccuInv$models$Larosterna_inca, type = "state")
saveRDS(PredInv_Larosterna_inca, "PredInv_Larosterna_inca.rds")

PredInv_Leucophaeus_modestus <- predict(Variables, OccuInv$models$Leucophaeus_modestus, type = "state")
saveRDS(PredInv_Leucophaeus_modestus, "PredInv_Leucophaeus_modestus.rds")

PredInv_Columba_livia <- predict(Variables, OccuInv$models$Columba_livia, type = "state")
saveRDS(PredInv_Columba_livia, "PredInv_Columba_livia.rds")

PredInv_Turdus_falcklandii <- predict(Variables, OccuInv$models$Turdus_falcklandii, type = "state") 
saveRDS(PredInv_Turdus_falcklandii, "PredInv_Turdus_falcklandii.rds")

PredInv_Sephanoides_sephaniodes <- predict(Variables, OccuInv$models$Sephanoides_sephaniodes, type = "state")
saveRDS(PredInv_Sephanoides_sephaniodes, "PredInv_Sephanoides_sephaniodes.rds")


##### Predicciones Primavera a partir de especies seleccionadas por simper

PredPrim_Leucophaeus_pipixcan <- predict(Variables, OccuPrim$models$Leucophaeus_pipixcan, type = "state")
saveRDS(PredPrim_Leucophaeus_pipixcan, "PredPrim_Leucophaeus_pipixcan.rds")

PredPrim_Larus_dominicanus <- predict(Variables, OccuPrim$models$Larus_dominicanus, type = "state")
saveRDS(PredPrim_Larus_dominicanus, "PredPrim_Larus_dominicanus.rds")

PredPrim_Columba_livia <- predict(Variables, OccuPrim$models$Columba_livia, type = "state")
saveRDS(PredPrim_Columba_livia, "PredPrim_Columba_livia.rds")

PredPrim_Larosterna_inca <- predict(Variables, OccuPrim$models$Larosterna_inca, type = "state")
saveRDS(PredPrim_Larosterna_inca, "PredPrim_Larosterna_inca.rds")

PredPrim_Phalacrocorax_bougainvillii <- predict(Variables, OccuPrim$models$Phalacrocorax_bougainvillii, type = "state")
saveRDS(PredPrim_Phalacrocorax_bougainvillii, "PredPrim_Phalacrocorax_bougainvillii.rds")

PredPrim_Pelecanus_thagus <- predict(Variables, OccuPrim$models$Pelecanus_thagus, type = "state")
saveRDS(PredPrim_Pelecanus_thagus, "PredPrim_Pelecanus_thagus.rds")


