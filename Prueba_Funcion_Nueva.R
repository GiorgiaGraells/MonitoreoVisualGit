#Funcion nueva ocupancia para avance
#Se trabajo con las 6 especies de las encuestas y se evaluó ocupancia en los distintos ambientes. 
# No se evaluó ocupancia para las otras variables
  
library(tidyverse)
library(stringr)
library(unmarked)
library(MuMIn)

setwd("~/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Analisis_Occu_punto")

## OCUPANCIA PARA LAS 6 ESPECIES DE ENCUESTAS, POR AMBIENTE, PRIMAVERA

data_reg <-read_rds("Occdata_regPRIM.rds") #%>% dplyr::select(starts_with("CHINCOL"), starts_with("CORMORAN"), starts_with("PALOMA"), starts_with("PELICANO"),starts_with("ZORZAL"),starts_with("GAVIOTA"))

data_ocu <-read_rds("Occdata_ocu.rds")
data_ocu <- data_ocu %>% select(-Sitio)

buffer <- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Analisis_Occu_punto/Buffers.rds")

data_det <-read_rds("Occdata_detPrim.rds")




#pres = data_reg
#sitecov = data_ocu
#obscov = data_det
#spp = 5
#dredge = T
#SppNames =c( "CHINCOL", "CORMORAN", "PALOMA", "PELICANO", "ZORZAL")
#form <-  "~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_lobos+ Presencia_pescadores ~ CobVeg + AMBIENTE+ Distancia_rio+ Distancia_Costa +Distancia_Agua+ Altura+ Distancia_construccion+ Distancia_camino"

#Funcion ocupancia nueva
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


## Para agregar buffer

data_ocu2 <- bind_cols(data_ocu, Bs[[1]], Bs[[2]], Bs[[3]], Bs[[4]], Bs[[5]],Bs[[6]],Bs[[7]],Bs[[8]],Bs[[9]],Bs[[10]])

### Para llenar

#Nuevos_Datos <- data.frame(AMBIENTE = unique(data_ocu$AMBIENTE), Pred = NA, SE = NA, Spp = NA, Up = NA, Down = NA)

#OccuPrim <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=6,  form= "~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_lobos+ Presencia_pescadores ~ CobVeg + AMBIENTE+ Distancia_rio+ Distancia_Costa +Distancia_Agua+ Altura+ Distancia_construccion+ Distancia_camino", dredge=TRUE, SppNames =c( "CHINCOL", "CORMORAN", "PALOMA", "PELICANO", "ZORZAL", "GAVIOTA"))
#OccuPrim2 <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=6,  form= "~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_lobos+ Presencia_pescadores ~ AMBIENTE", dredge=TRUE, SppNames =c( "CHINCOL", "CORMORAN", "PALOMA", "PELICANO", "ZORZAL", "GAVIOTA"))

OccuPrim3 <- batchoccu2(pres = data_reg, sitecov = data_ocu2, obscov = data_det, spp=44, form= "~Presencia_perros+ Temperatura +Humedad+ DirViento +RapViento+ Agua+ Presencia_lobos+ Presencia_pescadores ~ AMBIENTE+ Altura+  Buffer_30_Pastizales+Buffer_30_Matorrales+ Buffer_30_Sup impermeables+ Buffer_30_Oceano+ Buffer_30_Suelo Arenoso+Buffer_30_Rocas+ Buffer_30_Grava+  Buffer_600_Bosque Nativo + Buffer_600_Pastizales+Buffer_600_Matorrales+ Buffer_600_Humedales+ Buffer_600_Sup impermeables+ Buffer_600_Oceano+ Buffer_600_Suelo Arenoso+Buffer_600_Rocas+ Buffer_600_Grava+  Buffer_1100_Bosque Nativo + Buffer_1100_Pastizales+Buffer_1100_Matorrales+ Buffer_1100_Humedales+ Buffer_1100_Sup impermeables+ Buffer_1100_Oceano+ Buffer_1100_Suelo Arenoso+Buffer_1100_Rocas+ Buffer_1100_Grava+  Buffer_1700_Bosque Nativo+Buffer_1700_Pastizales+Buffer_1700_Matorrales+ Buffer_1700_Humedales+ Buffer_1700_Sup impermeables+ Buffer_1700_Oceano+ Buffer_1700_Suelo Arenoso + Buffer_1700_Rocas+ Buffer_1700_Grava+  Buffer_2200_Bosque Nativo+Buffer_2200_Pastizales+Buffer_2200_Matorrales+ Buffer_2200_Humedales+ Buffer_2200_Sup impermeables+ Buffer_2200_Oceano+ Buffer_2200_Suelo Arenoso +Buffer_2200_Rocas+ Buffer_2200_Grava+ Buffer_2800_Bosque Nativo + Buffer_2800_Pastizale s+Buffer_2800_Matorrales + Buffer_2800_Humedales+ Buffer_2800_Sup impermeables + Buffer_2800_Oceano+ Buffer_2800_Suelo Arenoso+Buffer_2800_Rocas+ Buffer_2800_Grava+  Buffer_3300_Bosque Nativo+Buffer_3300_Pastizales+Buffer_3300_Matorrales+ Buffer_3300_Humedales+ Buffer_3300_Sup impermeables+ Buffer_3300_Oceano+ Buffer_3300_Suelo Arenoso+Buffer_3300_Rocas+ Buffer_3300_Grava+Buffer_3900_Bosque Nativo+Buffer_3900_Pastizales+Buffer_3900_Matorrales+ Buffer_3900_Humedales+ Buffer_3900_Sup impermeable s+ Buffer_3900_Oceano+ Buffer_3900_Suelo Arenoso + Buffer_3900_Rocas+ Buffer_3900_Grava", dredge=TRUE, SppNames = c("BLANQUILLO", "CACHUDITO", "CAHUIL", "CHERCAN", "CHINCOL", 
                "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "CODORNIZ",
                "COLEGIAL", "COMETOCINO", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "FIOFIO",  
                "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GARZA_GRANDE","GAVIOTA", "GAVIOTIN_ELEGANTE",
                "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION", 
                "GUANAY",  "HUAIRAVO", "HUALA", "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", 
                "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",  "PICAFLOR_GIGANTE",  
                "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA", 
                "RAYADOR",  "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",  
                "TORTOLA",  "ZARAPITO","ZORZAL"))


