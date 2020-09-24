#Correccion abundancias aves por modelos de deteccion

library(tidyverse)
library(stringr)
library(unmarked)
library(MuMIn)
library(caret)

#Ocupancia para todas las especies en primavera

data_det <-read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Analisis_Occu_punto/Occdata_detPrim.rds")

data_ocu <-read_rds("Occdata_occu.rds")
data_ocu <- data_ocu %>% dplyr::select(-Sitio)
colnames(data_ocu) <- str_replace_all(colnames(data_ocu), pattern = " ", "_")

data_reg <-read_rds("Occdata_regPRIM.rds")


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



DetPrim <- batchoccu(pres = data_reg, sitecov = data_ocu, obscov = data_det, spp=48,  form= ~ Temperatura +Humedad+ DirViento +RapViento+ Agua ~ 1, dredge=TRUE, SppNames =c("BLANQUILLO", "CACHUDITO", "CAHUIL", "CHERCAN", "CHINCOL", "CHURRETE_CHICO", "CHURRETE_COMUN", "CHURRETE_COSTERO", "CODORNIZ",
                                                                                                                                                                             "COLEGIAL", "COMETOCINO", "CORMORAN", "COTORRA_ARGENTINA","DIUCA", "FIOFIO",  
                                                                                                                                                                             "GAVIOTA_FRANKLIN","GAVIOTA_GARUMA", "GARZA_GRANDE","GAVIOTA", "GAVIOTIN_ELEGANTE",
                                                                                                                                                                             "GOLONDRINA_CHILENA",  "GOLONDRINA_DORSO_NEGRO",  "GORRION", 
                                                                                                                                                                             "GUANAY",  "HUAIRAVO", "HUALA", "JOTE_CABEZA_NEGRA", "JOTE_CABEZA_ROJA", 
                                                                                                                                                                             "LILE",  "MIRLO", "GAVIOTIN_MONJA",  "PALOMA", "PELICANO", "PERRITO",  "PICAFLOR_GIGANTE",  
                                                                                                                                                                             "PILPILEN", "PILPILEN_NEGRO", "PIQUERO", "QUELTEHUE",  "RARA", 
                                                                                                                                                                             "RAYADOR",  "TAGUA_COMUN", "TENCA",  "TIUQUE",  "TORDO",    "TORTOLA",  "ZARAPITO","ZORZAL"))


Spp <- colnames(data_reg) %>% str_remove_all("\\d") %>% unique()

#Nuevos_Datos <- data_ocu %>% group_by(AMBIENTE) %>% summarise_all(mean) %>% mutate(Pred = NA, SE = NA, Spp = NA, Up = NA, Down = NA, Modelo = NA)
#ResultadosPrim <- list()
PorSitio <- data.frame(Sitio = read_rds("Occdata_occu.rds")$Sitio, Ambiente = data_ocu$AMBIENTE)


for(i in 1:length(Spp)){
  data_reg_temp <- data_reg %>% dplyr::select(starts_with(Spp[i]))
  
  data_reg_temp <- data_reg_temp[,1:3]
  
  message("Ajustando el modelo")
  DetPrim_temp <- batchoccu2(pres = data_reg_temp, sitecov = data_ocu, obscov = data_det, spp=1,  form= "~ Temperatura +Humedad+DirViento ~1", dredge=TRUE, SppNames =Spp[i])
  
  Nuevos_Datos$Spp <- Spp[i]
  
  Nuevos_Datos_Temp <- as.data.frame(Nuevos_Datos)
  
  PorSitio <-  PorSitio %>% mutate(Spp = NA)
  PorSitio$Spp <- DetPrim_temp$fit 
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
#
ResultadosPrim <- ResultadosPrim %>% reduce(bind_rows)
PorSitioPrim <- PorSitio


