# Función ocupancia nueva junio

library(tidyverse)
library(stringr)
library(unmarked)
library(MuMIn)
library(caret)

setwd("Analisis_Occu_punto/")
##########################

####INVIERNO###

## OCUPANCIA PARA LAS TODAS LAS ESPECIES 

data_reg <- read_rds("Occdata_regInv.rds")

colnames(data_reg) <- str_replace_all(colnames(data_reg), " ", "_")

Species_Names <- unique(gsub('[0-9]+', '', colnames(data_reg)))

N_Species <- length(Species_Names)

data_det <-read_rds("Occdata_detInv.rds")

data_ocu <-read_rds("Occdata_occu.rds")
data_ocu <- data_ocu %>% select(-Sitio)
colnames(data_ocu) <- str_replace_all(colnames(data_ocu), pattern = " ", "_")

data_ocu2 <- data_ocu %>% mutate_if(is.numeric, scale) #centro y escalamiento de variables-media 0, varianza1

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


######## Con solo algunas variables

# BUFFER 30 M DE LOS SITIOS DE MUESTREO
OccuInv37_30 <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, 
                           spp=N_Species, 
                           form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_30_Pastizales+Buffer_30_Matorrales+ Buffer_30_Sup_impermeables+ Buffer_30_Oceano+ Buffer_30_Suelo_arenoso+Buffer_30_Rocas+ Buffer_30_Grava", 
                           dredge=TRUE,  
                           SppNames = Species_Names)


#BUFFER 600M DE LOS SITIOS DE MUESTREO
OccuInv37_600 <- batchoccu2(pres = data_reg, 
                            sitecov = data_ocu, 
                            obscov = data_det, 
                            spp=N_Species , 
                            form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_600_Bosque_Nativo + Buffer_600_Pastizales+Buffer_600_Matorrales+ Buffer_600_Humedales+ Buffer_600_Sup_impermeables+ Buffer_600_Oceano+ Buffer_600_Suelo_arenoso+Buffer_600_Rocas+ Buffer_600_Grava", 
                            dredge=TRUE,  
                            SppNames = Species_Names)

#BUFFER 1100M DE LOS SITIOS DE MUESTREO
OccuInv37_1100 <- batchoccu2(pres = data_reg, 
                             sitecov = data_ocu, 
                             obscov = data_det, 
                             spp=N_Species , 
                             form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_1100_Bosque_Nativo + Buffer_1100_Pastizales+Buffer_1100_Matorrales+ Buffer_1100_Humedales+ Buffer_1100_Sup_impermeables+ Buffer_1100_Oceano+ Buffer_1100_Suelo_arenoso+Buffer_1100_Rocas+ Buffer_1100_Grava ", 
                             dredge=TRUE,  
                             SppNames = Species_Names)

#BUFFER 1700M DE LOS SITIOS DE MUESTREO
OccuInv37_1700 <- batchoccu2(pres = data_reg, 
                             sitecov = data_ocu, 
                             obscov = data_det, 
                             spp=N_Species , 
                             form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_1700_Bosque_Nativo+Buffer_1700_Pastizales+Buffer_1700_Matorrales+ Buffer_1700_Humedales+ Buffer_1700_Sup_impermeables+ Buffer_1700_Oceano+ Buffer_1700_Suelo_arenoso + Buffer_1700_Rocas+ Buffer_1700_Grava", 
                             dredge=TRUE,  
                             SppNames = Species_Names)

#BUFFER 2200M DE LOS SITIOS DE MUESTREO
OccuInv37_2200 <- batchoccu2(pres = data_reg, 
                             sitecov = data_ocu, 
                             obscov = data_det, 
                             spp=N_Species , 
                             form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_2200_Bosque_Nativo+Buffer_2200_Pastizales+Buffer_2200_Matorrales+ Buffer_2200_Humedales+ Buffer_2200_Sup_impermeables+ Buffer_2200_Oceano+ Buffer_2200_Suelo_arenoso +Buffer_2200_Rocas+ Buffer_2200_Grava", 
                             dredge=TRUE,  
                             SppNames = Species_Names)

#BUFFER 2800M DE LOS SITIOS DE MUESTREO
OccuInv37_2800 <- batchoccu2(pres = data_reg, 
                             sitecov = data_ocu, 
                             obscov = data_det, 
                             spp=N_Species , 
                             form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_2800_Bosque_Nativo + Buffer_2800_Pastizale s+Buffer_2800_Matorrales + Buffer_2800_Humedales+ Buffer_2800_Sup_impermeables + Buffer_2800_Oceano+ Buffer_2800_Suelo_arenoso + Buffer_2800_Rocas+ Buffer_2800_Grava", 
                             dredge=TRUE,  
                             SppNames = Species_Names)

#BUFFER 3300M DE LOS SITIOS DE MUESTREO
OccuInv37_3300 <- batchoccu2(pres = data_reg, 
                             sitecov = data_ocu, 
                             obscov = data_det, 
                             spp=N_Species , 
                             form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_3300_Bosque_Nativo+Buffer_3300_Pastizales+Buffer_3300_Matorrales+ Buffer_3300_Humedales+ Buffer_3300_Sup_impermeables+ Buffer_3300_Oceano+ Buffer_3300_Suelo_arenoso +Buffer_3300_Rocas+ Buffer_3300_Grava", 
                             dredge=TRUE,  
                             SppNames = Species_Names)

#BUFFER 3900M DE LOS SITIOS DE MUESTREO
OccuInv37_3900 <- batchoccu2(pres = data_reg, 
                             sitecov = data_ocu, 
                             obscov = data_det, 
                             spp=N_Species , 
                             form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_3900_Bosque_Nativo+Buffer_3900_Pastizales+Buffer_3900_Matorrales+ Buffer_3900_Humedales+ Buffer_3900_Sup_impermeables+ Buffer_3900_Oceano+ Buffer_3900_Suelo_arenoso + Buffer_3900_Rocas+ Buffer_3900_Grava", 
                             dredge=TRUE,  
                             SppNames = Species_Names)

#BUFFER 4400M DE LOS SITIOS DE MUESTREO
OccuInv37_4400 <- batchoccu2(pres = data_reg, 
                             sitecov = data_ocu, 
                             obscov = data_det, 
                             spp=N_Species , 
                             form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_4400_Bosque_Nativo+Buffer_4400_Pastizales+Buffer_4400_Matorrales+ Buffer_4400_Humedales+ Buffer_4400_Sup_impermeables+ Buffer_4400_Oceano+ Buffer_4400_Suelo_arenoso + Buffer_4400_Rocas+ Buffer_4400_Grava", 
                             dredge=TRUE,  
                             SppNames = Species_Names)

#BUFFER 50000M DE LOS SITIOS DE MUESTREO
OccuInv37_5000 <- batchoccu2(pres = data_reg, 
                             sitecov = data_ocu, 
                             obscov = data_det, 
                             spp=N_Species , 
                             form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_5000_Bosque_Nativo+Buffer_5000_Pastizales+Buffer_5000_Matorrales+ Buffer_5000_Humedales+ Buffer_5000_Sup_impermeables+ Buffer_5000_Oceano+ Buffer_5000_Suelo_arenoso + Buffer_5000_Rocas+ Buffer_5000_Grava", 
                             dredge=TRUE,  
                             SppNames = Species_Names)

########################
#para generar data frame con resumen de modelos para todas las especies y todos los buffers

Distancias <- round(seq(from = 30, to = 5000, length.out = 10), -2)
Distancias[1] <- 30
SppNames = Species_Names

Mods <- list(OccuInv37_30, OccuInv37_600, OccuInv37_1100, OccuInv37_1700, OccuInv37_2200, OccuInv37_2800, OccuInv37_3300, OccuInv37_3900, OccuInv37_4400, OccuInv37_5000)

Todos <- list()

Presences_by_sp <- list.files(path = "Final_Ocurrences/", pattern = ".rds", full.names = T)


Todos <- list()

for(i in 1:length(Distancias)){
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
  Todos[[i]] <- PorSpp %>% purrr::reduce(bind_rows)
  message(paste("lista distancia", Distancias[i]))
}

Todos <- Todos %>% purrr::reduce(bind_rows)

write_csv(Todos, "Resumen_ocuInvNuevo_buffer.csv")

Todos <-read_csv("Resumen_ocuInvNuevo_buffer.csv")

Seleccionados <- Todos %>% 
  dplyr::select(-Parametro, -Estimador) %>% 
  group_by(Especies) %>% 
  dplyr::filter(AICc == min(AICc)) %>% 
  distinct() %>% 
  ungroup() %>%
  dplyr::group_by(Distancia) %>% 
  summarise(n = n())


ggplot(Seleccionados, aes(x = Distancia, y = n)) +
  geom_path() +
  theme_bw() 

PorEspecie <- Todos %>% 
  group_by(Especies) %>% 
  dplyr::filter(AICc == min(AICc)) %>% 
  ungroup() %>% dplyr::select(Especies, Distancia) %>% 
  dplyr::distinct() %>% 
  arrange(Distancia)

#######
##########################

####Primavera###

## OCUPANCIA PARA LAS TODAS LAS ESPECIES 

data_reg <- read_rds("Occdata_regPRIM.rds")

colnames(data_reg) <- str_replace_all(colnames(data_reg), " ", "_")

Species_Names <- unique(gsub('[0-9]+', '', colnames(data_reg)))

N_Species <- length(Species_Names)

data_det <-read_rds("Occdata_detPrim.rds")

data_ocu <-read_rds("Occdata_occu.rds")
data_ocu <- data_ocu %>% select(-Sitio)
colnames(data_ocu) <- str_replace_all(colnames(data_ocu), pattern = " ", "_")

data_ocu2 <- data_ocu %>% mutate_if(is.numeric, scale) #centro y escalamiento de variables-media 0, varianza1


######## Con solo algunas variables

# BUFFER 30 M DE LOS SITIOS DE MUESTREO
OccuInv37_30 <- batchoccu2(pres = data_reg, sitecov = data_ocu, obscov = data_det, 
                           spp=N_Species, 
                           form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_30_Pastizales+Buffer_30_Matorrales+ Buffer_30_Sup_impermeables+ Buffer_30_Oceano+ Buffer_30_Suelo_arenoso+Buffer_30_Rocas+ Buffer_30_Grava", 
                           dredge=TRUE,  
                           SppNames = Species_Names)


#BUFFER 600M DE LOS SITIOS DE MUESTREO
OccuInv37_600 <- batchoccu2(pres = data_reg, 
                            sitecov = data_ocu, 
                            obscov = data_det, 
                            spp=N_Species , 
                            form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_600_Bosque_Nativo + Buffer_600_Pastizales+Buffer_600_Matorrales+ Buffer_600_Humedales+ Buffer_600_Sup_impermeables+ Buffer_600_Oceano+ Buffer_600_Suelo_arenoso+Buffer_600_Rocas+ Buffer_600_Grava", 
                            dredge=TRUE,  
                            SppNames = Species_Names)

#BUFFER 1100M DE LOS SITIOS DE MUESTREO
OccuInv37_1100 <- batchoccu2(pres = data_reg, 
                             sitecov = data_ocu, 
                             obscov = data_det, 
                             spp=N_Species , 
                             form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_1100_Bosque_Nativo + Buffer_1100_Pastizales+Buffer_1100_Matorrales+ Buffer_1100_Humedales+ Buffer_1100_Sup_impermeables+ Buffer_1100_Oceano+ Buffer_1100_Suelo_arenoso+Buffer_1100_Rocas+ Buffer_1100_Grava ", 
                             dredge=TRUE,  
                             SppNames = Species_Names)

#BUFFER 1700M DE LOS SITIOS DE MUESTREO
OccuInv37_1700 <- batchoccu2(pres = data_reg, 
                             sitecov = data_ocu, 
                             obscov = data_det, 
                             spp=N_Species , 
                             form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_1700_Bosque_Nativo+Buffer_1700_Pastizales+Buffer_1700_Matorrales+ Buffer_1700_Humedales+ Buffer_1700_Sup_impermeables+ Buffer_1700_Oceano+ Buffer_1700_Suelo_arenoso + Buffer_1700_Rocas+ Buffer_1700_Grava", 
                             dredge=TRUE,  
                             SppNames = Species_Names)

#BUFFER 2200M DE LOS SITIOS DE MUESTREO
OccuInv37_2200 <- batchoccu2(pres = data_reg, 
                             sitecov = data_ocu, 
                             obscov = data_det, 
                             spp=N_Species , 
                             form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_2200_Bosque_Nativo+Buffer_2200_Pastizales+Buffer_2200_Matorrales+ Buffer_2200_Humedales+ Buffer_2200_Sup_impermeables+ Buffer_2200_Oceano+ Buffer_2200_Suelo_arenoso +Buffer_2200_Rocas+ Buffer_2200_Grava", 
                             dredge=TRUE,  
                             SppNames = Species_Names)

#BUFFER 2800M DE LOS SITIOS DE MUESTREO
OccuInv37_2800 <- batchoccu2(pres = data_reg, 
                             sitecov = data_ocu, 
                             obscov = data_det, 
                             spp=N_Species , 
                             form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_2800_Bosque_Nativo + Buffer_2800_Pastizale s+Buffer_2800_Matorrales + Buffer_2800_Humedales+ Buffer_2800_Sup_impermeables + Buffer_2800_Oceano+ Buffer_2800_Suelo_arenoso + Buffer_2800_Rocas+ Buffer_2800_Grava", 
                             dredge=TRUE,  
                             SppNames = Species_Names)

#BUFFER 3300M DE LOS SITIOS DE MUESTREO
OccuInv37_3300 <- batchoccu2(pres = data_reg, 
                             sitecov = data_ocu, 
                             obscov = data_det, 
                             spp=N_Species , 
                             form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_3300_Bosque_Nativo+Buffer_3300_Pastizales+Buffer_3300_Matorrales+ Buffer_3300_Humedales+ Buffer_3300_Sup_impermeables+ Buffer_3300_Oceano+ Buffer_3300_Suelo_arenoso +Buffer_3300_Rocas+ Buffer_3300_Grava", 
                             dredge=TRUE,  
                             SppNames = Species_Names)

#BUFFER 3900M DE LOS SITIOS DE MUESTREO
OccuInv37_3900 <- batchoccu2(pres = data_reg, 
                             sitecov = data_ocu, 
                             obscov = data_det, 
                             spp=N_Species , 
                             form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_3900_Bosque_Nativo+Buffer_3900_Pastizales+Buffer_3900_Matorrales+ Buffer_3900_Humedales+ Buffer_3900_Sup_impermeables+ Buffer_3900_Oceano+ Buffer_3900_Suelo_arenoso + Buffer_3900_Rocas+ Buffer_3900_Grava", 
                             dredge=TRUE,  
                             SppNames = Species_Names)

#BUFFER 4400M DE LOS SITIOS DE MUESTREO
OccuInv37_4400 <- batchoccu2(pres = data_reg, 
                             sitecov = data_ocu, 
                             obscov = data_det, 
                             spp=N_Species , 
                             form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_4400_Bosque_Nativo+Buffer_4400_Pastizales+Buffer_4400_Matorrales+ Buffer_4400_Humedales+ Buffer_4400_Sup_impermeables+ Buffer_4400_Oceano+ Buffer_4400_Suelo_arenoso + Buffer_4400_Rocas+ Buffer_4400_Grava", 
                             dredge=TRUE,  
                             SppNames = Species_Names)

#BUFFER 50000M DE LOS SITIOS DE MUESTREO
OccuInv37_5000 <- batchoccu2(pres = data_reg, 
                             sitecov = data_ocu, 
                             obscov = data_det, 
                             spp=N_Species , 
                             form= "~ Temperatura + Humedad + DirViento + RapViento + Agua ~ Buffer_5000_Bosque_Nativo+Buffer_5000_Pastizales+Buffer_5000_Matorrales+ Buffer_5000_Humedales+ Buffer_5000_Sup_impermeables+ Buffer_5000_Oceano+ Buffer_5000_Suelo_arenoso + Buffer_5000_Rocas+ Buffer_5000_Grava", 
                             dredge=TRUE,  
                             SppNames = Species_Names)

########################
#para generar data frame con resumen de modelos para todas las especies y todos los buffers

Distancias <- round(seq(from = 30, to = 5000, length.out = 10), -2)
Distancias[1] <- 30
SppNames = Species_Names

Mods <- list(OccuInv37_30, OccuInv37_600, OccuInv37_1100, OccuInv37_1700, OccuInv37_2200, OccuInv37_2800, OccuInv37_3300, OccuInv37_3900, OccuInv37_4400, OccuInv37_5000)

Todos <- list()

Presences_by_sp <- list.files(path = "Final_Ocurrences/", pattern = ".rds", full.names = T)


Todos <- list()

for(i in 1:length(Distancias)){
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
  Todos[[i]] <- PorSpp %>% purrr::reduce(bind_rows)
  message(paste("lista distancia", Distancias[i]))
}

Todos <- Todos %>% purrr::reduce(bind_rows)

write_csv(Todos, "Resumen_ocuPrimNuevo_buffer.csv")

Todos <-read_csv("Resumen_ocuPrimNuevo_buffer.csv")

Seleccionados <- Todos %>% 
  dplyr::select(-Parametro, -Estimador) %>% 
  group_by(Especies) %>% 
  dplyr::filter(AICc == min(AICc)) %>% 
  distinct() %>% 
  ungroup() %>%
  dplyr::group_by(Distancia) %>% 
  summarise(n = n())


ggplot(Seleccionados, aes(x = Distancia, y = n)) +
  geom_path() +
  theme_bw() 

PorEspecie <- Todos %>% 
  group_by(Especies) %>% 
  dplyr::filter(AICc == min(AICc)) %>% 
  ungroup() %>% dplyr::select(Especies, Distancia) %>% 
  dplyr::distinct() %>% 
  arrange(Distancia)

## Resumen temporadas

TodosPrim <-read_csv("Resumen_ocuPrimNuevo_buffer.csv")


PorEspeciePrim <- TodosPrim %>% 
  group_by(Especies) %>% 
  dplyr::filter(AICc == min(AICc)) %>% 
  ungroup() %>% dplyr::select(Especies, Distancia) %>% 
  dplyr::distinct() %>% 
  arrange(Distancia) %>% 
  rename(Distancia_Prim = Distancia)

TodosInv <-read_csv("Resumen_ocuInvNuevo_buffer.csv")


PorEspecieInv <- TodosInv %>% 
  group_by(Especies) %>% 
  dplyr::filter(AICc == min(AICc)) %>% 
  ungroup() %>% dplyr::select(Especies, Distancia) %>% 
  dplyr::distinct() %>% 
  arrange(Distancia) %>% 
  rename(Distancia_Inv = Distancia)

PorEspecie <- full_join(PorEspecieInv, PorEspeciePrim) %>% 
  dplyr::filter(Especies != "Larus_dominicanus") %>% 
  group_by(Especies) %>% 
  rowwise() %>% 
  mutate(Distancia=mean(c(Distancia_Inv, Distancia_Prim), na.rm=T)) %>% 
  arrange(Distancia) 
