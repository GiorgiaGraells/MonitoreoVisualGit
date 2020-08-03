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

data_ocu <-read_rds("Occdata_ocu.rds")
data_ocu <- data_ocu %>% dplyr::select(-Sitio)
colnames(data_ocu) <- str_replace_all(colnames(data_ocu), pattern = " ", "_")

data_reg <-read_rds("Occdata_regPRIM.rds")


Spp <- colnames(data_reg) %>% str_remove_all("\\d") %>% unique()

Nuevos_Datos <- data.frame(AMBIENTE = unique(data_ocu$AMBIENTE), Pred = NA, SE = NA, Spp = NA, Up = NA, Down = NA)

Resultados <- list()

for(i in 1:length(Spp)){
  data_reg_temp <- data_reg %>% dplyr::select(starts_with(Spp[i]))
  
  data_reg_temp <- data_reg_temp[,1:3]
  
  message("Ajustando el modelo")
  OccuPrim_temp <- batchoccu3(pres = data_reg_temp, sitecov = data_ocu, obscov = data_det, spp=1,  form= "~1 ~ AMBIENTE", dredge=TRUE, SppNames =Spp[i])
  
  Nuevos_Datos_Temp <- Nuevos_Datos
  
  Nuevos_Datos_Temp$Spp <- Spp[i]
  
  message(paste("Prediciendo occupancia", dim(Nuevos_Datos), Spp[i]))
  
  Nuevos_Datos_Temp$Pred <- predict(OccuPrim_temp$models[[1]], type = "state", newdata = Nuevos_Datos)$Predicted
  
  message("Prediciendo SE")
  
  Nuevos_Datos_Temp$SE <- predict(OccuPrim_temp$models[[1]], type = "state", newdata = Nuevos_Datos)$SE
  
  message("Prediciendo Limites")
  
  Nuevos_Datos_Temp$Up <- predict(OccuPrim_temp$models[[1]], type = "state", newdata = Nuevos_Datos)$upper
  
  Nuevos_Datos_Temp$Down <- predict(OccuPrim_temp$models[[1]], type = "state", newdata = Nuevos_Datos)$lower
  Resultados[[i]] <- Nuevos_Datos_Temp
  message(i)
}

Resultados <- Resultados %>% reduce(bind_rows)

##############


#Ocupancia para todas las especies en invierno

data_det <-read_rds("Occdata_detInv.rds")

data_ocu <-read_rds("Occdata_ocu.rds")
data_ocu <- data_ocu %>% dplyr::select(-Sitio)
colnames(data_ocu) <- str_replace_all(colnames(data_ocu), pattern = " ", "_")

data_reg <-read_rds("Occdata_regInv.rds")


Spp <- colnames(data_reg) %>% str_remove_all("\\d") %>% unique()

Nuevos_Datos <- data.frame(AMBIENTE = unique(data_ocu$AMBIENTE), Pred = NA, SE = NA, Spp = NA, Up = NA, Down = NA)

Resultados2 <- list()

for(i in 1:length(Spp)){
  data_reg_temp <- data_reg %>% dplyr::select(starts_with(Spp[i]))
  
  data_reg_temp <- data_reg_temp[,1:3]
  
  message("Ajustando el modelo")
  OccuInv_temp <- batchoccu2(pres = data_reg_temp, sitecov = data_ocu, obscov = data_det, spp=1,  form= "~1 ~ AMBIENTE", dredge=FALSE, SppNames =Spp[i])
  
  Nuevos_Datos_Temp <- Nuevos_Datos
  
  Nuevos_Datos_Temp$Spp <- Spp[i]
  
  message(paste("Prediciendo occupancia", dim(Nuevos_Datos), Spp[i]))
  
  Nuevos_Datos_Temp$Pred <- predict(OccuInv_temp$models[[1]], type = "state", newdata = Nuevos_Datos)$Predicted
  
  message("Prediciendo SE")
  
  Nuevos_Datos_Temp$SE <- predict(OccuInv_temp$models[[1]], type = "state", newdata = Nuevos_Datos)$SE
  
  message("Prediciendo Limites")
  
  Nuevos_Datos_Temp$Up <- predict(OccuInv_temp$models[[1]], type = "state", newdata = Nuevos_Datos)$upper
  
  Nuevos_Datos_Temp$Down <- predict(OccuInv_temp$models[[1]], type = "state", newdata = Nuevos_Datos)$lower
  Resultados2[[i]] <- Nuevos_Datos_Temp
  message(i)
}


Resultados2 <- Resultados2 %>% reduce(bind_rows)

###


#PRIM
Resultados <- Resultados %>% mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO", "VERDE", "ROCA INTERVENIDA", "PLAYA INTERVENIDA", "PLAYA NATURAL"))
ggplot(Resultados, aes(x=AMBIENTE, y=Pred))+ geom_point(aes(color = Spp)) + geom_errorbar(aes(ymax = Pred + SE, ymin = Pred - SE, color = Spp)) + 
  xlab("Ambientes")+ylab("Predicción de presencia") + facet_wrap(~Spp)+theme_classic()+ theme(axis.text.x = element_text(angle=70, vjust= 1, hjust=1), legend.position = "none")

#El mismo grafico pero sin color por especie
ggplot(Resultados, aes(x=AMBIENTE, y=Pred))+ geom_point() + geom_errorbar(aes(ymax = Pred + SE, ymin = Pred - SE)) + ylim(c(0,1.05))+
  xlab("Ambientes")+ylab("Predicción de presencia") + facet_wrap(~Spp)+theme_classic()+ theme(axis.text.x = element_text(angle=70, vjust= 1, hjust=1), legend.position = "none")


#INV
Resultados2 <- Resultados2 %>% mutate(AMBIENTE=fct_relevel(AMBIENTE, "URBANO", "VERDE", "ROCA INTERVENIDA", "PLAYA INTERVENIDA", "PLAYA NATURAL"))
ggplot(Resultados2, aes(x=AMBIENTE, y=Pred))+ geom_point(aes(color = Spp)) + geom_errorbar(aes(ymax = Pred + SE, ymin = Pred - SE, color = Spp)) + ylim(c(0,1.05))+
  xlab("Ambientes")+ylab("Predicción de presencia") + facet_wrap(~Spp)+theme_classic()+ theme(axis.text.x = element_text(angle=70, vjust= 1, hjust=1), legend.position = "none")

#El mismo grafico pero sin color por especie
ggplot(Resultados2, aes(x=AMBIENTE, y=Pred))+ geom_point() + geom_errorbar(aes(ymax = Pred + SE, ymin = Pred - SE)) + ylim(c(0,1.05))+
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
PredOccuAmb_Inv <- Resultados2 %>%  dplyr::select(-SE) %>% dplyr::select(-Up) %>% dplyr::select(-Down)%>%  
  group_by(AMBIENTE,Spp) %>% spread(key = Spp, value = Pred, fill=0) %>% ungroup

#saco columna de ambientes, pero queda en el orde de intervencion humana que se utilizo para graficar:urbano/verde/roca-int/playa-int/playa-nat/roca-nat
rownames(PredOccuAmb_Inv) <-PredOccuAmb_Inv$AMBIENTE
PredOccuAmb_Inv <- PredOccuAmb_Inv %>% dplyr::select(-AMBIENTE)



####
#para juntar la ocupancia de invierrno y primavera parra poder comparar entre ellas

#Inv
PredOccuAmb_Inv <- Resultados2 %>%  dplyr::select(-SE) %>% dplyr::select(-Up) %>% dplyr::select(-Down)%>%  
  group_by(AMBIENTE,Spp) %>% spread(key = Spp, value = Pred, fill=0) %>% ungroup

#PredOccuAmb_Inv$AMBIENTE <- paste0('I-', PredOccuAmb_Inv$AMBIENTE)

#Prim
PredOccuAmb_Prim <- Resultados %>%  dplyr::select(-SE) %>% dplyr::select(-Up) %>% dplyr::select(-Down)%>%  
  group_by(AMBIENTE,Spp) %>% spread(key = Spp, value = Pred, fill=0) %>% ungroup



#Uniendo ambas temporadas orden invierno-primavera
PredOccuAmb <- bind_rows(PredOccuAmb_Inv, PredOccuAmb_Prim) %>%  arrange(AMBIENTE, desc())

#saco columna de ambientes, pero queda en el orden de intervencion humana que se utilizo para graficar:urbano/verde/roca-int/playa-int/playa-nat/roca-nat
PredOccuAmb <- PredOccuAmb %>% dplyr::select(-AMBIENTE)

#Reemplazando NA por valores dde cero

PredOccuAmb[is.na(PredOccuAmb)] = 0

############variables ambientales
Amb<- read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/Analisis_Occu_punto/Occdata_ocu.rds")
Sitio_Amb <- Amb %>% dplyr::select(Sitio, AMBIENTE)

#########################
#ordination by NMDS

NMDSPredOccuAmb <- metaMDS(PredOccuAmb, distance = "bray", k = 2)

#visualizacion
plot(NMDSPredOccuAmb, type = "t", display = "species")
plot(NMDSPredOccuAmb, type = "t", display = "sites")


#mejorando la visualizacion
co=c("red", "green", "blue", "brown", "yellow")
shape= c(18,16)

#Seguir con las indicaciones de youtube

###Prediccion de ocupancia por sitio



