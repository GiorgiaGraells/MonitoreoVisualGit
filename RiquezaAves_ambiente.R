# para grafico figura 1 resultados
# riqueza promedio por ambiente+ ds... dos temporadas

library(tidyverse)
library(patchwork)

#Preparacion datos
BirdNames <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/BirdNames.csv") %>% dplyr::select(-X1)

#Riqueza invierno
AvesInv <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv") %>% 
  dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))%>% rename(Nombre =Especie)
AvesInv <- AvesInv %>% group_by(Sitio, Nombre, AMBIENTE) %>% summarise(n=sum(N_individuos)) %>% 
  mutate(Nombre= str_replace(Nombre, "PELICANO", "PELICANO COMÚN"),
         Nombre= str_replace(Nombre, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Nombre= str_replace(Nombre, "LILEN", "LILE"))

AvesInv <- left_join(x=AvesInv, y= BirdNames) 

#Grafico riqueza especies invierno
AvesInv_riq <- AvesInv %>% group_by(AMBIENTE,Sitio, Habitat) %>% summarise(Riqueza = n())

AvesInv_riq2 <- AvesInv_riq %>% group_by(AMBIENTE, Habitat) %>% summarise_at("Riqueza", .funs = list(mean=mean, max=max, min=min)) %>% 
  mutate(AMBIENTE=fct_relevel(AMBIENTE, "VERDE", "URBANO", "ROQUERIO NATURAL","ROQUERIO INTERVENIDO","PLAYA NATURAL","PLAYA INTERVENIDA"))

ggplot(AvesInv_riq2, aes(x=AMBIENTE, y=mean)) + geom_col(aes(fill=Habitat), position="dodge") +
  scale_fill_manual(aesthetics = c("fill", "color"),values = c( '#5ab4ac','#d8b365')) +  theme_bw()+ ylab("Riqueza de especies invierno")+
  xlab("Sitios de muestreo")+ geom_errorbar(aes(ymin=min, ymax=max), position = position_dodge2(width = 0.1, padding = 0.7))+ 
  facet_wrap(~AMBIENTE, scales = "free_x", ncol=2) +   theme(axis.text.x=element_blank())



# Riqueza primavera

AMB_nombres <-read_rds("Occdata_occu.rds") %>% dplyr::select(Sitio, AMBIENTE)

AvesPrim <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves sep-oct 2019/Monitoreo punto/Registro_aves_veranoFINAL.csv")%>% 
  rename(Nombre =Especie) 
AvesPrim <- left_join(AvesPrim, AMB_nombres)
AvesPrim <- AvesPrim %>% group_by(Sitio, Nombre, AMBIENTE) %>% summarise(n=sum(N_individuos)) %>% 
  mutate(Nombre= str_replace(Nombre, "PELICANO", "PELICANO COMÚN"),
         Nombre= str_replace(Nombre, "JOTE NEGRO", "JOTE CABEZA NEGRA"),
         Nombre= str_replace(Nombre, "LILEN", "LILE"),
         Nombre= str_replace(Nombre, "CAHUIL", "GAVIOTA CAHUIL")) %>% 
  mutate(AMBIENTE= str_replace(AMBIENTE, "ROCA INTERVENIDA", "ROQUERIO INTERVENIDO"),
         AMBIENTE= str_replace(AMBIENTE, "ROCA NATURAL", "ROQUERIO NATURAL"))

AvesPrim <- left_join(x=AvesPrim, y= BirdNames) 

#Grafico riqueza especies primavera

AvesPrim_riq <- AvesPrim %>% group_by(AMBIENTE,Sitio, Habitat) %>% summarise(Riqueza = n())

AvesPrim_riq2 <- AvesPrim_riq %>% group_by(AMBIENTE, Habitat) %>% summarise_at("Riqueza", .funs = list(mean=mean, max=max, min=min)) %>% 
  mutate(AMBIENTE=fct_relevel(AMBIENTE, "VERDE", "URBANO", "ROQUERIO NATURAL","ROQUERIO INTERVENIDO","PLAYA NATURAL","PLAYA INTERVENIDA"))

ggplot(AvesPrim_riq2, aes(x=AMBIENTE, y=mean)) + geom_col(aes(fill=Habitat), position="dodge") +
  scale_fill_manual(aesthetics = c("fill", "color"),values = c( '#5ab4ac','#d8b365')) +  theme_bw()+ ylab("Riqueza de especies invierno")+
  xlab("Sitios de muestreo")+ geom_errorbar(aes(ymin=min, ymax=max), position = position_dodge2(width = 0.1, padding = 0.7))+ 
  facet_wrap(~AMBIENTE, scales = "free_x", ncol=2) +   theme(axis.text.x=element_blank())



##################
#para anova
# Temporada, AMBIENTE, grado urbanizacion, Sitio (aleatorio), Habitat, Riqueza  glmm poisson

library(MuMIn)

nuevoinv <- AvesInv_riq %>% ungroup() %>% mutate(Temporada="Invierno") %>% 
  mutate(Ambiente = case_when(AMBIENTE == "PLAYA INTERVENIDA"| AMBIENTE =="PLAYA NATURAL"~"Playa",
                              AMBIENTE =="ROQUERIO INTERVENIDO"| AMBIENTE =="ROQUERIO NATURAL"~"Roquerio",
                              AMBIENTE =="URBANO" | AMBIENTE == "VERDE"~"Terrestre")) %>% 
  mutate(Grado_modificacion=case_when(AMBIENTE == "PLAYA INTERVENIDA"| AMBIENTE =="ROQUERIO INTERVENIDO" |AMBIENTE =="URBANO"~"Intervenido",
                                      AMBIENTE == "PLAYA NATURAL"| AMBIENTE =="ROQUERIO NATURAL" |AMBIENTE =="VERDE"~"Natural" ))

nuevoprim <- AvesPrim_riq %>% ungroup() %>% mutate(Temporada="Primavera") %>% 
  mutate(Ambiente = case_when(AMBIENTE == "PLAYA INTERVENIDA"| AMBIENTE =="PLAYA NATURAL"~"Playa",
                              AMBIENTE =="ROQUERIO INTERVENIDO"| AMBIENTE =="ROQUERIO NATURAL"~"Roquerio",
                              AMBIENTE =="URBANO" | AMBIENTE == "VERDE"~"Terrestre")) %>% 
  mutate(Grado_modificacion=case_when(AMBIENTE == "PLAYA INTERVENIDA"| AMBIENTE =="ROQUERIO INTERVENIDO" |AMBIENTE =="URBANO"~"Intervenido",
                                      AMBIENTE == "PLAYA NATURAL"| AMBIENTE =="ROQUERIO NATURAL" |AMBIENTE =="VERDE"~"Natural" ))

Riqueza <- bind_rows(nuevoinv, nuevoprim)

# GLMmixto con variables aleatorias
Modelo <- glm(Riqueza ~ Temporada + Ambiente + Grado_modificacion + Habitat +  Temporada:Ambiente + Ambiente:Grado_modificacion , family = poisson, data =Riqueza ) 
options(na.action = "na.fail")
#Seleccion de modelos

Selected <- dredge(Modelo, m.lim = c(0, floor(nrow(Riqueza)/10)))

Final <- subset(Selected, delta < 2)
Final_conpeso <- model.sel(Final)

Final_DF <- as.data.frame(Final_conpeso)
kable(Final_DF, digits = 2, booktabs=T) %>%  kable_styling(latex_options = c("scale_down"), font_size = 9)



NewData <- Riqueza %>% dplyr::select(-Riqueza, -Sitio, -AMBIENTE) %>% distinct()

Predicciones <- predict(Modelo, NewData, type = "response",se.fit = T)

NewData$Pred <- Predicciones$fit
NewData$SE <- Predicciones$se.fit

ggplot(NewData, aes(y=Pred, x=Grado_modificacion)) + geom_col(aes(fill=Habitat), position="dodge") +
  scale_fill_manual(aesthetics = c("fill", "color"),values = c( '#5ab4ac','#d8b365')) +  theme_bw()+ ylab("Riqueza de especies")+
   geom_errorbar(aes(ymin=Pred-SE, ymax=Pred+SE), position = position_dodge2(width = 0.1, padding = 0.7))+
  facet_grid(Temporada ~ Ambiente) +   theme(axis.text.x=element_blank())
