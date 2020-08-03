#Caracterización sitios muestreo

library(tidyverse)
library(ggplot2)

data_ocu <-read_rds("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Occdata_occu.rds")
colnames(data_ocu) <- str_replace_all(colnames(data_ocu), pattern = " ", "_")

data_ocu <-data_ocu %>%  dplyr::select(AMBIENTE, contains("2200")) 
colnames(data_ocu) <-colnames(data_ocu) %>%  str_remove_all(pattern = paste0("Buffer_",2200, "_"))

#CobVeg + Altura + Distancia_Agua+ Distancia_rio+ Pastizales+Matorrales + Oceano+ Sup_impermeables+ Suelo_arenoso +Grava +
#  Cultivos+ Bosque_Nativos

data_ocu <- data_ocu %>%  group_by(AMBIENTE) %>%  summarise_all(.funs = list(Mean = mean, SD= sd)) %>% pivot_longer(cols = contains(c("Mean", "SD")), names_to = "Variable", values_to = "Valor")


data_ocu_mean <- data_ocu %>% dplyr::filter(str_detect(Variable, "Mean")) %>% mutate(Variable = str_remove_all(Variable,"_Mean")) %>% rename(Media = Valor)
  
data_ocu_sd <-  data_ocu %>% dplyr::filter(str_detect(Variable, "SD")) %>% mutate(Variable = str_remove_all(Variable,"_SD")) %>% rename(SD = Valor) %>% full_join(data_ocu_mean)

Caract_amb <- data_ocu_sd %>% filter(Variable!="Nieve") %>% filter(Variable!="Nubes")

saveRDS(Caract_amb, "Caract_amb.rds")

ggplot(Caract_amb, aes(x = AMBIENTE, y = Media)) + geom_col(aes(fill = Variable), position = "dodge") + geom_errorbar(aes(group = Variable, ymax = Media + SD, ymin = Media - SD), position = "dodge") + facet_wrap(~AMBIENTE, scales = "free_x",)

