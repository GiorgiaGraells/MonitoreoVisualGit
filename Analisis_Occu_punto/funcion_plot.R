library(tidyverse)
library(DiversityOccupancy)
library(gridExtra)

#ocupancia trabajada en rstudio cloud
#archivo guardado en wd, nombre: Ocupancia45_avesInv.rds
Occu45 <-read_rds("Ocupancia45_avesInv.rds") 

batch = Occu45
spp = 8
variable = "Distancia_Costa"

test_plot <- function (batch, spp, variable, N = 50)
{
  upper <- lower <- NULL

  Vars <- batch$models[[spp]]@formula %>% str_split(pattern = "~", simplify = T) %>% .[3] %>% str_split(pattern = "\\+", simplify = T) %>% as.character() %>% str_trim() %>% str_remove_all("1")
  Vars <- Vars[str_count(Vars) != 0]
  
  model_data <- batch$Covs %>% select(Vars)
  
  stopifnot(variable %in% Vars)
  
  all_vars <- model_data %>% select(-one_of(variable))
  num_vars <- all_vars %>% select_if(is.numeric) %>% summarize_all(mean)
  cat_vars <- all_vars %>% select_if(Negate(is.numeric)) %>% purrr::map(unique)
  
  resp_var <- model_data %>% pull(variable) 
  if(is.numeric(resp_var)) {
    resp_vals <- seq(min(resp_var), max(resp_var), length.out=N)
  } else {
    resp_vals <- unique(resp_var)
  }
  
  new_data <- tidyr::crossing(num_vars, !!!cat_vars, !!variable:=resp_vals)
  if(nrow(new_data) < 1){
    new_data <- data.frame(resp_vals)
    colnames(new_data) <- variable
  }
  
  pred <- predict(batch$models[[spp]], newdata = as.data.frame(new_data), se_fit=TRUE, type= "state") %>% bind_cols(new_data)
  
  ## Plot the response
  my_aes <- aes(x= !!sym(variable), y = Predicted)
  line_aes <- aes(x= !!sym(variable), y = Predicted)
  if (length(cat_vars)==1) {
    my_aes[["fill"]] <- sym(names(cat_vars))
    line_aes[["colour"]] <- sym(names(cat_vars))
  } else if (length(cat_vars)>1) {
    my_aes[["fill"]] <- quo(interaction(!!!syms(names(cat_vars))))
    line_aes[["colour"]] <- quo(interaction(!!!syms(names(cat_vars))))
  }
  range_aes <- aes(ymax= upper, ymin = lower)
  result <- ggplot(pred, my_aes) + theme_classic() + ylab("Occupancy")
  if(is.numeric(resp_var)) {
      (if (length(cat_vars)>0) {
        result <- result + geom_ribbon(range_aes, alpha = 0.5) + geom_line(line_aes)
      } else {
        result <- result + geom_ribbon(range_aes, fill="grey", alpha = 0.5) + geom_line()
      }) 
  } else {
    result + 
      geom_errorbar(range_aes) + 
      geom_point() 
  }
  
}


#Graficos

a <-test_plot(batch = Occu45, spp = 2, variable = "CobVeg")#blanquillo
b <-test_plot(batch = Occu45, spp = 2, variable = "Altura")
c <- grid.arrange(a,b, top="Blanquillo")

d <-test_plot(batch = Occu45, spp = 3, variable = "Distancia_Costa")#chercan
d + ggtitle("Chercan")

e <-test_plot(batch = Occu45, spp = 8, variable = "Distancia_Costa")#cormoran
e + ggtitle("Cormoran")

f <-test_plot(batch = Occu45, spp = 11, variable = "Distancia_camino")#garuma
g <-test_plot(batch = Occu45, spp = 11, variable = "Distancia_Costa")#garuma
h <- grid.arrange(f,g, top="Gaviota garuma")

a1 <-test_plot(batch = Occu45, spp = 8, variable = "Distancia_Costa")+ ggtitle("Cormoran")#cormoran
a2 <-test_plot(batch = Occu45, spp = 21, variable = "Distancia_Costa")+ ggtitle("monja")#monja
a3 <-test_plot(batch = Occu45, spp = 22, variable = "Distancia_Costa")#pelicano
a4 <-test_plot(batch = Occu45, spp = 11, variable = "Distancia_Costa")#garuma
#a5 <-test_plot(batch = Occu45, spp = 24, variable = "Distancia_Costa")#pilpilen
a6 <-test_plot(batch = Occu45, spp = 2, variable = "Distancia_Costa")#blanquillo
a7 <-test_plot(batch = Occu45, spp = 10, variable = "Distancia_Costa")#franklin

grid.arrange(a1,a2,a3,a4,a6,a7, ncol=2)

b1 <-test_plot(batch = Occu45, spp = 29, variable = "Distancia_Costa")#tenca
b2 <-test_plot(batch = Occu45, spp =30, variable = "Distancia_Costa")#tiuque
test_plot(batch = Occu45, spp = 31, variable = "Distancia_Costa")#tortola
test_plot(batch = Occu45, spp = 32, variable = "Distancia_Costa")#zorzal
grid.arrange(f,g, ncol=2, nrow=2, top="Gaviota garuma")

