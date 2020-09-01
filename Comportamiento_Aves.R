#Comportamiento aves por ambiente

ComportamientoInv <- read_csv("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Muestreo aves jun-jul 2019/Monitoreo_punto/Reg_completo_aves_inv.csv") %>% dplyr::mutate(Especie = str_trim(str_remove_all(Especie,pattern = "JUVENIL")))
ComportamientoInv <- ComportamientoInv %>% dplyr::select(AMBIENTE, Sitio, Especie, Comportamiento) %>% 
  mutate(Comportamiento=as.character(Comportamiento),
         Comportamiento= str_replace(Comportamiento, "1", "Nadando o caminando en el agua"),
         Comportamiento= str_replace(Comportamiento, "2", "En el suelo -tierra, pasto, arena, roca"),
         Comportamiento= str_replace(Comportamiento, "3", "Entre el follaJe -en árbol o arbusto"),
         Comportamiento= str_replace(Comportamiento, "4", "Posado en altura -poste o cable"),
         Comportamiento= str_replace(Comportamiento, "5", "Volando o planeando"),
         Comportamiento= str_replace(Comportamiento, "6", "En un nido") )
  



group_by(Sitio, Especie) %>% summarise(n=sum(N_individuos)) %>% spread(key = Especie, value = n, fill=0) %>% ungroup
AvesInv <- AvesInv %>% dplyr::select(-Sitio)