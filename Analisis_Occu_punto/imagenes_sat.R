#imagenes satelitales

library(raster)
library(readr)

#construccion y caminos

build_up <- raster("19H_hbase_human_built_up_and_settlement_extent_utm_30m.tif")
plot(build_up)

values(build_up) <-ifelse(values(build_up) > 240, NA, values(build_up))
build_up <- projectRaster(build_up, crs = "+proj=longlat +datum=WGS84 +no_defs", method="ngb") #reproyeccion
build_up <- readAll(build_up)
write_rds(build_up, "Capa_build_up.rds")

build_up <- read_rds("Capa_build_up.rds")

#conviertiendo en dos capas: construccion y caminos
construccion <- build_up
values(construccion) <-  ifelse(values(build_up) != 201, NA, 1)
plot(construccion)
write_rds(construccion, "Capa_construccion.rds")

caminos <- build_up
values(caminos) <- ifelse(values(build_up) != 202, NA, 1)
plot(caminos)
write_rds(caminos, "Capa_caminos.rds")
