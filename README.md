Monitoreo visual
================

#### Documentación

Revisar para completar

### Ocupancia monitoreo visual

#### Preparación de capas de trabajo

Cada capa de trabajo para ocupancia se realizó de forma independiente y
fueron determinadas en base a las preguntas de investigación. El archivo
Prep\_capas muestra este trabajo tomando como referencia la capa
“altura” que determina la resolucion del raster.

Se utilizó un shape file de Chile politico contemplando frontera
nacional y división de comunas.Se trabajo solo con la máscara de las
comunas de valparaiso, vinña del mar y concon. Las capas creadas fueron
distancia a la costa a partir de la frontera de chile por el oeste, rios
de chile descargados de divagis, distancia a cuerpos de agua al interior
de las ciudades, distancia a caminos (NASA:build\_up), distancia a
construccion (NASA:build\_up) y poblacion de divagis. la capa build up
de la nasa era una y fue dividida en dos: caminos y contruccion
/imagenes\_sat.R creacion de capa “distancias” que incluye todas las
variables.

#### Preparación de datos para ocupancia

Dada la confeccion del paquete DiversityOccupancy, los datos deben estar
ordenados de una forma en particular. Para esto se debió crear un data
frame para los registros de especies por cada uno de los tres dias de
muestreo por sitio, un data frame para las variables de ocupancia para
cada sitio y una lista de todas las variables de detección para los
momentos en que se realizan los registros de especies. archivo Prep
datos\_Occu\_punto.R

Archivos para ocupancia: (capas deteccion, ocupancia y registros)

INVIERNO Occdata\_regInv.rds Occdata\_ocu.rds Occdata\_detInv.rds

PRIMAVERA Occdata\_regPRIM.rds Occdata\_ocu.rds Occdata\_detPrim.rds

Datos presencias

  - Invierno: /home/giorgia/Documents/Doctorado Tesis/Monitoreo
    aves/Muestreo Aves jun-jul
    2019/Monitoreo\_punto/Reg\_completo\_aves.csv

  - Primavera: /home/giorgia/Documents/Doctorado tesis/Monitoreo
    aves/Muestreo aves sep-oct 2019/Monitoreo
    punto/Registro\_aves\_veranoFINAL.csv

#### Ocupancia

El análisis de ocupancia se realizó para los datos de invierno y
primavera por separado.
