Monitoreo visual
================

#### Documentación

Revisar para completar

### Analisis comunitario de especies preliminar

Analisis preliminar: solo reagrupacion de datos **exploracion datos,
analisis preliminar.R**

Utilización del paquete vegan, archivo **AnalisisVegan.R**

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

Datos presencias:

  - Invierno: /home/giorgia/Documents/Doctorado Tesis/Monitoreo
    aves/Muestreo Aves jun-jul
    2019/Monitoreo\_punto/Reg\_completo\_aves.csv

  - Primavera: /home/giorgia/Documents/Doctorado tesis/Monitoreo
    aves/Muestreo aves sep-oct 2019/Monitoreo
    punto/Registro\_aves\_veranoFINAL.csv

#### Ocupancia

El análisis de ocupancia se realizó para los datos de invierno y
primavera por separado. Los análisis se enceuntran en los archivos
Analisis\_Occu\_Invierno.R y Analisis\_Occu

En esos mismo archivos se determinó la ocupancia y deteccion de las 6
especies usadas en las encuestas para cada ambiente

Existe un archivo llamado Prueba\_funcion\_nueva.R que armó Derek para
ocupancia de varias especies, en donde se continua con la siguiente
especies aun cuando la anterior no haya convergido en un modelo.

#### Junio

Se realiza ocupancia con función nueva utilizando nuevas variables
ambientales generadas en **MapaYuryi.R** que corresponde a capas de
cobertura vegetal en buffers (distancia desde los puntos de muestreo),
archivo de salida que incluye variables ambientales y buffers por
distancia: **Occdata\_ocu.rds** y un leaflet al final del código para
ver las capas por cada sitio en el mapa. La ocupancia se realiza en el
archivo **Occu37sp.R** utilizando las 37 especies primero para la
temporada de invierno. Se genera un data frame con el resumen de los
modelos, el cual se exporta como csv **Resumen\_ocuInv\_buffer.csv**
ubicado en la carpeta de ocupancia. Se determinó a mano los modelos con
valores significativos y se incluyó el valor de SE en
**Resumen\_ocuInv\_buffer.csv** mediante excel. El elemento
**ResModelosOcuBuffer.csv** muestra el resultado de los modelos de
ocupancia por distintcas capas de buffer, lo q determina las escalas. A
partir de esta tabla y según los valores de AICc, se eligieron los
valores más bajos para determinar la escala a trabajar Se corrió
ocupancia con laas variables ambientales y los porcentajes de cobertura
vegetal definidos para la escala buffer de 2200m radio, todos juntos.
Resultados están en **OccuInv37\_final.rds** y **OccuPrim37\_final.rds**
El resumen de resultados esta en una planilla excel llamada
**Resumen\_ocu\_37total.xlsx** ubicada en la carpeta
**Analisis\_Occu\_punto**.

Para poder comparar el efecto de los estimadores por igual se
estandarizaron las variables mediante un escalamiento (también en
**Occu37sp.R**). El escalamiento se realizó para primavera e invierno y
se corrió la ocupancia nuevamente para las variables seleccionadas que
se habian identificado como las más importantes (algunas variables no se
presentaban en el mapa revisado de buffers del leaflet). Se guardan los
valores de estimadores en un data frame: PorSppInv- PorSppPrim para
graficar tipo meta-analisis (guardado como **meta\_ocupancia.rds**,
incluuir funcion **FUN**)

existe el archivo **Prueba\_Funcion\_Nueva.R** que contiene otros
analisis de ocupancia realizados

Se genera un nuevo archivo llamado **PredOccuAmb\_37sp.R** donde se
rescatan los datos de predicciones de la ocupancia realizada y se arman
graficos de todas las especies para mostrar el rango de ocupancia dde
las especies para cada ambiente para primavera e invierno. Se modificó
la funcion de ocupancia, ahora llamada DiversityOccupancy::batchoccu3,
en donde se fuerza a generar un modelo con el AMBIENTE para cada
especie. Se ve la oacupancia por sitio Ocupancia con ambiente como
variable forzada. Se corrio ocupancia con forzamiento de ambiente y
todas las otras variables definidas en el ppt seleccionadas con
anteriorirdad (incluidos los buffers de 2200 (prim e inv). Se obtienen
dos resultados: para cada ambiente (considerando el promedio de cada
variable para cada ambiente) y para cada sitio considerando el valor de
cada variable, se saca tb el mejor modelo y queda en una columna. Se
guardan los resultados como **ResultadosInv.rds**,
**ResultadosPrim.rds**, **PorSitioInv.rds** y **PorSitioPrim.rds** y
**PredOccuSitio.rds**

Se corre nmsd en vegan y se hacen varios graficos
bacanes

#### Ordenando los resultados y buscando una explicacion: 1. Marco conceptual rasgos funcionales

Se crearon distintos archivos: **Funcionales aves.R** Se toman los
nombres de todas las especies desde el archivo Bird\_guilts.xlsx, se
descargaron los rasgos funcionales de aves (alimenticios y de habitat) y
se filtro para todas las aves, idependiente de la temporada

**Clusters ocupancia.R** para ver la agrupacion de especies segun
variables de ocupancia para invierno y primavera y luego generar los
clusters para rasgos funcionales

Archivo **Ensamble\_func.R** con graficos y analisis exploratorios de
ensamble de aves y su posible explicacion con rasgos funcionales.
Resumen de modelos e importancia de variables.- se obtiene el df
Resumen\_Modelos (para inv y prim)

— analisis exploraorios con rasgos funcionales, pero se presentan
errores desde informaciond e ocupancia por nombre comun de especies
(ademas dde elevados valores de ocupancia para mas de 42 sp) 14 agoosto:
Modificacion de codigo para trabajar ocupancias con nombre de especies
en vez de nombre comun de aves. Modificacion enarcihvo de preparacion de
cpas ocupancia y cambio de elementos Occdata\_regPRIM.rds y
Occdata\_regInv.rds guardados en MonitoreoVisualGit

Se cambio el nombre de las especies por los nombres cientificos y se
corrio ocupancia para todas las sp nuevamente. Graficos Tmabien se vio
analisis NMDS y ensambles con nuevos y mejores colores

### Exploración de datos en base a reunion 18 agosto

Se acuerda generar una grafico de riqueza de especies por ambiente,
separado en marino-terrestre Para Presencia especies, abundancia y
probabilidad de ocupancia realizar: - NMDS + ANOSIM - SIMPER - BIOENV

Se borró script de exploración de datos en donde se habia comenzado con
la exploración, pero fue cocntinuada en el script AnalisisVegan Se geneó
un nuevo script llamado **Exploracion\_enfocada.R**, en donde se
resuelve lo conversado en reunion del 18 agosto con stefan.

grafico de riqueza por ambiente, estacional

vegan para abundancia por especie invierno y primavera: NMDS, Anosim,
Simper vegan para abundancia por especie invierno y primaveera: envfit

analisis listos para abundancia, comp especies y ocupancia

modificacion de parametros de distintas funciones para su correcto uso

Creacion de archivo **Comportamineto\_Aves.R** para resumen de
actividades
