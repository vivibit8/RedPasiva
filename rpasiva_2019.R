###### Codigo Red Pasiva #####

# Directorio
setwd("C:/Users/Ale/Documents/R/Red pasiva/RedPasiva/RED_PASIVA")
getwd()


## cargar librerias
library(raster)
library(rgdal)
library(ggplot2)
library(sp)
library(maptools)
library(rgeos)
library(RStoolbox)
library(dismo)
library(gstat)
library(psych)
library(devtools)
library(ltm)
library(RColorBrewer)
library(dplyr)
library(hydroGOF)
library(automap)
library(deldir)
library(geospt)
library(leaflet)
library(lattice)
library(rasterVis)
library(fields)
library(e1071)


library(NLP)
library(hydroGOF)
library(knitr)
# area de estudio
# limite de Cuenca
area_estudio=readOGR("limite_urbano_cuenca_utm_wgs84.shp")
area_estudio


# Dioxido de Nitrogeno ----------------------------------------------------
## Primero se sube la capa de puntos del contaminante "NO2_2019"
no2=readOGR("NO2_2019.shp")


## Se va a hacer una interpolacion usando la Mediana de todos los meses, 
#en el shape ya esta calculado la mediana de cada estacion
puntos <- no2[,"MEDIANA_AN"]
names(puntos) <- c("variables")

codigo<-no2[,"CODIGO"]


# Grilla, se crea una vez para todos los contaminantes
r <- raster(area_estudio, res=30)
r[] <- 1:length(r)
r.c <- crop(r,extent(area_estudio))
r.m <- raster::mask(r.c, area_estudio)
r.sp<-as(r.m, "SpatialPixelsDataFrame")


#Funciones de interpolacion
# IDW, se crea para todos los contaminantes, es la funcion que va a realizar el IDW
idw <- function(puntos, r.sp){
  idw.cv <- krige.cv(variables~1, puntos) # Validacion cruzada IDW
  idw.cv <- gof(idw.cv$var1.pred, puntos$variables) #Estadisticos de la validacion cruzada
  
  idw = krige(variables ~ 1, puntos, r.sp) # interpolacion
  idw.raster <-  raster::raster(idw) #Transformar a formato raster
  values(idw.raster)[values(idw.raster) < 0] = 0 #Eliminar los valores menores a 0
  names(idw.raster) <- c("IDW") # Nombrar el raster
  
  return(list(idw.cv, idw.raster)) #Generar una lista los resultados de los estadisticos y mapa de interpolacion
}


# Interpolaciones con datos NO2
no2 <- idw(puntos, r.sp)

# Estadisticas del modelo
estadisticas <- as.data.frame(t(no2[[1]][c(2,4,6)]))
names(estadisticas) <- c("MAE","RMSE","PBIAS")
kable(estadisticas)

# Mapa de interpolacion
col_1 <- rasterTheme(region=rev(brewer.pal(8,"PuOr")))
levelplot(no2[[2]],par.settings =col_1,margin=F,main="INTERPOLACION NO2 - 2019") +layer(sp.polygons(area_estudio))+
  layer(sp.polygons(puntos, pch=16))


# Dioxido de azufre -------------------------------------------------------
## Primero se sube la capa de puntos del contaminante "SO2_2019"
so2=readOGR("SO2_2019.shp")

## Se va a hacer una interpolacion usando la Mediana de todos los meses, 
## en el shape ya esta calculado la mediana de cada estacion

puntos <- so2[,"MEDIANA_A"]
names(puntos) <- c("variables")

## Interpolaciones con datos SO2
# Interpolacion IDW
so2 <- idw(puntos, r.sp)

# Estadisticas del modelo
estadisticas <- as.data.frame(t(so2[[1]][c(2,4,6)]))
names(estadisticas) <- c("MAE","RMSE","PBIAS")
kable(estadisticas)

# Mapa de interpolacion
col_1 <- rasterTheme(region=rev(brewer.pal(8,"PuOr")))
levelplot(so2[[2]],par.settings =col_1,margin=F,main="INTERPOLACION SO2 - 2019") +layer(sp.polygons(area_estudio))+
  layer(sp.polygons(puntos, pch=16))


# Ozono -------------------------------------------------------------------
# Primero se sube la capa de puntos del contaminante "O3_2019"
o3=readOGR("O3_2019.shp")

# Se va a hacer una interpolacion usando la Mediana de las mediciones de todos los meses, 
# en el shape ya esta calculado la mediana de cada estacion
puntos <- o3[,"MEDIANA_A"]
names(puntos) <- c("variables")

# Ejecuta la interpolacion IDW
o3 <- idw(puntos, r.sp)

# Estadisticas del modelo
estadisticas <- as.data.frame(t(o3[[1]][c(2,4,6)]))
names(estadisticas) <- c("MAE","RMSE","PBIAS")
kable(estadisticas)

# Mapa de interpolacion
col_1 <- rasterTheme(region=rev(brewer.pal(8,"PuOr")))
levelplot(o3[[2]],par.settings =col_1,margin=F,main="INTERPOLACION O3 - 2019") +layer(sp.polygons(area_estudio))+
  layer(sp.polygons(puntos, pch=16))


# Particulas Sedimentables ------------------------------------------------
# Primero se sube la capa de puntos del contaminante "O3_2019"
ps=readOGR("PS_2019.shp")

# Se va a hacer una interpolacion usando la Mediana de las mediciones de todos los meses, 
# en el shape ya esta calculado la mediana de cada estacion

puntos <- ps[,"MEDIANA_A"]
names(puntos) <- c("variables")

# Ejecuta la interpolacion IDW
ps <- idw(puntos, r.sp)

# Estadisticas del modelo
estadisticas <- as.data.frame(t(ps[[1]][c(2,4,6)]))
names(estadisticas) <- c("MAE","RMSE","PBIAS")
kable(estadisticas)

# Mapa de interpolacion
col_1 <- rasterTheme(region=rev(brewer.pal(8,"PuOr")))
levelplot(ps[[2]],par.settings =col_1,margin=F,main="INTERPOLACION PS - 2019") +layer(sp.polygons(area_estudio))+
  layer(sp.polygons(puntos, pch=16))

