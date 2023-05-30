# cargar librerias
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(ggplot2)
library(readxl)
library(vctrs)

# Cargar funciones --------------------------------------------------------
#km = read_sf("C:/Users/alanp/Documents/5to/cs datos espaciales/tarea2/cuenca.kml")
km = read_sf("/Users/itallo/Documents/GitHub/Tarea2_datos_espaciales/cuenca.kml")

km = mutate(km, Description = "Rio Aconcagua",altura = 1021)

#img.folder = "C:/Users/alanp/Documents/5to/cs datos espaciales/tarea2/landsat"
img.folder = "/Users/itallo/Documents/GitHub/Tarea2_datos_espaciales/landsat"


#files = list.files(img.folder, pattern = "SR_B", full.names = TRUE)
files = list.files(img.folder, pattern = "B", full.names = TRUE)

imgs = rast(files)
imgs

img_ext = ext(imgs)
img_ext

img_ext[2]-img_ext[1]
img_ext[4]-img_ext[3]


st_crs(imgs) == st_crs(km)

img.crs = st_crs(imgs)
img.crs

v = st_transform(x = km, crs = img.crs)

st_crs(imgs) == st_crs(v)
imgs.c = crop(imgs, vect(v))
plot(imgs.c)

imgs.m = mask(imgs, vect(v))
plot(imgs.m)

imgs.cm = crop(imgs.m, vect(v))
plot(imgs.cm)
km
imgs.cm


imgs.r = project(imgs.cm, vect(km), method = "near")
plot(imgs.r[[5]])

options(scipen = 999)

#lc.file = "C:/Users/alanp/Documents/5to/cs datos espaciales/tarea2/LC_CHILE_2014_b.tif"
lc.file = "/Users/itallo/Documents/GitHub/Tarea2_datos_espaciales/LC_CHILE_2014_b.tif"

lc = rast(lc.file)
lc
# proyecamos el lc a la proyeccion de la imagen satelital
lc.proj = project(lc, imgs.r[[5]], method = "bilinear") # metodo bilinear para datos categoricos
lc.crop = crop(lc.proj, imgs.r[[5]]) # cortamos el lc al extent de la imagen
plot(lc.crop)

# En este caso vamos a considerar solo las categorias de cultivos(100), bosques(200), praderas(300) y matorrales (400)

# Modificar pixeles de un raster
lc.crop[lc.crop >= 500] = NA # selecciona todos los pixeles diferentes a 100, 200, 300 y 400 y les asiga NA
lc.crop[lc.crop < 100] = NA
lc.crop[lc.crop < 200 & lc.crop >=100] = 100
lc.crop[lc.crop < 300 & lc.crop >=200] = 200
lc.crop[lc.crop < 400 & lc.crop >=300] = 300
lc.crop[lc.crop < 500 & lc.crop >=400] = 400

# guardamos el LandCover reclasificado
writeRaster(lc.crop, "C:/Users/alanp/Documents/5to/cs datos espaciales/tarea2/LC_reclasificado.tif", overwrite = TRUE)

plot(lc.crop, col = c("yellow","purple","blue","green"), 
     main = "Landcover por categorias")# plotear lc cortado, cambiamos color de las categorias

# Convertir lc.crop en un vector
lc.crop_vec <- as.vector(lc.crop)

# Obtener la tabla de frecuencias de las categor?as de color
color_counts <- table(lc.crop_vec)

# Crear un gr?fico de barras de la cantidad por categoria
text(x = barplot(table(lc.crop_vec), col = c("yellow", "purple", "blue", "green"),
                 main = "Cantidad por categoria",
                 xlab = "Categoria", ylab = "Cantidad",
                 ylim= c(0,6500000)),
     y = table(lc.crop_vec),
     labels = table(lc.crop_vec),
     pos = 3)

separate_eight_day_composite = function(x, fechas){
  # dias que faltan para terminar el mes
  days.dif = 1+(days_in_month(fechas) - day(fechas))
  
  # si faltan menos de 8 dias entonces la imagen comprende datos que pertenecen al mes siguiente
  index = grep(TRUE, days.dif<8)
  
  # no considerar la ultima imagen del año
  index = index[-length(index)]
  
  # seleccionar imagenes, fechas y dias de las imagenes que abarcan mas de un mes
  x_i = x[[index]]
  fechas_img = fechas[index]
  days.left = days.dif[index]
  
  # raster vacio para guardar resultados
  r1 = rast()
  
  # guardar las imagenes que abarcan solo 1 mes dentro de la composicion de 8 días
  r2 = x[[-index]]
  
  # cantidad de imagenes que abarcan mas de un mes
  n = length(fechas_img)
  
  # ciclo de 1 a n
  for (i in 1:n) {
    # obtener la fecha del último dia que abarca la serie de 8 días
    fecha2 = fechas_img[i] %m+% days(7)
    
    # Multiplicamos la imagen por la proporcion de días que abarca de cada mes
    # mes actual
    x1 = x_i[[i]] * (days.left[i]/8)
    # mes siguiente
    x2 = x_i[[i]] * (1-days.left[i]/8)
    # asignar fecha correspondiente a cada imagen
    names(x1) = fechas_img[i]
    names(x2) = fecha2
    # guardar las nuevas imagenes en un vector
    r1 = c(r1, x1, x2)
  }
  r = c(r1,r2)
  return(r)
}

# Funcion para calcular imagenes mensuales a partir de imagenes diarias
# x : imagenes diarias
# dates : vector con las fechas de cada imagen en formato YYYY-MM-DD
# fun : funcion a aplicar a las imagenes diarias "mean" para media, "sum" para suma
daily_to_monthly = function(x, dates, fun = "mean"){
  # aproxima al primer dia del mes de cada fecha en dates
  mes = floor_date(as_date(dates), unit = "month")
  
  # lista de meses unicos
  lista.meses = mes %>% unique
  
  # numero de meses
  n = length(lista.meses)
  
  # dummy raster para guardar los valores mensuales
  x.mensual = rast()
  
  # iterar i de 1 a n => i = {1,2,3,...n}
  for (i in 1:n) {
    print(lista.meses[i])
    # obtener la posicion de las imagenes que coinciden con el mes_i
    posicion = grep(lista.meses[i], mes)
    print(length(posicion))
    # calcular la media o suma de todos las imagenes del mes_i
    if (fun == "mean"){
      r = mean(x[[posicion]], na.rm = TRUE)
    }else if(fun == "sum"){
      r = sum(x[[posicion]], na.rm = TRUE)
    }else{
      message("No se reconoce la funcion utilizada, escriba: fun = sum o mean como argumento")
    }
    # agregar el resultado al dummy raster
    x.mensual = c(x.mensual, r)
  }
  # asignar la fecha a cada imagen
  names(x.mensual) = lista.meses
  # retornar rasters de valores mensuales
  return(x.mensual)
}


# Funcion para calcular imagenes anuales a partir de imagenes mensuales
# x : imagenes mensuales
# dates : vector con las fechas de cada imagen en formato YYYY-MM-DD
# fun : funcion a aplicar a las imagenes diarias "mean" para media, "sum" para suma
to_yearly = function(x, dates, fun = "mean"){
  # aproxima al primer dia del mes de cada fecha en dates
  y = floor_date(as_date(dates), unit = "year")
  
  # lista de meses unicos
  lista.y = y %>% unique
  
  # numero de meses
  n = length(lista.y)
  
  # dummy raster para guardar los valores mensuales
  x.anual = rast()
  
  # iterar i de 1 a n => i = {1,2,3,...n}
  for (i in 1:n) {
    # obtener la posicion de las imagenes que coinciden con el mes_i
    posicion = grep(lista.y[i], y)
    # calcular la media de todos las imagenes del mes_i
    if (fun == "mean"){
      r = mean(x[[posicion]], na.rm = TRUE)
    }else if(fun == "sum"){
      r = sum(x[[posicion]], na.rm = TRUE)
    }else{
      message("No se reconoce la funcion utilizada, escriba: fun = sum o mean como argumento")
    }
    # agregar el resultado al dummy raster
    x.anual = c(x.anual, r)
  }
  # asignar la fecha a cada imagen
  names(x.anual) = lista.y
  # retornar rasters de valores mensuales
  return(x.anual)
}


# Comparacion punto-pixel con estaciones meteorologicas -------------------

# ubicacion de las estaciones
est = tibble(
  ID = c(350021, 350018), 
  nombre = c("CauquenesINIA", "CauquenesESSAM"), 
  lon = c(-72.289999, -72.332499),
  lat = c(-35.956111, -35.972221)
)

# transformar tabla a objeto espacial
est = st_as_sf(est, coords = c("lon","lat"), crs = 4326)
est

# Datos de las estaciones
INIA.data = read_excel("./data/table/Cauquenes_INIA_historico_pp_tmax_tmin_1964_2022_diaria.xlsx")
ESSAM.data = read_excel("./data/table/Cauquenes_ESSAM_pp_1979_2022_diaria.xlsx")

head(INIA.data)
tail(INIA.data)
head(ESSAM.data)
tail(ESSAM.data)

# renombrar columnas
ESSAM.data = rename(ESSAM.data, fecha = 1, PP = 2)

# crear columna con nombre de la estacion
INIA.data = INIA.data %>% mutate(estacion = "CauquenesINIA")
ESSAM.data = ESSAM.data %>% mutate(estacion = "CauquenesESSAM")

# juntar las filas de ambas tablas
est.data = bind_rows(INIA.data, ESSAM.data) %>% 
  select(fecha, PP, estacion) %>% 
  mutate(fecha = as_date(fecha))
est.data

# cargar datos espaciales de precipitacion CR2MET2.5
pp = rast("data/raster/PPCR2MET2.5/CR2MET2.5_PP_DAY_1960_2021.nc")
pp

# vector de fechas
fechas = seq(
  ymd("1960-01-01"),
  ymd("2021-12-31"),
  by = "days")

# asignar nombre a las capas
names(pp) = fechas

# extraer datos de las estaciones
extr = terra::extract(pp, est, dataframe = TRUE) %>% 
  tibble() %>% 
  select(-ID) %>% 
  mutate(estacion = est$nombre) %>% 
  pivot_longer(cols = 1:ncol(.)-1, names_to = "fecha", values_to = "pp_cr2met") %>% 
  mutate(fecha = as_date(fecha))
extr

# Juntar datos de la estacion con los datos extraidos del raster
pp.data = full_join(extr, est.data, by = c("fecha","estacion")) %>% 
  rename(pp_est = PP)
pp.data

# Correlación entre datos medidios y datos modelados
pp.data %>% ggplot(aes(x = pp_est, y = pp_cr2met))+ #definir ejes
  geom_point()+ #agregar grafico de puntos/dispersion
  geom_abline(color = "red")+ #agregar linea 1:1
  geom_smooth(method = "lm")+ # agregar linea de ajuste lineal
  ggpubr::stat_cor()+ #agregar correlacion (R)
  tune::coord_obs_pred()+ # tamaño del eje x = tamaño del eje y (grafico cuadrado)
  facet_wrap(.~estacion)+ # separar los graficos por estacion
  labs(x = "Precipitación Estación Meteorológica", y = "Precipitación CR2MET",
       title = "Correlación entre precipitación diaria observada y modelada")

# serie de tiempo
pp.data %>% ggplot()+
  geom_line(aes(x = fecha, y = pp_est,color = "PP observada"))+
  geom_line(aes(x = fecha, y = pp_cr2met, color = "PP CR2MET"))+
  facet_wrap(.~ estacion, nrow = 2)+
  scale_x_date(date_labels = "%Y-%m", date_breaks = "4 year",
               limits = c(ymd("1960-01-01"), ymd("2021-12-31")))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  labs(x = "Tiempo", y = "Precipitación (mm)",
       title = "Serie de tiempo de precipitación diaria",
       color = "Fuente")

# calcular precipitacion acumulada mensual
pp.data.month = pp.data %>%
  mutate(fecha = floor_date(fecha, unit = "month")) %>% 
  group_by(estacion, fecha) %>% 
  summarise_all(sum)

# correlacion datos mensuales
pp.data.month %>% ggplot(aes(x = pp_est, y = pp_cr2met))+ #definir ejes
  geom_point()+ #agregar grafico de puntos/dispersion
  geom_abline(color = "red")+ #agregar linea 1:1
  geom_smooth(method = "lm")+ # agregar linea de ajuste lineal
  ggpubr::stat_cor()+ #agregar correlacion (R)
  tune::coord_obs_pred()+ # tamaño del eje x = tamaño del eje y (grafico cuadrado)
  facet_wrap(.~estacion)+ # separar los graficos por estacion
  labs(x = "Precipitación Estación Meteorológica", y = "Precipitación CR2MET",
       title = "Correlación entre precipitación mensual observada y modelada")

# serie de tiempo datos mensuales
pp.data.month %>% ggplot()+
  geom_line(aes(x = fecha, y = pp_est,color = "PP observada"))+
  geom_line(aes(x = fecha, y = pp_cr2met, color = "PP CR2MET"))+
  facet_wrap(.~ estacion, nrow = 2)+
  scale_x_date(date_labels = "%Y-%m", date_breaks = "4 year",
               limits = c(ymd("2000-01-01"), ymd("2021-12-31")))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  labs(x = "Tiempo", y = "Precipitación (mm)",
       title = "Serie de tiempo de precipitación mensual",
       color = "Fuente")

# calcular precipitacion acumulada mensual
pp.data.year = pp.data %>%
  mutate(fecha = floor_date(fecha, unit = "year")) %>% 
  group_by(estacion, fecha) %>% 
  summarise_all(sum)

# correlacion datos anuales
pp.data.year %>% ggplot(aes(x = pp_est, y = pp_cr2met))+ #definir ejes
  geom_point()+ #agregar grafico de puntos/dispersion
  geom_abline(color = "red")+ #agregar linea 1:1
  geom_smooth(method = "lm")+ # agregar linea de ajuste lineal
  ggpubr::stat_cor()+ #agregar correlacion (R)
  tune::coord_obs_pred()+ # tamaño del eje x = tamaño del eje y (grafico cuadrado)
  facet_wrap(.~estacion)+ # separar los graficos por estacion
  labs(x = "Precipitación Estación Meteorológica", y = "Precipitación CR2MET",
       title = "Correlación entre precipitación anual observada y modelada")

# serie de tiempo datos anuales
pp.data.year %>% ggplot()+
  geom_line(aes(x = fecha, y = pp_est,color = "PP observada"))+
  geom_line(aes(x = fecha, y = pp_cr2met, color = "PP CR2MET"))+
  facet_wrap(.~ estacion, nrow = 2)+
  scale_x_date(date_labels = "%Y-%m", date_breaks = "2 year",
               limits = c(ymd("1960-01-01"), ymd("2021-12-31")))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  labs(x = "Tiempo", y = "Precipitación (mm)",
       title = "Serie de tiempo de precipitación anual",
       color = "Fuente")

library(hydroGOF)
# Calcular metricas de ajuste
hydroGOF::gof(sim = pp.data.year$pp_cr2met, 
              obs = pp.data.year$pp_est)

hydroGOF::gof(sim = pp.data.month$pp_cr2met, 
              obs = pp.data.month$pp_est)

# Ajustar un modelo lineal
modelo = lm(pp_est~pp_cr2met, data = pp.data.month)
summary(modelo)
plot(modelo)
coefficients(modelo)

pp.data.month = pp.data.month %>% 
  mutate(pp_corr = -2.15+0.864*pp_cr2met) %>% 
  drop_na()

hydroGOF::gof(sim = pp.data.month$pp_corr, 
              obs = pp.data.month$pp_est)

# correlacion datos mensuales
pp.data.month %>% ggplot(aes(x = pp_est, y = pp_corr))+ #definir ejes
  geom_point()+ #agregar grafico de puntos/dispersion
  geom_abline(color = "red")+ #agregar linea 1:1
  geom_smooth(method = "lm")+ # agregar linea de ajuste lineal
  ggpubr::stat_cor()+ #agregar correlacion (R)
  tune::coord_obs_pred()+ # tamaño del eje x = tamaño del eje y (grafico cuadrado)
  facet_wrap(.~estacion)+ # separar los graficos por estacion
  labs(x = "Precipitación Estación Meteorológica", y = "Precipitación CR2MET",
       title = "Correlación entre precipitación mensual observada y modelada")
# serie de tiempo datos mensuales
pp.data.month %>% ggplot()+
  geom_line(aes(x = fecha, y = pp_est,color = "PP observada"))+
  geom_line(aes(x = fecha, y = pp_corr, color = "PP CR2MET"))+
  facet_wrap(.~ estacion, nrow = 2)+
  scale_x_date(date_labels = "%Y-%m", date_breaks = "4 year",
               limits = c(ymd("2000-01-01"), ymd("2021-12-31")))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  labs(x = "Tiempo", y = "Precipitación (mm)",
       title = "Serie de tiempo de precipitación mensual",
       color = "Fuente")

# calcular precipitacion acumulada mensual
pp.data.year = pp.data.month %>%
  mutate(fecha = floor_date(fecha, unit = "year")) %>% 
  group_by(estacion, fecha) %>% 
  summarise_all(sum)

# correlacion datos anuales
pp.data.year %>% ggplot(aes(x = pp_est, y = pp_corr))+ #definir ejes
  geom_point()+ #agregar grafico de puntos/dispersion
  geom_abline(color = "red")+ #agregar linea 1:1
  geom_smooth(method = "lm")+ # agregar linea de ajuste lineal
  ggpubr::stat_cor()+ #agregar correlacion (R)
  tune::coord_obs_pred()+ # tamaño del eje x = tamaño del eje y (grafico cuadrado)
  facet_wrap(.~estacion)+ # separar los graficos por estacion
  labs(x = "Precipitación Estación Meteorológica", y = "Precipitación CR2MET",
       title = "Correlación entre precipitación anual observada y modelada")

# serie de tiempo datos anuales
pp.data.year %>% ggplot()+
  geom_line(aes(x = fecha, y = pp_est,color = "PP observada"))+
  geom_line(aes(x = fecha, y = pp_corr, color = "PP CR2MET"))+
  facet_wrap(.~ estacion, nrow = 2)+
  scale_x_date(date_labels = "%Y-%m", date_breaks = "2 year",
               limits = c(ymd("1960-01-01"), ymd("2021-12-31")))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  labs(x = "Tiempo", y = "Precipitación (mm)",
       title = "Serie de tiempo de precipitación anual",
       color = "Fuente")

hydroGOF::gof(sim = pp.data.year$pp_corr, 
              obs = pp.data.year$pp_est)

# Balance Hìdrico a nivel de cuenca ---------------------------------------

# Cuenca de estudio -------------------------------------------------------

# leer shape de la cuenca
cuenca = read_sf("./data/vector/Cauquenes.shp") %>%
  # reproyectar a coordenadas geográficas
  st_transform(4326)

# Caudales ----------------------------------------------------------------

# Datos descargados de la DGA
q.month = read_csv("data/table/Caudal_Cauquenes.csv") %>% 
  pivot_longer(cols = 2:13, names_to = "mes",values_to = "caudal") # pivotear columnas
q.month

# vector de fechas mensuales
fechas = seq(ym("1987-01"), ym("2021-12"), by = "months")

# crear columna con fechas
q.month = q.month %>% 
  mutate(fecha = fechas) %>% 
  select(fecha, caudal)
q.month

# calcular caudal medio anual
q.year = q.month %>% 
  mutate(fecha = floor_date(fecha,unit = "year")) %>% 
  group_by(fecha) %>% 
  summarise_all(mean)
q.year

# Precipitacion CR2MET -------------------------------------------------------

# carpeta donde se encuentran los archivos de evapotranspiracion real de MODIS
pp.dir = "data/raster/PPCR2MET2.5"

# obtener la direccion de los archivos en la carpeta
files = list.files(pp.dir, full.names = TRUE, pattern = "nc$");files

# leer rasters
pp = rast(files)
pp

# crear vector de fechas
fechas.pp = seq(
  ymd("1960-01-01"),
  ymd("2021-12-31"),
  by = "days"
)

# asignamos las fechas como nombre de las capas
names(pp) = fechas.pp

# extraer valores dentro de la cuenca
extr = terra::extract(pp, cuenca)
as_tibble(extr)

# calcular precipitacion promedio de la cuenca
pp.day = extr %>%
  select(-ID) %>% 
  drop_na() %>% 
  summarise_all(mean) %>% 
  pivot_longer(cols = 1:ncol(.), names_to = "fecha", values_to = "pp")
pp.day

# PP mensual
pp.month = pp.day %>% 
  mutate(fecha = as_date(fecha),
         fecha = floor_date(fecha, unit = "month")) %>% 
  group_by(fecha) %>% 
  summarise(pp = sum(pp))
pp.month

# PP anual
pp.year = pp.month %>% 
  mutate(fecha = as_date(fecha),
         fecha = floor_date(fecha, unit = "year")) %>% 
  group_by(fecha) %>% 
  summarise(pp = sum(pp))
pp.year



# Evapotranspiración real -------------------------------------------------

# carpeta donde se encuentran los archivos de evapotranspiracion real de MODIS
dir = "data/raster/ET.MODIS"

# obtener la direccion de los archivos en la carpeta
files = list.files(dir, full.names = TRUE, pattern = "ET_500");files

# leer rasters
et = rast(files)
et

# crear vector de fechas
fechas.et = names(et) %>% 
  #recortamos los nombres  desde el caracter 26 al caracter 32.
  str_sub(start = 26, end = 32) %>%
  # le damos formato de fecha, %Y = YYYY, %j=ddd (numero de dia del año o dia juliano)
  as.Date("%Y%j")
fechas.et

# asignamos las fechas como nombre de las capas
names(et) = fechas.et

# separar las imagenes que abarcan mas de un mes dentro de los 8 dias del composite
et = separate_eight_day_composite(et, fechas.et);et

# actualizar vector de fechas
fechas.et = as_date(names(et))

# calcular imagenes mensuales y anuales de etr
et.m = daily_to_monthly(et, dates = fechas.et, fun = "sum");et.m

et.y = to_yearly(et.m, dates = names(et.m), fun = "sum");et.y

# ETr mensual
extr = terra::extract(et.m, cuenca)

et.month = extr %>%
  # eliminar primera columna de ID
  select(-ID) %>% 
  # borrar valores NA
  drop_na() %>%  
  # calcular la mediana de cada columna 
  # (para evitar el efecto sobre la media, producto de los valores altos de etr en la ciudad)
  summarise_all(median) %>% 
  pivot_longer(cols = 1:ncol(.), names_to = "fecha", values_to = "et") %>%  # pivotear tabla
  mutate(fecha = as_date(fecha))
et.month

# Etr anual
extr = terra::extract(et.y, cuenca)
et.year = extr %>%
  select(-ID) %>% 
  drop_na() %>% 
  summarise_all(median) %>%
  pivot_longer(cols = 1:ncol(.), names_to = "fecha", values_to = "et") %>% 
  mutate(fecha = as_date(fecha))
et.year

# Balance Hídrico.
# Datos de caudales estan en m3/s mientras que PP y ETr estan en mm
# Se deben transformar datos de m3/s a mm
# La fórmula para convertir la media mensual de caudal en metros cúbicos por segundo a 
# caudal acumulado mensual en milímetros es:
#   
#   caudal mensual en milímetros = 
#       (caudal medio mensual en metros cúbicos por segundo x 
#       60 segundos por minuto x 
#       60 minutos por hora x 
#       24 horas por día x
#       número de días en el mes) / 
#       (área de la cuenca en metros cuadrados x 0.001) 
# 
# Donde:
#   
#   La precipitación acumulada mensual se expresa en milímetros.
#   El caudal medio mensual se expresa en metros cúbicos por segundo.
#   El área de la cuenca se expresa en metros cuadrados.
#   El factor de conversión de 0.001 se usa para convertir de metros a milímetros.

# calcular el area de la cuenca
sf_use_s2(FALSE) # necesario cuando la cuenca esta en coordenadas geográficas
st_area(cuenca) # calcular area
area_cuenca = st_area(cuenca) %>% as.numeric() # sacar solo el valor sin la unidad
area_cuenca

# m3/s -> mm
q.month = q.month %>% mutate(
  days = days_in_month(fecha),
  caudal = caudal*(3600*24*days)/(0.001*area_cuenca)
)

data.month = full_join(pp.month, et.month, by = "fecha") %>% 
  full_join(q.month, by = "fecha") %>% 
  select(-days)
data.month

ggplot(data.month)+
  geom_line(aes(x = fecha, y = pp, color = "Precipitación"))+
  geom_line(aes(x = fecha, y = et, color = "ETr"))+
  geom_line(aes(x = fecha, y = caudal,color = "Caudal"))+
  scale_x_date(limits = c(ymd("2000-01-01"), ymd("2021-12-31")))+
  labs(x = "tiempo", y = "(mm)", title = "Serie de tiempo mensual de componentes del BH",
       color = "")

# Calcular media mensual por variable para el mismo periodo de tiempo (2000 en adelante)
data.mean = data.month %>% 
  drop_na() %>% 
  mutate(mes = month(fecha)) %>% 
  group_by(mes) %>% 
  summarise_all(mean) %>% 
  mutate(disp = pp-et-caudal)

ggplot(data.mean)+
  geom_line(aes(x = mes, y = pp, color = "Precipitación"), linewidth = 0.8)+
  geom_line(aes(x = mes, y = et, color = "ETr"), linewidth = 0.8)+
  geom_line(aes(x = mes, y = caudal,color = "Caudal"), linewidth = 0.8)+
  geom_line(aes(x = mes, y = disp, color = "Disponibilidad"), linewidth = 0.8)+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.8)+
  # scale_x_date(limits = c(ymd("2000-01-01"), ymd("2021-12-31")))+
  labs(x = "mes", y = "(mm)", title = "Balance hídrico medio mensual",
       color = "")

# Balance a hidrico anual
  
# calcular acumulado anual
data.year = data.month %>% 
  mutate(fecha = floor_date(fecha, unit = "year")) %>% 
  group_by(fecha) %>% 
  summarise_all(sum) %>% 
  mutate(disp = pp-et-caudal)

# serie de tiempo de datos anuales
ggplot(data.year)+
  geom_line(aes(x = fecha, y = pp, color = "Precipitación"), linewidth = 0.8)+
  geom_line(aes(x = fecha, y = et, color = "ETr"), linewidth = 0.8)+
  geom_line(aes(x = fecha, y = caudal,color = "Caudal"), linewidth = 0.8)+
  geom_point(aes(x = fecha, y = caudal,color = "Caudal"), linewidth = 0.8)+
  geom_line(aes(x = fecha, y = disp, color = "Disponibilidad"), linewidth = 0.8)+
  scale_x_date(limits = c(ymd("2000-01-01"), ymd("2021-12-31")),
               date_labels = "%Y", date_breaks = "2 year")+
  labs(x = "tiempo", y = "(mm)", title = "Serie de tiempo mensual de componentes del BH",
       subtitle = "Los año sin medicion de caudal, faltan datos en algunos meses",
       color = "")

# hay meses de caudales que no tienen datos, por eso hay años que no tienen datos de caudales
# podemos eliminar esos meses con drop_na
# calcular acumulado anual sin contar los meses de datos faltantes
data.year = data.month %>% 
  mutate(fecha = floor_date(fecha, unit = "year")) %>% 
  drop_na() %>% 
  group_by(fecha) %>% 
  summarise_all(sum) %>% 
  mutate(disp = pp-et-caudal)

# serie de tiempo de datos anuales
ggplot(data.year)+
  geom_line(aes(x = fecha, y = pp, color = "Precipitación"), linewidth = 0.8)+
  geom_line(aes(x = fecha, y = et, color = "ETr"), linewidth = 0.8)+
  geom_line(aes(x = fecha, y = caudal,color = "Caudal"), linewidth = 0.8)+
  geom_point(aes(x = fecha, y = caudal,color = "Caudal"), linewidth = 0.8)+
  geom_line(aes(x = fecha, y = disp, color = "Disponibilidad"), linewidth = 0.8)+
  scale_x_date(limits = c(ymd("2000-01-01"), ymd("2021-12-31")),
               date_labels = "%Y", date_breaks = "2 year")+
  labs(x = "tiempo", y = "(mm)", title = "Serie de tiempo mensual de componentes del BH",
       subtitle = "Se eliminan los meses de datos de caudal faltantes antes de sumar el acumulado anual",
       color = "")


# Podemos tambien ignorar los años donde faltan datos mensuales de caudales
# calcular acumulado anual sin contar los meses de datos faltantes
data.year = data.month %>% 
  mutate(fecha = floor_date(fecha, unit = "year")) %>% 
  group_by(fecha) %>% 
  summarise_all(sum) %>% 
  mutate(disp = pp-et-caudal) %>% 
  drop_na()

# serie de tiempo de datos anuales
ggplot(data.year)+
  geom_line(aes(x = fecha, y = pp, color = "Precipitación"), linewidth = 0.8)+
  geom_line(aes(x = fecha, y = et, color = "ETr"), linewidth = 0.8)+
  geom_line(aes(x = fecha, y = caudal,color = "Caudal"), linewidth = 0.8)+
  geom_point(aes(x = fecha, y = caudal,color = "Caudal"), linewidth = 0.8)+
  geom_line(aes(x = fecha, y = disp, color = "Disponibilidad"), linewidth = 0.8)+
  scale_x_date(limits = c(ymd("2000-01-01"), ymd("2021-12-31")),
               date_labels = "%Y", date_breaks = "2 year")+
  labs(x = "tiempo", y = "(mm)", title = "Serie de tiempo mensual de componentes del BH",
       subtitle = "Se ignoran los años donde faltan datos mensuales de caudal",
       color = "")

# Tambien podemos ignorar los meses donde no hay medicion de caudales solo para esa variable
data.year = data.month %>% 
  mutate(fecha = floor_date(fecha, unit = "year")) %>% 
  group_by(fecha) %>% 
  summarise_all(sum, na.rm = TRUE) %>% # aqui sumamos independiente de si faltan algunos meses en el año 
  mutate(disp = pp-et-caudal)

# serie de tiempo de datos anuales
ggplot(data.year)+
  geom_line(aes(x = fecha, y = pp, color = "Precipitación"), linewidth = 0.8)+
  geom_line(aes(x = fecha, y = et, color = "ETr"), linewidth = 0.8)+
  geom_line(aes(x = fecha, y = caudal,color = "Caudal"), linewidth = 0.8)+
  geom_point(aes(x = fecha, y = caudal,color = "Caudal"), linewidth = 0.8)+
  geom_line(aes(x = fecha, y = disp, color = "Disponibilidad"), linewidth = 0.8)+
  scale_x_date(limits = c(ymd("2000-01-01"), ymd("2021-12-31")),
               date_labels = "%Y", date_breaks = "2 year")+
  labs(x = "tiempo", y = "(mm)", title = "Serie de tiempo mensual de componentes del BH",
       subtitle = "La suma anual se hace con todos los meses disponibles para cada variable",
       color = "")
