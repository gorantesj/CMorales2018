# Librerías
library(tidyverse)
library(survey)
library(readr)
library(lubridate)
library(magrittr)
library(readxl)
library(sf)

# leer bases
equipo1 <- read_csv("data/TUXTLA GUTIÉRREZ, EQUIPO 1.csv")
equipo2 <- read_csv("data/TUXTLA GUTIÉRREZ, EQUIPO 2.csv")
equipo3 <- read_csv("data/TUXTLA GUTIÉRREZ, EQUIPO 3.csv")
equipo4 <- read_csv("data/TUXTLA GUTIÉRREZ, EQUIPO 4.csv")

codigos <- read_csv("data/codigos.csv")

bd <-  bind_rows(equipo1, equipo2, equipo3, equipo4)

bd <-bd %>% set_names(codigos$id)

rm(equipo1, equipo2, equipo3, equipo4)

# INE
edad_ine <- read_csv("data/INE/grupos_etarios.csv")

# Shapefile
seccion <- read_sf("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/SHP/2017/07 - Chiapas/SECCION.shp")%>%
  st_transform(4326)
manzana <- read_sf("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/SHP/2017/07 - Chiapas/MANZANA.shp")%>%
  st_transform(4326)

# Limpar lista nominal ----------------------------------------------------
#Filtrar estado y muni
edad_ine %<>%  filter(NOMBRE_ENTIDAD == "CHIAPAS", NOMBRE_MUNICIPIO == "TUXTLA GUTIERREZ")

# hacer columnas
edad <-edad_ine %>%
  gather(grupo_nominal, nominal, c(LISTA_18_HOMBRES:LISTA_65_Y_MAS_MUJERES)) %>%
  # gather(grupo_padron, padron, c(PADRON_18_HOMBRES:PADRON_65_Y_MAS_MUJERES)) %>%
  select(SECCION, grupo_nominal, nominal)
# Cambiar nombres por como están en la bd
edad %<>% mutate(sexo=if_else(str_detect(grupo_nominal, pattern = "HOMBRES"),
                             "Hombre", "Mujer"),
  grupo_nominal = gsub(pattern = "_HOMBRES", replacement = "", grupo_nominal),
                  grupo_nominal = gsub(pattern = "_MUJERES", replacement = "", grupo_nominal))

lista_nominal <- edad %>%  mutate(
  edad_nominal = case_when(
    grupo_nominal %in% c( "LISTA_18", "LISTA_19", "LISTA_20_24") ~   "18 a 24 años",
    grupo_nominal %in% c( "LISTA_25_29", "LISTA_30_34") ~    "25 a 34 años",
    grupo_nominal %in% c( "LISTA_35_39", "LISTA_40_44") ~   "35 a 44 años",
    grupo_nominal %in% c( "LISTA_45_49", "LISTA_50_54") ~  "45 a 54 años",
    grupo_nominal %in% c( "LISTA_55_59", "LISTA_60_64") ~   "55 a 64 años",
    grupo_nominal == "LISTA_65_Y_MAS" ~   "65 años o más" ) ) %>%
  group_by(seccion =SECCION, sexo,edad= edad_nominal) %>%  summarise(n =sum(nominal)) %>%  ungroup()

rm(edad, edad_ine)
# limpiar bd --------------------------------------------------------------
#Fecha y hora
bd %<>%  mutate(auxiliar = grepl(pattern = "p", x = fecha),
  fecha = gsub(pattern  ="GMT-5", fixed = T, replacement = "", x =fecha  ) %>%
    ymd_hms() + hours(12)*auxiliar)

bd <-bd %>%  mutate(fecha= ymd_hms(fecha)) %>%
  mutate(dia = floor_date(fecha, unit = "day"))
bd %>%  count(dia)

#Se quita la de prueba de antes del levantamiento
bd %<>%   filter(dia != as.Date( "2021-04-14 00:00:00"))

# quitar otro mensaje
bd %<>%  mutate(credencial = gsub(pattern = " (Agradecer y solicitar la participación de otra persona)",
                                  replacement = "",
                                  x = credencial,
                                  fixed = T))


#Quitar incisos y no leer
bd  %<>%
  map_df(~{if(is.character(.x)){
    sub(pattern = '*.[[:punct:]]',replacement =  '\\1',  x = .x) %>%
  gsub(pattern = " (No leer)",replacement =  "", fixed = T, x = .) %>%
  str_squish( .) %>%
  gsub(pattern = "9 No sabe / No contesta",
       replacement =  "No sabe / No contesta", fixed = T, x = .) }
    else(.x)})

#Quitar encuestas que se hayan registrado con menos de un min de diferencia
bd %<>%  group_by(nombre_encuestador) %>%
  arrange(fecha) %>%
  mutate(lag =lag(fecha), lag2 = fecha-lag) %>%
  # select(nombre_encuestador, lag, fecha, lag2) %>%
  mutate(duracion = as.numeric(lag2)) %>%
  filter(duracion > 60)

# Revisar bd de encuestadores para determinar si se elimina a alguno
bd %>%
  filter(!nombre_encuestador  %in%c("Isaac", "Pablo")) %>%
  count(municipal) %>%  mutate(n = round(n*100/sum(n))) %>%
  arrange(desc(n))

#Agrupar seccion y manzana y pegarsela al shp
enc <-bd %>%group_by(seccion, manzana) %>%
  summarise(encuestas  = n())
manzana %>%
  inner_join(enc, by = c("MANZANA" = "manzana", "SECCION" = "seccion")) %>%
  select(SECCION, MANZANA, encuestas)
