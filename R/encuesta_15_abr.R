# Cargar librerías
library(tidyverse)
library(sf)
library(leaflet)

# Cargar info
manzanas <- read_rds(file = "data/manzanas_estrato.rds") %>%
  filter(STATUS!=2)
secciones <- read_csv("data/estratos.csv")

pal <- colorFactor(
  palette = c('red', 'blue', 'cyan', 'purple', 'black'),
  domain = manzanas$estrato_nombre
)


leaflet(manzanas) %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(estrato_nombre),
              stroke = F,
              label = ~as.character(SECCION))

secciones %>%
  ggplot(aes(x=x, y=y, color=estrato_nombre, shape=SECCION==1747)) +
  geom_point()


# Diseño
# Encuestas
N <- 1200

#Manzanas por Sección
MS <- 7
# Manzanas
M <- N/MS
# Encuestas por manzana
EM <- 8


# Resumen Estratos
resumen <- secciones %>%
  group_by(estrato_nombre) %>%
  summarise(ln=sum(ln),
            secciones=n()) %>%
  mutate(ln=ln/sum(ln),
         encuestas=round(N*ln),
         manzanas_muestra=round(encuestas/EM),
         secciones_muestra=round(manzanas_muestra/MS))

# Secciones estratificadas
estratos_secc <- secciones %>%
  split(.$estrato_nombre)
set.seed(2022)
# Muestra de estratos
estratos_en_muestra <- resumen$estrato_nombre %>%
  map_df(~{
    estratos_secc[[.x]] %>%
      sample_n(size = min(c(resumen %>%
               filter(estrato_nombre==.x) %>%
               pull(secciones_muestra),
               nrow(.)
               )), weight = ln)
  })

# Muestra de manzanas
set.seed(2022)
manzanas_en_muestra <- manzanas %>%
  filter(SECCION %in% estratos_en_muestra$SECCION) %>%
  group_by(SECCION) %>%
  sample_n(size=MS)

manzanas_en_muestra %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(estrato_nombre),
              color = ~pal(estrato_nombre),
              label = ~as.character(SECCION))

nrow(manzanas_en_muestra)*EM
set.seed(2022)
ruta <- manzanas_en_muestra %>%
  ungroup() %>%
  st_centroid() %>%
  mutate(x=st_coordinates(geometry)[,1],
         y=st_coordinates(geometry)[,2],
         ) %>%
  as_tibble() %>%
  select(x,y) %>%
  scale() %>%
  dist() %>%
  hclust(method = "centroid") %>%
  cutree(k = 12)



manzanas_en_muestra <- manzanas_en_muestra %>%
  ungroup() %>%
  mutate(ruta=ruta,
         equipo=case_when((ruta %in% c(2))~1,
                          (MANZANA==51 & SECCION ==1602)~1,
                          (ruta %in% c(1,3, 12,7))~2,
                          (ruta %in% c(6,4,10, 11))~3,
                          (ruta %in% c(5,8,9))~4
                          ))

manzanas_en_muestra %>%
  group_by(ruta) %>%
  summarise(n=n())

manzanas_en_muestra %>%
  group_by(equipo) %>%
  summarise(n=n())

pal <- colorFactor(
  palette = c("red", "blue", "green","orange"),
  domain = manzanas_en_muestra$equipo
)
manzanas_en_muestra%>%
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(equipo),
              color = ~pal(equipo),
              label = ~glue::glue("{ruta} Sección {SECCION}
                                  Manzana {MANZANA}"))

# Equipo 1
pal <- colorFactor(
  palette = RColorBrewer::brewer.pal(n = 5, name = "Dark2"),
  domain = manzanas_en_muestra %>%
    filter(equipo==4) %>% pull(SECCION) %>% unique()
)
manzanas_en_muestra%>%
  filter(equipo==4) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB) %>%
  addPolygons(fillColor = ~pal(SECCION),
              color = ~pal(SECCION),
              label = ~glue::glue("Sección {SECCION}
                                  Manzana {MANZANA}")) %>%
  leaflet.extras::addControlGPS(options = leaflet.extras::gpsOptions(position = "topleft", activate = TRUE,
                                                     autoCenter = TRUE, maxZoom = 10,
                                                     setView = TRUE))

manzanas_en_muestra%>%
  filter(equipo==4) %>% pull(SECCION) %>% unique() %>%
  as.numeric() %>% sort()

manzanas_en_muestra%>%
  filter(equipo==4) %>% pull(MANZANA) %>% unique() %>%
  as.numeric() %>% sort()

# Sección sustituta
estrato_sus <- secciones %>% filter(SECCION=="1938") %>% pull(estrato_nombre)
set.seed(2022)
estrato_nuevo <- secciones %>%
  filter(estrato_nombre=="Periferia norte",
         !(SECCION %in% manzanas_en_muestra$SECCION)) %>%
  sample_n(size = 1)
manzanas %>%
  filter(SECCION==estrato_nuevo$SECCION) %>%
  sample_n(size = MS) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB) %>%
  addPolygons(fillColor = ~pal(SECCION),
              color = ~pal(SECCION),
              label = ~glue::glue("Sección {SECCION}
                                  Manzana {MANZANA}"))

#Lista nominal nueva
edad_ine <- read_csv("data/INE/grupos_etarios.csv")

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

#cuotas
cuotas <- lista_nominal %>%  filter(seccion %in% manzanas_en_muestra$SECCION)%>%
  group_by(seccion) %>% mutate(probabilidad=n/sum(n)) %>%
  mutate(entrevistas= round(probabilidad*57)) %>%
  # ungroup() %>%  summarise(sum(entrevistas)) %>%
  select(SECCION= seccion, sexo, edad, entrevistas)

cuotas %>%   write_excel_csv("Mayo 2021/cuotas.csv")

manzanas_en_muestra %>%  write_excel_csv("Mayo 2021/muestra.csv")
saveRDS(manzanas_en_muestra, "Mayo 2021/muestra.rds")

manzanas_en_muestra %>%  left_join(cuotas, by ="SECCION")





