# Diseño Muestral
library(tidyverse)
library(magrittr)
library(electoral)
library(sf)


##Leer bds
setwd("~/Documents/Git/CMorales2018/")
# electorales
basetux <- read_csv("~/Documents/Git/analisis_nuevas/Tuxtla/inp/2018_SEE_AYUN_CHIS_CAS.csv")
basetux <- basetux %>% rename("PANAL" = "X19")

#Base con los estratos
dm <- read_csv("data/estratos.csv")

# Shapefile secciones
secc <- st_read("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/SHP/2017/07 - Chiapas/SECCION.shp", stringsAsFactors = FALSE, quiet = TRUE) %>%
  st_transform(4326)

#manzanas
manzanas <- st_read("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/SHP/2017/07 - Chiapas/MANZANA.shp"     ,
                    stringsAsFactors = FALSE, quiet = TRUE) %>%
  st_transform(4326)

#colonias
colonia <- st_read("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/SHP/2017/07 - Chiapas/COLONIA.shp",
                   stringsAsFactors = FALSE, quiet = T)


# Revisar mapa ------------------------------------------------------------

# Ver división
secc %>%  inner_join(dm, by = "SECCION") %>%
  ggplot()+geom_sf(aes(fill = estrato, geometry = geometry))

#Porcentaje de la lista nominal
basetux %>%  filter(MUNICIPIO == "TUXTLA GUTIERREZ") %>%
  group_by(SECCION) %>% summarise(ln = sum(LISTA_NOMINAL, na.rm = T)) %>%
  left_join(dm) %>%
  group_by(estrato) %>%
  summarise(pctln = sum(ln*100)) %>%
  arrange(desc(ln))




# #Transformar base con electoral -----------------------------------------

tuxtla <- basetux %>%
  rename( "nominal"="LISTA_NOMINAL", "total"="TOTAL_VOTOS", "seccion"="SECCION")

info <- list()
partidos <- c("mvc")
coaliciones <- list(c("pan","prd","mc"),
                    c("pt","morena", "es"),
                    c("pri", "pvem", "na", "pcu"))

color <- c("mvc"="#703582",
           "coalición_pan prd mc"="#2260BF",
           "coalición_pt morena es"="#BF3722",
           "coalición_pri pvem na pcu"="#23A95D")
info <- preparar_info_de_eleccion(bd = tuxtla,
                                  partidos = partidos,
                                  coaliciones = coaliciones,
                                  unidad_analisis = "municipio",
                                  id_unidad_analisis="TUXTLA GUTIERREZ",
                                  año_analisis = 2018,
                                  nombre_unidad_analisis = "Presidencia municipal - Tuxtla Gutiérrez",
                                  colores = color)

#Shapefile con votaciones
votgeo<-  info$bd %>%  filter(municipio == "TUXTLA GUTIERREZ") %>%
  group_by(seccion) %>%  summarise(pan = sum(`votos_coalición_pan prd mc`),
                                   morena = sum(`votos_coalición_pt morena es`),
                                   pri  = sum(`votos_coalición_pri pvem na pcu`),
                                   ln = sum(nominal),
                                   total = sum(total))

votgeo <-secc %>%  inner_join(votgeo, by = c(  "SECCION" = "seccion"))
votgeo%>%
  st_centroid()

votgeo <- votgeo %>%  left_join(dm)

# Calcular pesos por estrato
#numero persona
resumen <- votgeo %>%
  group_by(estrato) %>%
  summarise(
    morena=sum(morena)/sum(ln),
    var_morena=var(morena/ln),
    ln = sum(ln)) %>%
  mutate(ln=ln/sum(ln))

# x y y del centroide y pegarla  al shp y relativizar morena

votgeo_tb<-votgeo%>%
  st_centroid() %>% mutate(x=st_coordinates(geometry)[,1],
                           y=st_coordinates(geometry)[,2]) %>%  as_tibble() %>%
  mutate(across(c("morena","pan","pri"), ~.x/ln), dif = morena-pan) %>%
  select(SECCION, x, y, pan:pri, estrato, dif, ln)

votgeo_tb %>%  ggplot()+ geom_point(aes(x =x, y = y, color = pri))+
  scale_color_gradient2(low = "red", high = "blue", midpoint = .25)


#Variables geográficas
set.seed(123)
hc_geo <- hclust(dist(votgeo_tb %>%  select(x,y) %>% scale())) %>%  cutree(k =8)

#Variables electorales
hc_completo <- hclust(dist(votgeo_tb %>%  select(x,y,morena, pan) %>%  scale()) ^2 ) %>%
  cutree(k =5)

hc_dif<- hclust(dist(votgeo_tb %>%  select(x,y,dif) %>%  scale(), method = "euclidean") ) %>%
  cutree(k =5)
# Estrato geográfico y lectoral
votgeo_estratos  <- votgeo_tb %>%
  mutate(estratogeo = hc_geo, estratoele = hc_completo, estratodif = hc_dif)

#Viz
votgeo_estratos %>%  ggplot()+
  geom_point(size = 3,aes(x = x, y = y, color = estratodif %>% as_factor())) +
  geom_hline(yintercept = 16.74)

votgeo_estratos %>%  ggplot()+
  geom_point(aes(x = x, y = y, color = estratogeo %>% as_factor()))

votgeo_estratos %>%  ggplot()+
  geom_point(size = 3,aes(x = x, y = y, color = estratoele %>% as_factor()))


# Transformar datos
votgeodm <- votgeo_estratos %>%
  mutate(estrato_final = case_when(estratodif == 2~"4",
                                   (estratodif == 3 & y> 16.773 & x>-93.10) ~"5",
                                   (estratodif == 3& y<16.74& x< -93.10)~"1",
                                   (estratodif == 3 & x< -93.1161) ~"6",
                                   (estratodif == 1& y>16.765)~"5",
                                   (estratodif == 3 & y > 16.769 & x> -93.088)~ "5",
                                   T~estratodif %>%  as.character()))

resumen_dif <- votgeodm %>%
  group_by(estrato_final) %>%
  summarise(
    ln= sum(ln),
    diferencia=sum(dif),
    var_dif=var(dif)) %>%
  mutate(ln=ln/sum(ln))


votgeodm %>%  ggplot()+ geom_point(size = 3,aes(x =x, y = y, color = estrato_final)) +
  geom_hline(yintercept = 16.769)+
  geom_vline(xintercept = -93.088)

votgeodm<- votgeodm %>%
  mutate(estrato_nombre = case_when(estrato_final == "1" ~ "Periferia sur oriente",
                                    estrato_final == "3" ~ "Centro oriente",
                                    estrato_final == "5" ~ "Periferia norte",
                                    estrato_final == "4" ~ "Periferia sur poniente",
                                    estrato_final == "6" ~ "Centro poniente"
  )  )

resumen_final <- votgeodm %>%
  group_by(estrato_nombre) %>%
  summarise(
    n= n(),
    ln= sum(ln),
    diferencia=sum(dif),
    var_dif=var(dif)) %>%
  mutate(ln=ln/sum(ln)) %>%  arrange(diferencia)

# Revisar estratos de manzanas
manzanas_estrato <-manzanas %>%  inner_join(votgeodm, by = "SECCION")
# manzanas_estrato <-secc %>%  inner_join(votgeodm, by = "SECCION")

manzanas_estrato %>%  ggplot()+
  geom_sf(aes(fill = estrato_nombre, geometry = geometry))+
  scale_fill_manual(values = c("#F7EFE9", "#F7EFE9", "#F7EFE9", "#F7EFE9", "#750017", rep("#F7EFE9", 4)))+
  theme_minimal()+
  theme(panel.grid = element_blank())

# saveRDS(manzanas_estrato, "~/Desktop/manzanas_estrato.rds")
# votgeodm %>%  write_excel_csv("~/Documents/Git/CMorales2018/data/estratos_final.csv")

# Cargar librerías
library(tidyverse)
library(sf)
library(leaflet)

# Cargar info
# manzanas <- read_rds(file = "data/manzanas_estrato.rds") %>%
#   filter(STATUS!=2)
manzanas <- manzanas_estrato
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
MS <- 8
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
  mutate(entrevistas= round(probabilidad*63.4)) %>%
   # ungroup() %>%  summarise(sum(entrevistas)) %>%
  select(SECCION= seccion, sexo, edad, entrevistas)

cuotas %>%   write_excel_csv("Mayo 2021/cuotas.csv")

manzanas_en_muestra %>% write_sf("Mayo 2021/muestra.shp")
# saveRDS(manzanas_en_muestra, "Mayo 2021/muestra.rds")

secc %>%  filter(SECCION %in% manzanas_en_muestra$SECCION) %>%
  write_sf("Mayo 2021/secciones_muestra.shp")

manzanas_en_muestra %>%  left_join(cuotas, by ="SECCION")






