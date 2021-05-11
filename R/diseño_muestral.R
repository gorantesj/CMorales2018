# Diseño Muestral
library(tidyverse)
library(magrittr)
library(electoral)
library(sf)


##Leer bds

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
votgeodm %>%  write_excel_csv("~/Documents/Git/CMorales2018/data/estratos_final.csv")


secc %>%  filter(SECCION %in% manzanas_en_muestra$SECCION) %>%
  st_write("Mayo 2021/secciones_muestra.shp")
