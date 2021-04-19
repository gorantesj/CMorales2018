# Librerías
library(tidyverse)
library(readr)



# leer bases
equipo1 <- read_csv("data/TUXTLA GUTIÉRREZ, EQUIPO 1.csv")
equipo2 <- read_csv("data/TUXTLA GUTIÉRREZ, EQUIPO 2.csv")
equipo3 <- read_csv("data/TUXTLA GUTIÉRREZ, EQUIPO 3.csv")
equipo4 <- read_csv("data/TUXTLA GUTIÉRREZ, EQUIPO 4.csv")

codigos <- read_csv("data/codigos.csv")

bd <-  bind_rows(equipo1, equipo2, equipo3, equipo4)

bd <-bd %>% set_names(codigos$id)

rm(equipo1, equipo2, equipo3, equipo4)
# limpiar bd --------------------------------------------------------------
#Fecha y día
bd<- bd %>%  mutate(fecha = gsub(pattern  ="GMT-5", fixed = T,
                                 replacement = "", x =fecha ),
                    fecha = gsub(pattern  ="m.", fixed = T,
                                 replacement = "", x =fecha ),
                    fecha = gsub(pattern  =" a.", fixed = T,
                                 replacement = "", x =fecha ),
                    fecha = str_squish(fecha))

bd <-bd %>%  mutate(fecha= ymd_hms(fecha)) %>%
  mutate(dia = floor_date(fecha, unit = "day"))

bd %>%  count(dia)

bd %<>%   filter(dia != as.Date( "2021-04-14 00:00:00"))

#
#
# quitar otro mensaje
bd %<>%  mutate(credencial = gsub(pattern = " (Agradecer y solicitar la participación de otra persona)",
                                  replacement = "",
                                  x = credencial,
                                  fixed = T))


# bd  %>%
#   map(~{sub(pattern = '*.[[:punct:]]',replacement =  '\\1',  x = .x) %>%
#       gsub(pattern = " (No leer)",replacement =  "", fixed = T, x = .) %>%
#       str_squish( .) %>%
#       gsub(pattern = "9 No sabe / No contesta",
#            replacement =  "No sabe / No contesta", fixed = T, x = .)}) %>%
#   as_tibble()


#Encuestas por persona
bd %>%  count(nombre_encuestador) %>%  arrange(desc(n))   %>%  mutate(mean(n))


# pct de encuestas a favor de Carlos
bd %>% group_by(equipo, nombre_encuestador) %>%  count(municipal) %>%
  mutate(n = round(n *100/sum(n))) %>%  filter(municipal== "Carlos Morales, por MORENA") %>%
  arrange(desc(n))
  # ungroup() %>%
  # mutate(mean(n))

bd %>%  group_by(nombre_encuestador) %>%
  arrange(fecha) %>%
  mutate(lag =lag(fecha), lag2 = fecha-lag) %>%
  # select(nombre_encuestador, lag, fecha, lag2) %>%
  mutate(duracion = as.numeric(lag2)) %>%
  filter(duracion > 60) %>%  count( equipo, nombre_encuestador, seccion, manzana, personas) %>%
  arrange(desc(n)) %>%
  view()

bd %>% filter()
arrange(fecha) %>%  select(fecha, equipo, nombre_encuestador, seccion, manzana) %>%
  view()

#Agrupar seccion y manzana y pegarsela al shp
