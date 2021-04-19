# Librerías
library(tidyverse)
library(readr)



# leer script de procesamiento

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


