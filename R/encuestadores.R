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

#Encuestas por persona
bd %>%  count(nombre_encuestador) %>%  arrange(desc(n))   %>%  mutate(mean(n))


# pct de encuestas a favor de Carlos
bd %>% group_by(nombre_encuestador) %>%  count(municipal) %>%
  mutate(n = round(n *100/sum(n))) %>%  filter(municipal== "Carlos Morales, por MORENA") %>%
  arrange(desc(n))
