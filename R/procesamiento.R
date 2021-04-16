# Librerías
library(tidyverse)
library(survey)
library(readr)
library(lubridate)

# leer bases 
equipo1 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Clientes/Chiapas/Tuxtla-2021/Bds/TUXTLA GUTIÉRREZ, EQUIPO 1.csv")
equipo2 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Clientes/Chiapas/Tuxtla-2021/Bds/TUXTLA GUTIÉRREZ, EQUIPO 2.csv")
equipo3 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Clientes/Chiapas/Tuxtla-2021/Bds/TUXTLA GUTIÉRREZ, EQUIPO 3.csv")
equipo4 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Clientes/Chiapas/Tuxtla-2021/Bds/TUXTLA GUTIÉRREZ, EQUIPO 4.csv")

codigos <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Clientes/Chiapas/Tuxtla-2021/Bds/codigos.csv")

bd <-  bind_rows(equipo1, equipo2, equipo3, equipo4)

bd <-bd %>% set_names(codigos$id)


# limpiar bd --------------------------------------------------------------

bd<- bd %>%  mutate(fecha = gsub(pattern  ="GMT-5", fixed = T,
                            replacement = "", x =fecha ),
               fecha = gsub(pattern  ="m.", fixed = T,
                            replacement = "", x =fecha ),
               fecha = gsub(pattern  =" a.", fixed = T,
                            replacement = "", x =fecha ),
               fecha = str_squish(fecha))

# Reporte de levantamiento ------------------------------------------------

# encuestas
bd <-bd %>%  mutate(fecha= ymd_hms(fecha)) %>%  
  mutate(dia = floor_date(fecha, unit = "day")) 

bd %>%  count(dia) 
bd %<>%   filter(dia != as.Date( "2021-04-14 00:00:00"))
bd %>%   filter(dia == as.Date( "2021-04-16 00:00:00")) %>%  count(nombre_encuestador)

bd   %<>% 
  mutate (municipal  = sub('*.[[:punct:]]', '\\1', municipal) ,
          municipal = str_squish(municipal))

bd %>%  count(seccion, manzana) %>%  arrange(desc(n)) %>%  count(n)

bd   %>%   count(municipal)  %>%  
  mutate(n = round(n *100/sum(n))) %>% 
  arrange(desc(n))

bd  %>% group_by(equipo, nombre_encuestador) %>% 
  count(municipal)  %>%  
  mutate(n = round(n *100/sum(n))) %>% 
  arrange(desc(n)) 
  




bd  %>% group_by(equipo) %>% 
  count(municipal)  %>%  
  mutate(n = round(n *100/sum(n))) %>% 
  filter(municipal %in% c("Carlos Morales, por MORENA","Willy Ochoa, por PRI, PAN y PRD",
                          "Paco Rojas, por Movimiento Ciudadano")) 
