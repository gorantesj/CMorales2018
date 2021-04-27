library(tidyverse)
library(survey)
bd <- read_csv("data/encuestas.csv")
bd <- bd %>% mutate(id=row_number())
manzanas <-  read_rds("data/manzanas_estrato.rds")
estratos <- read_csv("data/estratos_final.csv")
# Secciones que no pertencen al marco muestral
bd <- bd %>% inner_join(manzanas %>%
                    select(SECCION, MANZANA, estrato_nombre),
                  by = c("seccion"="SECCION","manzana"="MANZANA"))

# Se corrige municipal
bd <- bd %>% mutate(municipal2=if_else(municipal %in% c("Carlos Morales, por MORENA", "Paco Rojas, por Movimiento Ciudadano", "Willy Ochoa, por PRI, PAN y PRD", "No sabe / No contesta", "Ninguno", "No votará"), municipal,"Otro"
),
municipal2=if_else(municipal2=="Ninguno", true = "No votará", municipal2),
personas=substr(personas, 1,1) %>% as.numeric(),
partido2=if_else(partido %in% c("PRI","PAN","PRD","MORENA","Movimiento Ciudadano", "Partido Verde", "No sabe / No contesta", "Ninguno"), partido, "Otro")
)

# Filtar Isaac, no se ve bien
bd <- bd %>% filter(nombre_encuestador!="Isaac")

bd <- bd %>% filter(hour(fecha)<23 & hour(fecha)>7)

# Diferentes diseños
diseño_simple <- survey::svydesign(ids = ~1, data = bd)
diseño_estratificado <- survey::svydesign(ids = ~1,
                                          data = bd,
                                          strata = ~estrato_nombre)

# Aproximacion de probabilidad a la seccion.
bd <- manzanas %>% as_tibble() %>%
  group_by(estrato_nombre, SECCION) %>% summarise(ln=sum(ln)) %>%
  mutate(probabilidad_seccion=ln/sum(ln)) %>%
  select(seccion=SECCION, probabilidad_seccion) %>%
  inner_join(bd)
# Aproximación de probabilidad de la manzana
auxiliar <- manzanas %>%
  filter(SECCION %in% unique(bd$seccion)) %>%
  group_by(seccion=SECCION) %>%
  summarise(n=n())
auxiliar2 <- bd %>% group_by(seccion, manzana) %>%
  summarise(n()) %>%
  summarise(n_manzanas = n())
bd <- bd %>% inner_join(auxiliar %>% inner_join(auxiliar2, by="seccion") %>%
                    as_tibble() %>%
  mutate(probabilidad_manzanas = n_manzanas/n) %>%
  select(seccion,probabilidad_manzanas))

diseño_complejo <- survey::svydesign(ids = ~seccion+manzana+personas,
                                          data = bd,
                                     probs = ~probabilidad_seccion+probabilidad_manzanas+1/personas,
                                          strata = ~estrato_nombre,
                                     nest = T)

lista_n <- lista_nominal %>% group_by(sexo, edad) %>%
  summarise(Freq=sum(n))
diseño_complejo_pe<-postStratify(diseño_complejo,
                                 ~sexo+edad,
                                 partial = T,
                                 population = lista_n)


# Gráficas ----------------------------------------------------------------
# Ánimo
survey::svymean(design = diseño_complejo_pe %>%
                  subset(estrato_nombre == "Centro oriente"),~emocion) %>%
  as_tibble(rownames = "Emocion") %>%
  mutate(Emocion=gsub(pattern = "emocion",
                        x = Emocion, replacement = ""),
         mean = round(mean,digits =  3)) %>%
  filter(mean>= .01) %>%
  ggplot(aes(x=reorder(str_wrap(Emocion,30),mean), y=mean,
             fill=Emocion,
  )) +
  geom_bar(stat="identity") +
  ggfittext::geom_bar_text(aes(label=scales::percent(mean, accuracy = .1)), size = 20)+
  scale_x_discrete(name="Emoción")+
  scale_y_continuous(name="Porcentaje",
                     labels = scales::label_percent())+
  scale_fill_manual(values = c(rep("#8C2E48", 4),"grey50", rep("#8C2E48", 3)))+
  coord_flip() +
  theme(plot.background = element_blank(),
        legend.position = "none",
        text = element_text(size=22, family = "Avenir"),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey20",
                                  linetype = 3,
                                  size = .5),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank()
  )


# Voto
survey::svymean(design = diseño_complejo_pe%>%
                  subset(estrato_nombre =="Periferia sur poniente"),~votar) %>%
  as_tibble(rownames = "voto") %>%
  mutate(voto=gsub(pattern = "votar",
                      x = voto, replacement = ""),
         mean = round(mean,digits =  3)) %>%
  mutate(voto = case_when(voto == "9 No contesta"~"No contesta",
                          voto == "Estoy seguro que iré a "~"Estoy seguro que iré a votar",
                          voto == "No iré a "~ "No iré a votar",
                          voto == "Todavía no sé si iré a "~"Todavía no sé si iré a votar")) %>%
  ggplot(aes(x=reorder(str_wrap(voto,30),mean), y=mean,
             fill=voto,
  )) +
  geom_bar(stat="identity") +
  ggfittext::geom_bar_text(aes(label=scales::percent(mean, accuracy = .1)), size = 20)+
  scale_x_discrete(name="Respuesta")+
  scale_y_continuous(name="Porcentaje",
                     labels = scales::label_percent())+
  scale_fill_manual(values = c("#B0CFA9", "grey50", "#E3526F","#F79D5C" ) )+
  coord_flip() +
  theme(plot.background = element_blank(),
        legend.position = "none",
        text = element_text(size=22, family = "Avenir"),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey20",
                                  linetype = 3,
                                  size = .5),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank()
  )

# Votación por Carlos
survey::svymean(design = diseño_complejo_pe %>%
                  subset(estrato_nombre == "Periferia sur poniente") ,~municipal2) %>%
  as_tibble(rownames = "Candidato") %>%
  mutate(Candidato=gsub(pattern = "municipal2",
                        x = Candidato, replacement = "")) %>%
  ggplot(aes(x=reorder(str_wrap(Candidato,30),mean), y=mean,
             fill=Candidato,
             )) +
  geom_bar(stat="identity") +
  ggfittext::geom_bar_text(aes(label=scales::percent(mean, accuracy = .1)), size = 20)+
  scale_x_discrete(name="Candidatos")+
  scale_y_continuous(name="Intención de voto",
                     labels = scales::label_percent())+
  scale_fill_manual(values = c("#990c0c","grey50", "grey70","grey60",  "#e16924","#1247A2"),
                    guide=F)+
  coord_flip() +
  theme(plot.background = element_blank(),
        legend.position = "none",
        text = element_text(size=22, family = "Avenir"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey20",
                                  linetype = 3,
                                  size = .5),
        axis.ticks.y = element_blank()
        )

# Votación por Partido
survey::svymean(design = diseño_complejo_pe %>%
                  subset(estrato_nombre == "Periferia sur poniente"),~partido2) %>%
  as_tibble(rownames = "Partido") %>%
  mutate(Partido=gsub(pattern = "partido2",
                        x = Partido, replacement = "")) %>%
  ggplot(aes(x=reorder(str_wrap(Partido,30),mean), y=mean,
             fill=Partido,
  )) +
  geom_bar(stat="identity") +
  ggfittext::geom_bar_text(aes(label=scales::percent(mean, accuracy = .1)), size = 20)+
  scale_x_discrete(name="Partido")+
  scale_y_continuous(name="Intención de voto",
                     labels = scales::label_percent())+
  scale_fill_manual(values = c("#990c0c",
                               "#e16924",
                               "grey65",
                               "grey60",
                               # "grey50",
                               "grey70",
                               "#1247A2",
                               "#0B840B",
                               "#FFC928",
                               "#CC1111",
                               "grey70"),
                    guide=F)+
  coord_flip() +
  theme(plot.background = element_blank(),
        legend.position = "none",
        text = element_text(size=22, family = "Avenir"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey20",
                                  linetype = 3,
                                  size = .5),
        axis.ticks.y = element_blank()
  )

#Evaluación mejor
survey::svymean(design = diseño_complejo_pe  %>%
                  subset(estrato_nombre == "Centro oriente"),~servicio_mejor) %>%
  as_tibble(rownames = "servicio") %>%
  mutate(servicio=gsub(pattern = "servicio_mejor",
                      x = servicio, replacement = ""),
         servicio = case_when(mean <.01 ~"Otros", T ~servicio)) %>%
  group_by(servicio) %>%  summarise(mean =round(sum(mean), 3 )) %>%
  # filter(mean>= .001) %>%
  ggplot(aes(x=reorder(str_wrap(servicio,30),mean), y=mean,
             fill=servicio  )) +
  geom_bar(stat="identity") +
  ggfittext::geom_bar_text(aes(label=scales::percent(mean, accuracy = .1)), size = 20)+
  scale_x_discrete(name="Servicio")+
  scale_y_continuous(name="Porcentaje",
                     labels = scales::label_percent())+
  scale_fill_manual(values = c("Drenaje" = "#B0CFA9",
                               "Agua potable"="#B0CFA9",
                               "Alumbrado público" = "#B0CFA9",
                               "Pavimentación de calles" = "#B0CFA9",
                               "Recolección de basura" = "#B0CFA9",
                               "Seguridad pública" = "#B0CFA9",
                               "Transporte público" = "#B0CFA9",
                               "Ninguno" ="grey50",
                               "No sabe / No contesta"= "grey60",
                               "Otros" = "grey80"))+
  coord_flip() +
  theme(plot.background = element_blank(),
        legend.position = "none",
        text = element_text(size=22, family = "Avenir"),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey20",
                                  linetype = 3,
                                  size = .5),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank()
  )
#Evaluación peor
survey::svymean(design = diseño_complejo_pe %>%
                  subset(estrato_nombre == "Centro poniente"),~servicio_peor) %>%
  as_tibble(rownames = "servicio") %>%
  mutate(servicio=gsub(pattern = "servicio_peor",
                       x = servicio, replacement = ""),
         servicio = case_when(mean <.01 ~"Otros", T ~servicio)) %>%
  group_by(servicio) %>%  summarise(mean =round(sum(mean), 3 )) %>%
  # filter(mean>= .001) %>%
  ggplot(aes(x=reorder(str_wrap(servicio,30),mean), y=mean,
             fill=servicio  )) +
  geom_bar(stat="identity") +
  ggfittext::geom_bar_text(aes(label=scales::percent(mean, accuracy = .1)), size = 20)+
  scale_x_discrete(name="Servicio")+
  scale_y_continuous(name="Porcentaje",
                     labels = scales::label_percent())+
  scale_fill_manual(values = c( "Agua potable"="#C2465F",
                                "Drenaje"="#C2465F",
                                "Alumbrado público" = "#C2465F",
                                "Pavimentación de calles" = "#C2465F",
                                "Recolección de basura" = "#C2465F",
                                "Seguridad pública" = "#C2465F",
                                "Transporte público" = "#C2465F",
                                "Educación pública" = "#C2465F",
                               "Ninguno" ="grey50",
                               "No sabe / No contesta"= "grey60",
                               "Otros" = "grey80"))+
  coord_flip() +
  theme(plot.background = element_blank(),
        legend.position = "none",
        text = element_text(size=22, family = "Avenir"),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey20",
                                  linetype = 3,
                                  size = .5),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank()
  )

# Opinión AMLO

# survey::svymean(design = diseño_complejo_pe  %>%
#                   subset(estrato_nombre == "Centro oriente"),
#                 ~amlo) %>%
#   as_tibble(rownames = "Opinión") %>%
#   mutate(Opinión=gsub(pattern = "amlo",
#                       x = Opinión, replacement = ""),
#          Opinión=factor(x = Opinión,
#                         levels = c("Aprueba mucho", "Aprueba poco", "Desaprueba poco", "Desaprueba mucho", "No sabe / No contesta"))) %>%
#   ggplot(aes(x=Opinión, y=mean)) +
#   scale_y_continuous(name="Porcentaje", labels=scales::percent_format())+
#   theme(plot.background = element_blank(),
#         legend.position = "bottom",
#         text = element_text(size=22, family = "Avenir"),
#         panel.background = element_blank(),
#         panel.grid = element_line(colour = "grey20",
#                                   linetype = 3,
#                                   size = .5),
#         panel.grid.minor = element_blank(),
#         axis.ticks.y = element_blank()
#   )+
#   geom_bar(stat="identity") +
#   ggfittext::geom_bar_text(aes(label=scales::percent(mean, accuracy = .1)), size = 20)+
#   scale_fill_manual()


survey::svymean(design = diseño_complejo_pe  %>%
                  subset(estrato_nombre == "Periferia sur poniente"),
                ~amlo) %>%
  as_tibble(rownames = "Opinión") %>%
  mutate(Opinión=gsub(pattern = "amlo",
                      x = Opinión, replacement = ""),
         Opinión=factor(x = Opinión,
                        levels = c("Aprueba mucho", "Aprueba poco", "Desaprueba poco", "Desaprueba mucho", "No sabe / No contesta"))) %>%
  ggplot(aes(x=Opinión, y=mean, fill=Opinión)) +
  scale_y_continuous(name="Porcentaje", labels=scales::percent_format())+
  theme(plot.background = element_blank(),
        legend.position = "none",
        text = element_text(size=22, family = "Avenir"),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey20",
                                  linetype = 3,
                                  size = .5),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank()
  )+
  geom_bar(stat="identity", width = .65) +
  ggfittext::geom_bar_text(aes(label=scales::percent(mean, accuracy = .1)), size = 20)+
  scale_fill_manual(values =c("#82C286","#B0CFA9", "#C2465F",
                              "#E3526F","grey50"), guide=F)

survey::svymean(design = diseño_complejo_pe%>%
                  subset(estrato_nombre == "Periferia sur poniente"),
                ~carlos) %>%
  as_tibble(rownames = "Opinión") %>%
  mutate(Opinión=gsub(pattern = "carlos",
                      x = Opinión, replacement = ""),
         Opinión=factor(x = Opinión,
                        levels = c("Aprueba mucho", "Aprueba poco", "Desaprueba poco", "Desaprueba mucho", "No sabe / No contesta"))) %>%
  ggplot(aes(x=Opinión, y=mean, fill=Opinión)) +
  scale_y_continuous(name="Porcentaje", labels=scales::percent_format())+
  theme(plot.background = element_blank(),
        text = element_text(size=22, family = "Avenir"),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey20",
                                  linetype = 3,
                                  size = .5),
        axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank()
  )+
  geom_bar(stat="identity", width =.65) +
  ggfittext::geom_bar_text(aes(label=scales::percent(mean, accuracy = .1)), size = 20)+
  scale_fill_manual(values =c("#82C286","#B0CFA9", "#C2465F",
                                   "#E3526F","grey50"), guide=F)


# Mapas -------------------------------------------------------------------
seccion<- read_sf("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/SHP/2017/07 - Chiapas/SECCION.shp")%>%
  st_transform(4326)
estratos <- read_rds ("data/manzanas_estrato.rds")
seccion %>% inner_join(estratos , by = "SECCION")  %>%

estratos %>%   ggplot()+geom_sf(aes(fill = estrato, geometry = geometry))+
  scale_fill_manual(values = c())
  theme_minimal()+
  theme(panel.grid = element_blank())

