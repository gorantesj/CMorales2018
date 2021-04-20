library(tidyverse)
library(survey)
bd <- read_csv("data/encuestas.csv")
bd <- bd %>% mutate(id=row_number())
manzanas <-  read_rds("data/manzanas_estrato.rds")
# Secciones que no pertencen al marco muestral
bd <- bd %>% inner_join(manzanas_en_muestra %>%
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

# Votación por Carlos
survey::svymean(design = diseño_complejo_pe,~municipal2) %>%
  as_tibble(rownames = "Candidato") %>%
  mutate(Candidato=gsub(pattern = "municipal2",
                        x = Candidato, replacement = "")) %>%
  ggplot(aes(x=reorder(str_wrap(Candidato,30),mean), y=mean,
             fill=Candidato,
             )) +
  geom_bar(stat="identity") +
  ggfittext::geom_bar_text(aes(label=scales::percent(mean)))+
  scale_x_discrete(name="Candidatos")+
  scale_y_continuous(name="Intención de voto",
                     labels = scales::label_percent())+
  scale_fill_manual(values = c("#990c0c","grey50", "grey70","grey60",  "#e16924","#1247A2"),
                    guide=F)+
  coord_flip() +
  theme(plot.background = element_blank(),
        text = element_text(size=18),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey20",
                                  linetype = 3,
                                  size = .5),
        axis.ticks.y = element_blank()
        )

# Votación por Partido
survey::svymean(design = diseño_complejo_pe,~partido2) %>%
  as_tibble(rownames = "Partido") %>%
  mutate(Partido=gsub(pattern = "partido2",
                        x = Partido, replacement = "")) %>%
  ggplot(aes(x=reorder(str_wrap(Partido,30),mean), y=mean,
             fill=Partido,
  )) +
  geom_bar(stat="identity") +
  scale_x_discrete(name="Candidatos")+
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
        text = element_text(size=18),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey20",
                                  linetype = 3,
                                  size = .5),
        axis.ticks.y = element_blank()
  )

# Opinión AMLO

survey::svymean(design = diseño_complejo_pe,
                ~amlo) %>%
  as_tibble(rownames = "Opinión") %>%
  mutate(Opinión=gsub(pattern = "amlo",
                      x = Opinión, replacement = ""),
         Opinión=factor(x = Opinión,
                        levels = c("Aprueba mucho", "Aprueba poco", "Desaprueba poco", "Desaprueba mucho", "No sabe / No contesta"))) %>%
  ggplot(aes(x=Opinión, y=mean)) +
  scale_y_continuous(name="Porcentaje", labels=scales::percent_format())+
  theme(plot.background = element_blank(),
        text = element_text(size=18),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey20",
                                  linetype = 3,
                                  size = .5),
        axis.ticks.x = element_blank()
  )+
  geom_bar(stat="identity") +
  scale_fill_manual()


survey::svymean(design = diseño_complejo_pe,
                ~amlo) %>%
  as_tibble(rownames = "Opinión") %>%
  mutate(Opinión=gsub(pattern = "amlo",
                      x = Opinión, replacement = ""),
         Opinión=factor(x = Opinión,
                        levels = c("Aprueba mucho", "Aprueba poco", "Desaprueba poco", "Desaprueba mucho", "No sabe / No contesta"))) %>%
  ggplot(aes(x=Opinión, y=mean, fill=Opinión)) +
  scale_y_continuous(name="Porcentaje", labels=scales::percent_format())+
  theme(plot.background = element_blank(),
        text = element_text(size=18),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey20",
                                  linetype = 3,
                                  size = .5),
        axis.ticks.x = element_blank()
  )+
  geom_bar(stat="identity") +
  scale_fill_manual(values = rev(c("grey60","#C92424", "#C25F60",
                               "#8CCF94","#53B046")), guide=F)

survey::svymean(design = diseño_complejo_pe,
                ~carlos) %>%
  as_tibble(rownames = "Opinión") %>%
  mutate(Opinión=gsub(pattern = "carlos",
                      x = Opinión, replacement = ""),
         Opinión=factor(x = Opinión,
                        levels = c("Aprueba mucho", "Aprueba poco", "Desaprueba poco", "Desaprueba mucho", "No sabe / No contesta"))) %>%
  ggplot(aes(x=Opinión, y=mean, fill=Opinión)) +
  scale_y_continuous(name="Porcentaje", labels=scales::percent_format())+
  theme(plot.background = element_blank(),
        text = element_text(size=18),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey20",
                                  linetype = 3,
                                  size = .5),
        axis.ticks.x = element_blank()
  )+
  geom_bar(stat="identity") +
  scale_fill_manual(values = rev(c("grey60","#C92424", "#C25F60",
                                   "#8CCF94","#53B046")), guide=F)
