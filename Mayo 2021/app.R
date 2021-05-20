#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(readr)
library(readxl)
library(gt)
library(tidyr)
library(glue)
library(lubridate)
library(ggplot2)
library(forcats)
library(magrittr)
library(shinycssloaders)


muestra <- st_read("muestra.shp")
seccion <- st_read("secciones_muestra.shp")

bd_m <- read_csv("cuotas.csv")
bd_m %<>%  mutate(seccion = SECCION, n = entrevistas)

eliminar <- read_csv("eliminar.csv")
bd <- read_xlsx("bd.xlsx") %>%
  # filter(Date > ymd_hm("2021-04-24 13:00")) %>%
  anti_join(eliminar) %>% mutate(duracion = VEnd-VStart)

# Correcciones ------------------------------------------------------------
bd %<>% filter(duracion >= 2 )
# bd <- bd %>% filter(Srvyr != "Tadeo Sevilla")
# bd <- bd %>% mutate(SECCIO = case_when(SbjNum %in% c(150900528, 150900529, 150900530, 150900531)~ "684",
#                                        SbjNum == 150854925~"679",
#                                        SbjNum %in% c(
#                                          150929993, 150931150, 150931151, 150931311, 150940225, 150940226,
#                                          150940227, 150940630, 150941044, 150942032, 150942476, 150943285,
#                                          150944810, 150944811, 150944812, 150944813, 150944814, 150944815,
#                                          150944816, 150944817, 150944818, 150945446, 150945668, 150945669,
#                                          150945670, 150945671, 150945672, 150945673, 150945674, 150945765,
#                                          150946950, 150947422, 150947767, 150948152, 150948535, 150948842,
#                                          150949419, 150949420
#                                        )~"682",
#                                        SbjNum == 150962679~"683",
#                                        T~SECCIO),
#                     Longitude = if_else(SbjNum %in% c(
#                       150964972, 150964973, 150964974, 150964975, 150964976, 150964977,
#                       150964978, 150964979, 150964980, 150964981, 150964982, 150964984),true = -99.07706,
#                       false = Longitude
#                     ),
#                     Latitude = if_else(SbjNum %in% c(
#                       150964972, 150964973, 150964974, 150964975, 150964976, 150964977,
#                       150964978, 150964979, 150964980, 150964981, 150964982, 150964984),true = "18.9741284",
#                       false = Latitude
#                     )
# )
#





enc <- bd %>% select(seccion = SECCIO,PC,PD, Encuestador = Srvyr, id = SbjNum, Latitude,Longitude) %>%
  # na.omit  %>%
  mutate(Longitude = as.character(Longitude))

bd_e <- enc %>% as_tibble %>%
  mutate(edad = as.character(cut(PC,c(17,24,34,44,54,64,200),
                                 c("18 a 24 años","25 a 34 años","35 a 44 años","45 a 54 años","55 a 64 años","65 años o más")))) %>%
  group_by(seccion) %>%
  count(edad,PD) %>%
  ungroup %>% select(seccion,sexo =PD,edad,hecho = n) %>%
  mutate(seccion = as.numeric(seccion))

faltan <- bd_m %>% left_join(bd_e) %>% mutate(faltan = if_else(is.na(hecho),true = n,false = n - hecho))

enc <- enc %>%  na.omit() %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = "+init=epsg:4326")
content <- paste(sep = " ",
                 "<b>Total de encuestas</b>:","<br/>",
                 faltan$hecho %>%  sum(na.rm = T),"al",
                 format(today(),"%d de abril %y")
)
pal <- colorFactor(rainbow(n=length(unique(enc$Encuestador))),domain = unique(enc$Encuestador))
ui <-
  navbarPage("Tuxtla",
             tabPanel("Mapa",
                      leafletOutput(outputId = "map", height = 800),

                      # Shiny versions prior to 0.11 should use class = "modal" instead.
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                    width = 330, height = "auto",
                                    HTML('<button data-toggle="collapse" data-target="#demo">Min/max</button>'),
                                    tags$div(id = 'demo',  class="collapse",
                                             selectInput("seccion", "Sección", c("Seleccione..."="",unique(muestra$SECCION))),
                                             gt_output("faltantes"),
                                             hr(),
                                             actionButton("des", "Deseleccionar")
                                    )
                      )
             ),
             tabPanel("Auditoría",
                      passwordInput("psw","Contraseña"),
                      uiOutput("graficas")
             )
  )

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$map <- renderLeaflet({
    bbox <- muestra %>%  st_union() %>% st_bbox()
    leaflet(muestra) %>% addPolygons(weight = 1, label = ~glue::glue("Sección: {SECCION}, Manzana: {MANZANA}")) %>%
      addProviderTiles("CartoDB.Positron") %>%
      leaflet.extras::addResetMapButton() %>%
      addCircleMarkers(data = enc,
                       group = "encuestas_iniciales",stroke = F,color = ~pal(Encuestador),fillOpacity = 1,
                       label = ~glue("Sección: {seccion} id: {id}, encuestador: {Encuestador}")) %>%
      addPopups((bbox[[1]]+bbox[[3]])/2, bbox[[4]], content, group = "info",
                options = popupOptions(closeButton = FALSE)
      ) %>%
      addLegend(pal = pal, values = ~Encuestador, data = enc, position = "bottomleft",group = "encuestador") %>%
      addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE,
                                         autoCenter = TRUE, maxZoom = 10,
                                         setView = TRUE)) %>%
      addLayersControl(overlayGroups = "encuestas_iniciales",position = "bottomright")
  })

  output$faltantes <- render_gt({
    validate(need(input$seccion != "",message =  "Escoja una sección"))
    faltan %>% filter(seccion == input$seccion) %>% select(sexo,edad,faltan) %>%
      pivot_wider(names_from = sexo,values_from = faltan) %>% gt() %>% tab_header(title = "Faltantes",subtitle = glue("Sección: {input$seccion}"))
  },)

  proxy <- leafletProxy("map")

  observeEvent(input$seccion,{
    req(input$seccion)
    man <- muestra %>% filter(SECCION == input$seccion)
    enc_secc <- enc %>% filter(seccion == input$seccion)
    bbox2 <- enc_secc %>% st_bbox()
    seccs <- seccion %>% filter(SECCION == input$seccion)
    bbox1 <- seccs %>% sf::st_bbox()
    bbox <- c(min(bbox1[[1]],bbox2[[1]], na.rm = TRUE),
              min(bbox1[[2]],bbox2[[2]], na.rm = TRUE),
              max(bbox1[[3]],bbox2[[3]], na.rm = TRUE),
              max(bbox1[[4]],bbox2[[4]], na.rm = TRUE)
    )
    content <- paste(sep = " ",
                     "<b>Seccion: </b>", input$seccion, "<br/>",
                     "<b>Total de encuestas:</b>",nrow(enc_secc),"<br/>",
                     "<b>Última actualización: </b>",
                     format(today(),"%d de abril %y")
    )
    ja <- proxy %>% flyToBounds(bbox[[1]],bbox[[2]],bbox[[3]],bbox[[4]]) %>%
      clearGroup(group = "man") %>% clearGroup(group = "encuestas") %>% clearGroup(group = "seccs") %>%
      clearGroup(group = "info") %>% clearGroup(group = "encuestas_iniciales") %>%
      addPolygons(data = man, fill = F, color = "red", group = "man", weight = 5,fillOpacity = .1,stroke = T) %>%
      addPolygons(data = seccs, fill = F, color = "black", group = "seccs", weight = 5, opacity = 1)

    if(nrow(enc_secc) > 0){
      ja %>% addCircleMarkers(data = enc_secc,
                              group = "encuestas",stroke = F,color = ~pal(Encuestador),fillOpacity = 1,
                              label = ~glue("id: {id}, encuestador: {Encuestador}")) %>%
        addPopups((bbox[[1]]+bbox[[3]])/2, bbox[[4]], content,group = "info",
                  options = popupOptions(closeButton = FALSE)
        )
    } else{
      ja
    }

  })

  observeEvent(input$des,{
    updateSelectInput("seccion",session = session, selected = "")
    bbox <- st_bbox(muestra)
    proxy %>% clearGroup(group = "seccs") %>% clearGroup(group = "encuestas") %>%
      clearGroup(group = "info") %>%
      clearGroup(group = "man") %>%
      flyToBounds(bbox[[1]],bbox[[2]],bbox[[3]],bbox[[4]]) %>%
      addCircleMarkers(data = enc,
                       group = "encuestas_iniciales",stroke = F,color = ~pal(Encuestador),fillOpacity = 1,
                       label = ~glue("Sección: {seccion} id: {id}, encuestador: {Encuestador}")) %>%
      # addPolygons(data = muestra, color = "red", weight = 1, group = "inicio",
      #             label = ~glue::glue("Sección: {SECCION}, Manzana: {MANZANA}") )%>%
      addPopups((bbox[[1]]+bbox[[3]])/2, bbox[[4]], content, group = "info",
                options = popupOptions(closeButton = FALSE)
      )
  })

  output$graficas <- renderUI({
    validate(
      need(input$psw == "chambeadora",message = "Escriba la contraseña correcta")
    )
    tagList(
      fluidRow(
        column(6,
               withSpinner(plotOutput("total"))
        ),
        column(6,
               withSpinner(plotOutput("hora"))
        )
      ),
      fluidRow(
        withSpinner(plotOutput("histograma"))
      ),
      fluidRow(
        column(6,
               withSpinner(plotOutput("mediana"))
        ),
        column(6,
               withSpinner(plotOutput("varianza"))
        )
      )
    )
  })

  output$total <- renderPlot({

    bd %>% count(Srvyr) %>% mutate(mediana = median(n)) %>% mutate(Srvyr = fct_reorder(Srvyr,n)) %>% ggplot() +
      geom_col(aes(x = n, y = Srvyr, fill = n>mediana))
  })

  output$hora <- renderPlot({

    bd %>% mutate(dia = floor_date(Date,"day"),hora = floor_date(Date,"hour")) %>%
      ggplot() + geom_bar(aes(x = hora)) +
      geom_line(aes(x = dia),stat = "count", color = "red")
  })

  output$histograma <- renderPlot({
    bd %>% transmute(duracion = VEnd-VStart, SbjNum) %>% arrange(desc(duracion)) %>% ggplot() +
      geom_histogram(aes(x=duracion),bins = 100)
  })

  output$mediana <- renderPlot({
    bd %>% mutate(mediana = median(duracion)) %>%
      mutate(Srvyr = fct_reorder(Srvyr,duracion,.fun = median)) %>% ggplot() +
      geom_boxplot(aes(x = duracion, y = Srvyr)) + geom_vline(aes(xintercept=mediana), color = "red") +
      ggtitle("Ordenado por mediana")
  })

  output$varianza <- renderPlot({
    bd %>% mutate(duracion = VEnd-VStart) %>%
      mutate(Srvyr = fct_reorder(Srvyr,duracion,.fun = var))%>% ggplot() +
      geom_boxplot(aes(x = duracion, y = Srvyr))+
      ggtitle("Ordenado por varianza")
  })

}

# Run the application
shinyApp(ui = ui, server = server)
