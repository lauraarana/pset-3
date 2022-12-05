## Problem set 3 Taller de R
##Mariana  Ordonez 202021691
##Ivan Rodriguez 202112819
##Laura Sofia Arana 202020594

#"R version 4.2.2 (2022-10-31)"

## Limpiar el entorno
rm(list=ls())

require(pacman)
p_load(tidyverse, rio, 
       arrow, ## read parque files
       broom, # tidy-coefficients
       mfx, # marginal effects
       margins,  # marginal effects
       estimatr, # robust standard errors
       lmtest, # HAC (Newey-West) standard errors
       fixest, # hdfe regressions (feols)
       modelsummary, # Coefplot with modelplot
       stargazer, # export tables to latex 
       coefplot
)

##########          Punto 1          ##########

#1.1
regresiones <- import ("input/data_regresiones.rds") 

modelo1 <- lm(price ~ surface_total + bathrooms, data=regresiones)
summary(modelo1)

modelo2 <- lm(price ~ surface_total + property_type, data=regresiones)
summary(modelo2)

modelo3 <- lm(price ~ surface_total + dist_cbd + dist_cole + dist_park, data=regresiones)
summary(modelo3)

#1.2 y 1.3
tabla <- full_join(tidy(modelo1), tidy(modelo2),"term") %>% full_join(tidy(modelo3),"term") 
tabla
                   
models <- msummary(list(modelo1,modelo2,modelo3))
models

stargazer(modelo1, modelo2, modelo3,
          type= 'text',
          dep.var.labels = c('','Price',''), 
          regresiones = FALSE,
          digits = 3, 
          out = paste0('output/resultados_regresiones.xlsx '))

##Coefplot
mods = list(modelo1, modelo2, modelo3)

modplot <- coefplot(mods, title = "Coeficientes de stimacion de 3 modelos")

export(modplot, file = "output/plot_regresiones.png" )

#png(modplot = paste0('output/plot_regresiones.png '))


##########          Punto 2          ##########
## setup
rm(list=ls())
require(pacman)
p_load(tidyverse,ggsn,ggmap,osmdata,leaflet,sf,tmaptools)

## restaurantes
rest_osm <- opq(bbox = getbb("Cartagena de Indias, Colombia")) %>%
  add_osm_feature(key="amenity" , value="restaurant") %>%
  osmdata_sf()
rest_osm
restaurantes <- rest_osm$osm_points
leaflet() %>% addTiles() %>% addCircles(data=restaurantes)

## parques
park_osm <- opq(bbox = getbb("Cartagena de Indias, Colombia")) %>%
  add_osm_feature(key="leisure" , value="park") %>%
  osmdata_sf()
park_osm
parques <- park_osm$osm_polygons
leaflet() %>% addTiles() %>% addPolygons(data=parques)

## Castillo de San Felipe
punto <- geocode_OSM("Castillo de San Felipe, Cartagena", as.sf=T)
punto
leaflet() %>% addTiles() %>% addCircles(data=punto)

## polygono bogota
car_osm <- opq(bbox = getbb("Cartagena de Indias, Colombia")) %>%
  add_osm_feature(key="boundary", value= "administrative") %>%
  osmdata_sf()
car <- car_osm$osm_multipolygons %>% subset(admin_level==6 & name=="Cartagena de Indias")
car <- st_transform(car,4326)
leaflet() %>% addTiles() %>% addPolygons(data=car)

## subset
parques <- parques[car,] 
restaurantes <- restaurantes[car,]

## add osm layer
osm_layer <- get_stamenmap(bbox= as.vector(st_bbox(car)), maptype="toner", source="osm", zoom=11)

## plot
map <- ggmap(osm_layer) +
  geom_sf(data=car, alpha=0.3 , inherit.aes=F) +
  geom_sf(data=restaurantes, aes(color="A"), inherit.aes = F) +
  geom_sf(data=parques, aes(color="B"), inherit.aes = F)+
  geom_sf(data=punto, aes(color="C"),inherit.aes = F)+
  scale_color_manual(labels=c("A"="Restaurantes","B"="Parques" , "C"="Castillo de San Felipe"),
                     values=c("A"="red","B"="green" , "C"="blue")) + theme_test()
map

export


##########          Punto 3          ##########
rm(list=ls())
require(pacman)
p_load(rvest, rio, stringi,tm,cluster,wordcloud, wordcloud2,
       RColorBrewer, RCurl, XML)

setwd("C:/Users/Ivan/Desktop/R/Taller de R/Extracciones-desde-Git/pset-3")

"3.1 Desde la consola de Rstudio lea la siguiente url 
https://es.wikipedia.org/wiki y cree un objeto que contenga el HTML de la página
como un objeto xml_document."
browseURL("https://es.wikipedia.org/wiki/Departamentos_de_Colombia")
a1="https://es.wikipedia.org/wiki/Departamentos_de_Colombia"
a=read_html(a1)
class(a)
"3.2 Use el xpath para extraer el título de la página 
(Departamentos de Colombia)."
a %>% html_nodes(xpath = '//*[@id="firstHeading"]/span')
"3.3. Extraiga la tabla que contiene los departamentos de Colombia y exporte el objeto en un archivo
output/tabla_departamento.xlsx"
b1 = a %>% html_table()
length(b1)
b=b1[4]
dir.create("C:/Users/Ivan/Desktop/R/Taller de R/Extracciones-desde-Git/pset-3/output")
export(b,"output/tabla_departamento.xlsx")

"3.4 Extraiga los parrafos del documento (elementos con etiqueta p) y genere una nube de palabras.
Exporte el objeto en un archivo output/nube_palabras.png"
c1= a %>% html_nodes("p")
c2=html_text(a)
content <- stringi::stri_extract_all_words(c2, simplify = TRUE)
c=wordcloud(content, min.freq = 10, colors = RColorBrewer::brewer.pal(5,"Spectral"))
png(file="C:/Users/Ivan/Desktop/R/Taller de R/Extracciones-desde-Git/pset-3/output/nube_palabras.png",
    width=600, height=350)
c=wordcloud(content, min.freq = 10, colors = RColorBrewer::brewer.pal(5,"Spectral"))
dev.off()
