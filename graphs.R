library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(scales)
library(glue)
library(leaflet)
library(skimr)
library(countrycode)
library(shiny)
library(shinydashboard)
library(sf)
library(rgeos)
library(ggspatial)
library(rworldmap)
library(shinycssloaders)
library(tidyverse)
library(tidyverse)
library(magrittr)
library(png)
library(highcharter)
library(shinyjs)
library(tmap)
library(tmaptools)
library(mapdeck)


prison_population <- prison_population_rate
prison_population <- st_read()


#map for prisoner population for each country

mapdata <- map_data("world")
View(mapdata)
mapdata <- left_join(mapdata, prison_population, by="region")
view(mapdata)
#mapdata1: complete table
mapdata1 <- mapdata %>% filter(!is.na(mapdata$`Prison population rate (World Prison Brief 2018)`))
map1 <- ggplot(mapdata1, aes(x = long, y = lat, group=group))+
  geom_polygon(aes(fill = `Prison population rate (World Prison Brief 2018)`),color = "black")
map1
map2 <- map1 + scale_fill_gradient(name = "% prison population  rate ", low ="yellow", high = "red", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
map2
map2
mapdata1 <- st_as_sf(x=mapdata1, coords = c("long","lat"), crs = projcrs)
data("mapdata1")
tm_shape(mapdata1)+
  tm_polygons("HPI")
tmap_mode("view")
tmap_last()
