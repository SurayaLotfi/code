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



prison_population_rate <- read_csv("Uni/SEM 4/Intro to DS/Assignment/group/CRIME ANALYZER/prison-population-rate.csv")

library(readr)
detected_trafficking <- read_csv("Uni/SEM 4/Intro to DS/Assignment/group/CRIME ANALYZER/filzah/detected-trafficking.csv")

library(readr)
data_cts_violent_and_sexual_crime <- read_csv("Uni/SEM 4/Intro to DS/Assignment/group/CRIME ANALYZER/filzah/data_cts_violent_and_sexual_crime.csv")

library(readr)
data_cts_intentional_homicide <- read_csv("Uni/SEM 4/Intro to DS/Assignment/group/CRIME ANALYZER/filzah/data_cts_intentional_homicide.csv")

library(readr)
data_cts_corruption_and_economic_crime <- read_csv("Uni/SEM 4/Intro to DS/Assignment/group/CRIME ANALYZER/filzah/data_cts_corruption_and_economic_crime.csv")

max(data_cts_intentional_homicide$`...10`)
test <- data_cts_intentional_homicide%>% group_by(Region,Year)%>%
  summarise(total_value = sum(VALUE)/1000)
test
View(test)
ih_line_data <- data_cts_intentional_homicide %>% group_by(Region,Year)%>%
  summarise(rate = sum(VALUE)/1000)%>% ungroup%>%
  filter(rate!=0)%>%
  mutate(label = paste0('<b>',Region,'</b>',
                        '(', Year, ')<br>',
                        'Value(rate): ',round(rate,2),'per 1,000'))
View(ih_line_data)

vs_line_data <- data_cts_violent_and_sexual_crime %>%  group_by(Region,Year)%>%
  summarise(rate = sum(VALUE)/1000)%>% ungroup%>%
  filter(rate!=0)%>%
  mutate(label = paste0('<b>',Region,'</b>',
                        '(', Year, ')<br>',
                        'Value(rate): ',round(rate,2),'per 1,000'))
View(vs_line_data)

library(readr)corruption_economic_crime <- read_csv("Uni/SEM 4/Intro to DS/Assignment/group/CRIME ANALYZER/filzah/corruption_economic_crime.csv")
View(corruption_economic_crime)
ce_line_data <- corruption_economic_crime %>% group_by(Region,Year)%>%
  summarise(rate = sum(VALUE)/1000)%>%ungroup%>%
  filter(rate!=0)%>% mutate(label = paste0('<b>',Region,'</b>',
                                   '(', Year, ')<br>',
                                   'Value(rate): ',round(rate,2),'per 1,000'))
View(ce_line_data)


detected_trafficking <-  na.omit(detected_trafficking)

#gather data multiple columns
detected_trafficking<-gather(detected_trafficking, key = "Year",value = "VALUE", 4:18)
View(detected_trafficking)

dt_line_data <- detected_trafficking %>% group_by(Region,Year)%>%
  summarise(rate = sum(VALUE)/1000)%>%ungroup%>%
  filter(rate!=0)%>% mutate(label = paste0('<b>',Region,'</b>',
                                   '(', Year, ')<br>',
                                   'Value(rate): ',round(rate,2),'per 1,000'))
View(dt_line_data)

  


#kecikkan value VALUE in crime justice
#multiple_bar <- data_cts_access_and_functioning_of_justice %>% group_by(Indicator,Category)%>%
  #summarise(ratio = sum(VALUE)/100000)%>%ungroup%>%filter(ratio!=0)%>%
  #mutate(round(ratio,2))
#multiple_bar <- multiple_bar[!grepl('Total',multiple_bar$Category),]
#View(multiple_bar)

#traffic_linedata <- traffic_linedata %>% mutate(Year = data.frame())
#unique(data_cts_access_and_functioning_of_justice$Indicator)
#unique(removetotal$Sex)
#removetotal <- data_cts_access_and_functioning_of_justice[!grepl('Total',data_cts_access_and_functioning_of_justice$Age),] 
#removetotal
#data_cts_access_and_functioning_of_justice <- data_cts_access_and_functioning_of_justice[!grepl('Total',data_cts_access_and_functioning_of_justice$Sex),] 
#View(data_cts_access_and_functioning_of_justice)
#justice <- data_cts_access_and_functioning_of_justice[!grepl('Total',data_cts_access_and_functioning_of_justice$Category),]
#View(justice)
#unique(data_cts_access_and_functioning_of_justice$Category)




#show rows with only selected crimes
View(data_cts_access_and_functioning_of_justice)
justice_data <- data_cts_access_and_functioning_of_justice%>%
  filter(grepl('by selected crime',Dimension))
View(justice_data)
justice_data <- justice_data[!grepl('Counts',justice_data$`Unit of measurement`),]
justice_data <- justice_data[!grepl('Total',justice_data$Sex),]
justice_data <- justice_data[!grepl('Total',justice_data$Age),]

View(corruption_economic_crime)

#crime rate data
csvData <- read_csv("Uni/SEM 4/Intro to DS/Assignment/group/CRIME ANALYZER/Suraya/csvData.csv")