# Load R packages
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
library(highchatter)
install.packages("highchatter")
library(shinyjs)
library(tmap)
library(tmaptools)
library(mapdeck)
library(maps)


#header
header<- dashboardHeader(title = "GLOBAL CRIME ANALYZER",
                         titleWidth = 400,disable = FALSE)


#sidebar

sidebar <-dashboardSidebar(sidebarMenu(menuItem( 
  text = "HOME/ABOUT", # text = will be displayed on page
  tabName = "Home" #tabName = the variable for server to process whenever it is called/use
),
menuItem(
  text = "Crime Rates",
  tabName = "crimerates"
),
menuItem(
  text = "Types of Crime",
  tabName = "types"
),
menuItem(
  text = "Criminal Justice",
  tabName = "justice"
),
menuItem(
  text = "Prison Population Rate",
  tabName = "pp"
 ),
menuItem(
  text = "Documentation",
  tabName = "documentation"
 ),
menuItem(
  text = "About Us",
  tabNamae = "aboutus"
 )
)
)
#body                         
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName= "Home",
      
      fluidRow(
        box(
          title = "HOME/ABOUT US",
          width = 12
        )
      )
    ), #moving on to the next one
    
    
    tabItem(
      tabName = "crimerates",
      
      fluidRow(
        box(
          title = "Global Crime Rates",
          width = 12
        )
      )
    ),
    
    
    tabItem(
      tabName = "types",
      
      fluidRow(
        box(
          title = "Types of Crimes",
          width = 12
        )
      )
    ),
    
    
    tabItem(
      tabName = "justice",
      
      fluidRow(
        box(
          title = "Criminal Justice",
          width = 12
        )
      )
    ),
    tabItem(
      tabName = "pp",
      
      fluidRow(
        
        box(
        title = "PRISON POPULATION MAP",
        width = 8,
        
        height = "920px",
        plotlyOutput(outputId = "prison_population_map",width = "100%",height=
                       "700px")
        ),
        
        box(
          
        )

       
      )
     ),

    
    tabItem(
      tabName = "documentation",
      
      fluidRow(
        box(
          title = "DOCUMENTATION",
          width = 12
        )
      )
    ),
    
    tabItem(
      tabName = "aboutus",
      
      fluidRow(
        box(
          title = "ABOUT US",
          width = 12
        )
      )
    )
    
  )
)



dashboardPage(header, sidebar, body,skin = "black")

dashboardPage(header, sidebar, body,skin = "black")
