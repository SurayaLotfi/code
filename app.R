

ui <- fluidPage(

#header
header<- dashboardHeader(title = "GLOBAL CRIME ANALYZER",
                         titleWidth = 300,disable = FALSE),


#sidebar

sidebar <-dashboardSidebar(sidebarMenu(menuItem( 
  text = "Home/About", # text = will be displayed on page
  tabName = "Home", #tabName = the variable for server to process whenever it is called/use
  icon = (icon("lightbulb"))
),
menuItem(
  text = "Crime Rates",
  tabName = "crimerates",
  icon=(icon("percent"))
),
menuItem(
  text = "Types of Crimes",
  tabName = "types",
  icon = (icon("table"))
),
menuItem(
  text = "Criminal Justice",
  tabName = "justice",
  icon = (icon("globe"))
),
menuItem(
  text = "Prison Population Rate",
  tabName = "pp",
  icon = (icon("chart-line"))
),
menuItem(
  text = "Documentation",
  tabName = "documentation",
  icon = (icon("file"))
),
menuItem(
  text = "About Us",
  tabName = "aboutus",
  icon = (icon("angellist"))
)
)
),
#body                         
body <- dashboardBody(
  tags$style(HTML(".main-sidebar { font-size: 18px; }")),
  tabItems(
    tabItem(
      tabName= "Home",
      
      fluidRow(
        box(
          title = "HOME/ABOUT US",
          width = 12,
          
          #starting of navbar
          br(),
          fluidRow( 
            box(
              
              #strong(h2("Investigate.rs", align = "center")),
              strong(p("INVESTIGATE.rs", style = "font-size: 40px;", align = "center")),
              p("Our group name is INVESTIGATE.rs, we chose this name because we provide an interactive platform which provides data of crimes around the world 
                       while .rs is stand for r shiny. If you read INVESTIGATE.rs in a faster way, it will sound like (Investigators).", style = "font-size: 20px;", align = "center"),
              width = 12,
              height = "200px",
              background = 'maroon'
            )),
          br(),
          box(
            strong(p("Welcome, visitor!", style = "font-size: 40px;", align = "center")),
            p("Let's gain knowledge on statistics of crime all around the world!", style = "font-size: 26px;", align = "center"),
            p("Crimes analyzer has been created with the goal to help people worldwide fully acknowledge the crimes 
          that have been committed around the world. Crimes are defined as an illegal act for which someone can be punishable by law. 
          Criminologists commonly group crimes into several major categories which are violent crime, property crime, white-collar crime, 
          organized crime, and consensual or victimless crime. 
          ", style = "font-size: 18px;", align = "left"),
            br(),
            p("Below are the examples for each type of crimes which may help you to get a better understanding of this issues.", style = "font-size: 20px;"),
            br(), 
            p("- Violent crime: Homicide, aggravated and simple assault, rape and sexual assault, and robbery", style = "font-size: 20px;", align = "left"),
            p("- Property crime: Burglary, larceny, motor vehicle theft, and arson", style = "font-size: 18px;", align = "left"),
            p("- White-collar crime: Securities fraud, embezzlement, corporate fraud, and money laundering", style = "font-size: 20px;", align = "left"),
            p("- Organized crime: Drug trafficking, smuggling of migrants, human trafficking and money-laundering", style = "font-size: 20px;", align = "left"),
            p("- Consensual/Victimless crime: Prostitution, assisted suicide, trespassing, recreational drug use", style = "font-size: 20px;", align = "left"),
            br(),
            p("We really hope that everyone who crosses this website may instill awareness on how dangerous the world is and what 
            to do to avoid from involving crimes because they will suffer your life as well as people that you love the most. ", style = "font-size: 20px;", align = "left"),
            width = "100%",
            height = "630px",
            background = 'purple',
            br(),
            br()),
          
          br(),
          box(
            strong(p("How do we come to Crime Analyzer Project?", style = "font-size: 40px;", align = "center")),
            br(),
            strong(p("Problem Statement: ", style = "font-size: 24px;")),
            p("- In 2022, Venezuela is ranked as the highest crime rates in the world with 83.76 percent per 100k people. Due to this, police in U.S department indicate it is unsafe to travel to the country.", style = "font-size: 20px;"),
            p("- In Malaysia, In 2020, crime index decreased by 21.4 per cent to 65,623 cases as compared to 83,456 cases in 2019. Violent and property crime also decreased by 19.5 per cent and 21.8 per cent respectively to 13,279 cases and 52,344 cases in 2020."
              , style = "font-size: 20px;"),
            p("- Nowadays, crimes are slightly on the rise. The offenders, the abetted, and the victims are coming from various type of ages, adults, youngsters and even children which resulted in anxious and worries especially among parents and peers.", style = "font-size: 20px;"),
            p("- As a result, it reflects the country's background that will be presented to the world.", style = "font-size: 20px;"),
            br(),
            strong(p("Solution:", style = "font-size: 24px;")),
            p("Crime is a complex matter that has posed a major threat to the entire world. Crimes not only burden people around but may also drag down 
            the reputation of the country if it is involved on the international level. Due to that, we are passionate to develop a website which may help 
            people to visualize the data in an interesting way, thus helping people to spread awareness to the whole world.", style = "font-size: 20px;"),
            br(),
            strong(p("The questions:", style = "font-size: 24px;")),
            p("* Which region has the highest crime category each year?", style = "font-size: 20px;"),
            p("* How the number of prisoners are related with the number of the crime reported? ", style = "font-size: 20px;"),
            p("* How likely juvenile and adult will commit crime?",  style = "font-size: 20px;"),
            p("* What year is the highest crime happened in the country?", style = "font-size: 20px;"), 
            p("* How rates of each crime for each country?",  style = "font-size: 20px;"),
            br(),
            strong(p("Objective:", style = "font-size: 24px;")),
            p("1. Spread awareness about crimes around the world.", style = "font-size: 20px;"),
            p("2. Guidance of people who need reference in safety aspect.", style = "font-size: 20px;"),
            p("3. Acknowledge the common crimes happen to the public.",  style = "font-size: 20px;"),
            br(),
            strong(p("Dataset used: ", style = "font-size: 24px;")),
            p("We use datasets from the official website of the United Nations Office on Drugs and Crime (UNODC).", style = "font-size: 20px;"),
            
            width = "100%",
            height = "1200px",
            background = 'green')
        )
      )
    ),
    #moving on to the next one
    
    
    tabItem(
      tabName = "crimerates",
      
      fluidRow(
        box(
          title = "GLOBAL CRIMES RATE",
          width = 12,
          
          h2("Crime Rate of Countries in 2022", align = 'center'),
          
          mainPanel(
            plotlyOutput("linePlot"),
            
            
            
            br(),
            br(),
            h4('The chart above explains the Crime Index of Countries in 2022 and their ranking.', align = 'center'), 
            br(),
            h3(strong('What is the Crime Index?'), align = 'center'),
            p(span("Crime Index", style = "color:teal"),' is an estimation of overall level of crime in a given city or a country. We consider 
            crime levels lower than 20 as very low, crime levels between 20 and 40 as being low, crime levels 
            between 40 and 60 as being moderate, crime levels between 60 and 80 as being high and finally crime 
            levels higher than 80 as being very high.', align = 'justify',style = "font-size:20px"),
            br(),
            p("Overall crime rate is calculated by dividing the total number of reported crimes of any kind by the 
            total population, then multiplying the result by 100,000 (because crime rate is typically reported as 
            X number of crimes per 100,000 people). Crime rates vary greatly from country to country and are 
            influenced by many factors. For example, high poverty levels and unemployment tend to inflate a 
            country's crime rate. Conversely, strict police enforcement and severe sentences tend to reduce crime 
            rates. There is also a strong correlation between age and crime, with most crimes, especially violent 
            crimes, being committed by those ages 20-30 years old.", align = 'justify',style = "font-size:20px")
            
            
          ),
          
          sidebarPanel(
            # choose the type of chart
            selectInput(inputId ="channel1", label = "How do you want to see the chart?",
                        choices = c("By Country (Alphabetical Order)"="cot",
                                    "By Ranking"="ran"))
          ),
          
          valueBoxOutput('highest', width = 3),
          valueBoxOutput('safest', width = 3),
          valueBoxOutput('average', width = 3)
        )
      )
    ),
    
    tabItem(
      tabName = "types",
      
      box(
        title = "TYPES OF CRIMES",
        width = 12,
        radioButtons(
          inputId = "crimes",
          label = "Select the type of crime",
          choices = c("Intentional Homicide","Violent and sexual",
                      "Trafficking","Corruption and Economic"),
          selected = "Intentional Homicide",
          inline = F
        )
      ),
      
      
      
      fluidRow(
        column(
          width = 12,
          plotlyOutput(outputId = "toc") %>% withSpinner(color="red")
        )
      ),
      
      fluidRow(
        box(
          title = "WORLDWIDE OVERVIEW",
          width = 12,
          
          valueBoxOutput(outputId = "total_value",width = 4),
          
          valueBoxOutput(outputId =  "maxcountry", width = 4),
          
          valueBoxOutput(outputId = "mincountry", width = 4)
          
        )
      )
      
    ),
    
    
    
    tabItem(
      tabName = "justice",
      
      fluidRow(
        box(
          title = "CRIMINAL JUSTICE",
          width = 12,
          
          sliderInput(inputId = "year",
                      label = "Frequency of Justice being made",
                      min = 2003, max =2020,value =2003),
          h6(""),
          h3("Above is a slider, pick a year!"),
          
          
          
          h3("Meanwhile below is where you pick a continent"),
          
          selectInput(inputId = "continent", label = "Choose Continent",
                      choices = unique(data_cts_access_and_functioning_of_justice$Region)
                      
          )
        ),
        
        actionButton(inputId = "show",label="Show Data"),
        
        box(
          title = "Timeline of total number of criminal justice has been made by continent",
          width = 12,
          height = "1000px",
          plotlyOutput("justicetimeline")
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
          title = "Data",
          width = 4,
          height = "120px",

          dataTableOutput("pp_data")


        )


      )
    ),
    
    
    tabItem(
      tabName = "documentation",
      
      fluidRow(
        box(
          title = "DOCUMENTATION", style = "font-size: 26px;",
          width = 12,
          box(
            strong(p("Home/About", style = "font-size: 24px;")),
            p("This section introduces our group name, lists all of the major types of crime as well as
            the reason we come up to this Crime Analyzer Project.", style = "font-size: 20px;"),
            width = 12,
            height = "150px",
            background = 'yellow'
          ),
          box(
            strong(p("Crime Rates", style = "font-size: 24px;")),
            p("This section allows the stakeholder to see the crime rates by choosing one user input which are by country or ranking.", style = "font-size: 20px;"),
            br(),
            width = 12,
            height = "150px",
            background = 'red'
          ),
          box(
            strong(p("Types of Crime", style = "font-size: 24px;")),
            p("This section shows the total number of victims for the crimes selected such as intentional homecide, violent and sexual, also corruption and economic.", style = "font-size: 20px;"),
            br(),
            width = 12,
            height = "150px",
            background = 'yellow'
          ),
          box(
            strong(p("Global Criminal Justice", style = "font-size: 24px;")),
            p("This section will show the timeline of total number for criminal justice that has been involved by continent.", style = "font-size: 20px;"),
            br(),
            width = 12,
            height = "120px",
            background = 'red'
          ),
          box(
            strong(p("Prison Population Rate", style = "font-size: 24px;")),
            p("This section will show the prison population rate by each country by using an interactive globe!", style = "font-size: 20px;"),
            br(),
            width = 12,
            height = "120px",
            background = 'yellow'
          ),
          box(
            strong(p("About Us", style = "font-size: 24px;")),
            p("Let's meet our team!", style = "font-size: 20px;"),
            p("We are a group of four students who further study in Computer Science, 
            major in Information Systems and expected to graduate on 2024.", style = "font-size: 20px;"),
            p("Wish us luck to our future endeavours!", style = "font-size: 20px;"),
            width = 12,
            height = "200px",
            background = 'light-blue'
          )
          
        )
      )
    ),
    
    tabItem(
      tabName = "aboutus",
      
      fluidRow(
        box(
          title = "ABOUT US",
          width = 12,
          strong(p("Group Members", align = "center", style = "font-size: 30px;")),
          #HTML(''),
          
          h3("We are a group of four students who study in Computer Science, 
            major in Information Systems and expected to graduate on 2024."),
          h3("Wan Suraya", align = "center"),
          h3("Amrin Hafiz", align = "center"),
          h3("Nurul Filzah", align = "center"),
          h3("Syakir Nu'aim", align = "center")
        )
      )
    )
    
  )
)
)

dashboardPage(header, sidebar, body,skin = "green")


############################################################ SERVER #######################################################

server <- function(input,output){
  
  #crimerates
  output$linePlot <- renderPlotly({
    if(input$channel1 == "cot"){
      
      
      # line chart
      plot_ly(
        data = csvData,
        x = ~crimeIndex,
        y = ~country,
        text = ~ranking,
        hovertemplate = paste('%{y}\n',
                              '<i>Ranking</i>: %{text}\n',
                              'Crime Index: %{x}',
                              '<extra></extra>'),
        showlegend = FALSE,
        type = "scatter",
        mode = "lines+markers",
        
        line = list(width = 1,
                    dash = 'dot',
                    color = 'green')) %>%
        layout(xaxis = list(title = 'Crime Index'),
               yaxis = list(title = 'Country')) 
      
      
    } else{# ranking
      
      plot_ly(
        data = csvData,
        x = ~crimeIndex,
        y = ~reorder(country,crimeIndex),
        text = ~ranking,
        hovertemplate = paste('%{y}\n',
                              '<i>Ranking</i>: %{text}\n',
                              'Crime Index: %{x}',
                              '<extra></extra>'),
        showlegend = FALSE,
        type = "scatter",
        mode = "lines+markers",
        line = list(width = 1,
                    dash = 'dot',
                    color = 'green')) %>%
        layout(xaxis = list(title = 'Crime Index'),
               yaxis = list(title = 'Country')) 
      
    }
  }) 
  output$highest <- renderValueBox({
    valueBox(value = '83.76',
             subtitle = 'VENEZUELA RANKED THE FIRST WITH THE HIGHEST CRIME INDEX ',
             icon = icon("exclamation-triangle"),
             color = 'light-blue',
             width = 3)
  })
  output$safest <- renderValueBox({
    valueBox(value = '12.13',
             subtitle = 'QATAR IS THE SAFEST COUNTRY WITH THE LOWEST CRIME INDEX ',
             icon = icon("shield-alt"),
             color = 'light-blue',
             width = 3)
  })
  output$average <- renderValueBox({
    valueBox(value = '44.74',
             subtitle = 'THE AVERAGE CRIME INDEX AMONG ALL COUNTRIES ',
             icon = icon("table"),
             color = 'light-blue',
             width = 3)
  })
  
  
  #typeOfCrime------------------------------------------------------
  
  output$total_value <- renderValueBox({
    if(input$crimes == "Intentional Homicide"){
      total_val <- sum(data_cts_intentional_homicide$VALUE)
      
      valueBox(
        value = comma(total_val),
        subtitle = "Number of Victims Recorded",
        color = "black",
        icon = icon("exclamation-triangle"),
        width=12
      )
      
    }else if(input$crimes == "Violent and sexual"){
      total_val <- sum(data_cts_violent_and_sexual_crime$VALUE) 
      
      valueBox(
        value = comma(total_val),
        subtitle = "Number of Victims Recorded",
        color = "black",
        width = 12
      )
    }else if(input$crimes == "Corruption and Economic"){
      total_val <- sum(corruption_economic_crime$VALUE)
      
      valueBox(
        value = comma(total_val),
        subtitle= "Number of Victims Recorded",
        color = "black",
        width =12
      )
    }
  })
  #max country-----------------------------------------------------------------------------------------
  
  output$maxcountry <- renderValueBox({
    if(input$crimes == "Intentional Homicide"){
      max_country <- data_cts_intentional_homicide %>%
        group_by(Country)%>%summarise(freq = sum(VALUE))%>%
        arrange(desc(freq))%>%
        head(1)%>%
        pull(Country)
      
      valueBox(
        value = max_country,
        subtitle = "Country with the highest numbers of victims for Intentional Homicide",
        color = "black",
        width = 12
      )
    }else if(input$crimes == "Violent and sexual"){
      max_country <- data_cts_violent_and_sexual_crime%>%
        group_by(Country)%>% summarise(freq=sum(VALUE))%>%
        arrange(desc(freq))%>%
        head(1)%>%
        pull(Country)
      
      
      valueBox(
        value = max_country,
        subtitle = "Country with the highest numbers of victims for Violent and Sexual",
        color = "black",
        width = 12
      )
    }else if(input$crimes == "Corruption and Economic"){
      max_country <- corruption_economic_crime %>%
        group_by(Country)%>%summarise(freq=sum(VALUE))%>%
        arrange(desc(freq))%>%
        head(1)%>%
        pull(Country)
      
      valueBox(
        value = max_country,
        subtitle = "Country with the highest numbers of victims for Corruption and Economic",
        color = "black",
        width = 12
      )
    }
    
  })
  
  #minimum country----------------------------------------------------------------------------
  
  output$mincountry <- renderValueBox({
    if(input$crimes == "Intentional Homicide"){
      min_country<-data_cts_intentional_homicide %>% group_by(Country)%>%
        summarise(freq=sum(VALUE))%>% arrange(freq)%>%head(1)%>%
        pull(Country)
      
      valueBox(
        value = min_country,
        subtitle= "Country with the lowest number of victims for Intentional Homicide",
        color = "black",
        width = 12
      )
    }else if(input$crimes == "Violent and sexual"){
      min_country<-data_cts_violent_and_sexual_crime%>% group_by(Country)%>%
        summarise(freq=sum(VALUE))%>% arrange(freq)%>%head(1)%>%
        pull(Country)
      
      valueBox(
        value = min_country,
        subtitle= "Country with the lowest number of victims for Intentional Homicide",
        color = "black",
        width = 12
      )
    }else if(input$crimes == "Corruption and Economic"){
      min_country<-corruption_economic_crime%>% group_by(Country)%>%
        summarise(freq=sum(VALUE))%>% arrange(freq)%>%head(1)%>%
        pull(Country)
      
      valueBox(
        value = min_country,
        subtitle= "Country with the lowest number of victims for Intentional Homicide",
        color = "black",
        width = 12
      )
    }
  })
  
  ##lineplot for type of crimes------------------------------------------------------------
  
  output$toc <- renderPlotly({
    if(input$crimes == "Intentional Homicide" ){
      ih_lineplot <- ggplot(data = ih_line_data,
                            aes(x = Year, y = rate,
                                group = Region, color = Region,
                                text = label))+
        geom_line(lwd = 0.70)+
        geom_point(size = 1)+
        scale_x_continuous(breaks = seq(1990, 2020, by =5),
                           limits = c(1990,2020))+
        labs(x = "Year", y = "Total Number of Victims per 1,000",
             title = "<b>Total Number of Victimes Over the Years</b>",
             color = "")+
        theme_minimal()
      ggplotly(ih_lineplot, tooltip = "text")
      
      
    }else if(input$crimes == "Violent and sexual"){
      vs_lineplot <- ggplot(data = vs_line_data,
                            aes(x = Year, y = rate,
                                group = Region, color = Region,
                                text = label))+
        geom_line(lwd = 0.70)+
        geom_point(size = 1)+
        scale_x_continuous(breaks = seq(2003, 2020, by =5),
                           limits = c(2003,2020))+
        labs(x = "Year", y = "Total Number of Victims per 1,000",
             title = "<b>Total Number of Victimes Over the Years</b>",
             color = "")+
        theme_minimal()
      ggplotly(vs_lineplot, tooltip = "text")
      
    }else if(input$crimes == "Corruption and Economic"){
      ce_lineplot <- ggplot(data = vs_line_data,
                            aes(x = Year, y = rate,
                                group = Region, color = Region,
                                text = label))+
        geom_line(lwd = 0.70)+
        geom_point(size = 1)+
        scale_x_continuous(breaks = seq(2003, 2020, by =5),
                           limits = c(2003,2020))+
        labs(x = "Year", y = "Total Number of Victims pr 1,000",
             title = "<b>Total Number of Victims Over the Years</b>",
             color = "")+
        theme_minimal()
      ggplotly(ce_lineplot, tooltip = "text")
    }
  })
  
  ###criminal justice
  reactive_output <- eventReactive(eventExpr = input$show, 
                                   filter(data_cts_access_and_functioning_of_justice,
                                          Year == input$year,
                                          Region == input$continent)
  )
  
  
  
  output$justicetimeline <- renderPlotly({
    
    justice_timeline <- reactive_output() %>% group_by(Indicator, Region)%>%
      summarise(freq = sum(VALUE))%>%
      mutate(label = glue(
        "Indicator: {Indicator}
                Continent: {Region}
                Number of justice: {comma(freq)}"
      )
      ) %>% 
      ggplot(aes(
        x=Indicator,
        y=freq, 
        text = label,
        group = 1
      ))+
      geom_line(aes(col=Region))+geom_point()+geom_area(fill="#02d6d9",alpha=0.4)+
      scale_y_continuous(labels=comma)+
      labs(title = "",
           x="Year",
           Y = "Number of Justice")+
      theme_bw()
    
    ggplotly(justice_timeline, tooltip = "text") %>% layout(showlegend =T)
  })
  
  
  
  
  output$barPlot <- renderPlot({
    #plotting multiple bar graph
    if(input$indicator == "pc"){ 
      justice <- ggplot(justice_data,aes(Category, y=VALUE,
                                         fill = Sex))+
        ggtitle("Top categories with picked indicator justice")+
        xlab("Category")+
        ylab("Total Value")+
        geom_col(position = "dodge",stat='identity')+
        geom_text(aes(label = VALUE), vjust = 0.3, color = "black",
                  position = position_dodge(0.9))
      justice + coord_flip()
    }
  })
  
  ###prison population
  
  output$prison_population_map <- renderPlotly({
    pp_map <- prison_population_rate
    l <- list(color = toRGB("grey"), width = 0.5)
    g<- list( resolution=200,
              showcoastlines=T, coastlinecolor="RebeccaPurple",
              showland=F, landcolor="grey85",
              showocean=T, oceancolor="white",
              showlakes=T, lakecolor="Lightblue",
              showrivers=T, rivercolor="Lightblue",
              projection = list(type = 'natural earth', scale = 1)
    );5
    plot_ly(pp_map, z = pp_map$rate, text = pp_map$region, locations = pp_map$Code,
            type = 'choropleth', color = pp_map$rate, colors = "YlGnBu", marker = list(line = l ),
            colorbar=list(title = "Prison Population Rate")) %>% layout(geo=g)
  })
  
  output$pp_data <- renderDataTable(
    prison_population_rate <- rename(prison_population_rate, Region=region)
    
  )
  
  
} #total countries with the amount of __ range


shinyApp(ui=ui, server=server)