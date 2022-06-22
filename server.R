function(input,output){
  
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
