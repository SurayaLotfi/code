function(input,output){
  
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
              )
    plot_ly(pp_map, z = pp_map$rate, text = pp_map$region, locations = pp_map$Code,
            type = 'choropleth', color = pp_map$rate, colors = "YlGnBu", marker = list(line = l ), 
            colorbar=list(title = "Prison Population Rate")) %>% layout(geo=g)
  })
}
