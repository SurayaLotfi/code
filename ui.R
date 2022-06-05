
#header
header<- dashboardHeader(title = "SOCIAL MEDIA INFLUENCE",
                         titleWidth = 400,disable = FALSE)


#sidebar

sidebar <-dashboardSidebar(sidebarMenu(menuItem( 
                                         text = "HOME/ABOUT", # text = will be displayed on page
                                         tabName = "Home" #tabName = the variable for server to process whenever it is called/use
                                                ),
                                       menuItem(
                                         text = "Trends",
                                         tabName = "trends"
                                                ),
                                       menuItem(
                                         text = "News",
                                         tabName = "news"
                                               ),
                                       menuItem(
                                          text = "Statistics",
                                          tabName = "stats"
                                       ),
                                       menuItem(
                                          text = "Documentation",
                                          tabName = "documentation"
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
    tabName = "trends",
    
         fluidRow(
           box(
             title = "TRENDS THROUGH THE YEAR/MOST USED APP",
             width = 12
             )
          )
         ),
   
   tabItem(
     tabName = "news",
     
     fluidRow(
       box(
         title = "NEWS ON SOCIAL MEDIA",
         width = 12
       )
     )
   ),
   
   tabItem(
     tabName = "stats",
     
     fluidRow(
       box(
         title = "STATISTICS on INFLUENCE OF SOCIAL MEDIA",
         width = 12
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
   )
    
  )
)

dashboardPage(header, sidebar, body,skin = "black")
