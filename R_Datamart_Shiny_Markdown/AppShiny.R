if(!require("ggmap"))install.packages("ggmap")
library(ggmap)
library(shinyWidgets)

register_google(key = "AIzaSyC8tRJl2yBKsQ3fGfzPGArP5m64ETU7ZGo") 


load(file = 'basetablegambling.RData')

library(shiny)
library(shinydashboard)


ui <- dashboardPage(
  dashboardHeader(title = "Gambling Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Gambling-Weekday Statistics", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName="dashboard",
              h2(
                
                fluidRow(
                  
                  box(title = "Select Inputs",
                      width = 6,selectizeGroupUI(
                        id = "my-filters",
                        inline = FALSE,
                        params = list(
                          
                          var_one = list(inputId = "Country_Name", title = "Country Names", placeholder = 'select'),
                          var_two = list(inputId = "Gender", title = "Gender", placeholder = 'select'),
                          var_three = list(inputId = "Application_Description", title = "Applications", placeholder = 'select'),
                          var_four = list(inputId = "Age_Group", title = "Age Groups", placeholder = 'select'),
                          var_five = list(inputId = "Loyalty_Status", title = "Loyalty Status", placeholder = 'select')
                        )
                      )
                  ),
                  
                  box(title = "Geographical Footprint of Customers",plotOutput("map")),
                  box(plotOutput("plot")),
                  box(title = "Total Customers Stakes per Game-Type",plotOutput("bar2")),
                  box(title = "Total Customers Winnings per Game-Type",plotOutput("bar")),
                  box(title = "Loyal Gamblers",plotOutput("bar7"))
                  
                )
              )
      
              
      ),
      tabItem(tabName="widgets",
              h2(
                fluidRow(
                  
                  box(title = "Gambling Frequnecy of Casino in days",plotOutput("bar3")),
                  box(title = "Gambling Frequnecy of Sports in days",plotOutput("bar4")),
                  box(title = "Gambling Frequnecy of Games in days",plotOutput("bar5")),
                  box(title = "Gambling Frequnecy of Poker in days",plotOutput("bar6"))
                )
              )
      )
    )
  )
)





server <- function(input, output) {
  
  
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = Basetable_Gambling,
    vars = c("Country_Name", "Gender", "Application_Description", "Age_Group","Loyalty_Status")
  )
  
  # output$table <- renderTable({
  #   # head(test)
  #   head(res_mod())
  #   #res_mod()
  # })
  
  
  
  output$map <- renderPlot({
    ggmap(get_map(location='test', maptype = "terrain",source='google', zoom =1 )) + geom_point(aes(x=Longitude, y=Latitude), data = res_mod(),color="blue", size=3)
  })
  
  output$bar<- renderPlot({
    
    Profit_Ca <- round(sum(res_mod()$Profit_Ca, na.rm = TRUE),2)
    Profit_Fo <- round(sum(res_mod()$Profit_FO, na.rm = TRUE),2)
    Profit_LA <- round(sum(res_mod()$Profit_LA, na.rm = TRUE),2)
    Profit_Ga <- round(sum(res_mod()$Profit_Ga, na.rm = TRUE),2)
    Profit_Po <- round(sum(res_mod()$Profit_Po, na.rm = TRUE),2)
    
    Profit <- c(Profit_Ca, Profit_Fo, Profit_LA, Profit_Ga, Profit_Po)
    
    names(Profit) <- c("Casino", "Fixed Odds(Sports)", "Live Action(Sports)","Games","Poker")
    Gain<-as.data.frame(Profit)
    ggplot(data=Gain, aes(x=c("Casino", "Fixed Odds(Sports)", "Live Action(Sports)","Games","Poker"),y=Profit))+geom_bar(stat="identity")+scale_y_continuous(labels=scales::comma) 
    
    
  })
  
  output$bar2<- renderPlot({
    
    Stakes_Ca <- round(sum(res_mod()$Total_Stakes_Ca, na.rm = TRUE),2)
    Stakes_Sp <- round(sum(res_mod()$Total_Stakes_Sp, na.rm = TRUE),2)
    Stakes_Ga <- round(sum(res_mod()$Total_Stakes_Ga, na.rm = TRUE),2)
    Stakes_Po <- round(sum(res_mod()$Total_Amount_Bought, na.rm = TRUE),2)
    
    
    Stakes <- c(Stakes_Ca, Stakes_Sp, Stakes_Ga, Stakes_Po)
    
    names(Stakes) <- c("Casino", "Sports","Games","Poker")
    Stakes<-as.data.frame(Stakes)
    
    ggplot(data=Stakes, aes(x=c("Casino", "Sports","Games","Poker"),y=Stakes))+geom_bar(stat="identity")+scale_y_continuous(labels=scales::comma) 
    
  })
  
  
  output$bar3<- renderPlot({
    
    Casino = res_mod() %>% 
      dplyr::group_by (MostPlayed_Day_Ca)%>% 
      dplyr::summarise(
        Gamblers=length(unique(UserID)))
    
    Casino = Casino[1:7,]
    
    
    ggplot(data = Casino, aes(x=`MostPlayed_Day_Ca`, y= Gamblers)) + 
      geom_bar(stat = "identity") +
      coord_flip() + labs(title="Number of Gamblers per Day", x="Days_Played", y="Number of Gamblers") 
    
  })
  
  
  output$bar4<- renderPlot({
    
    Sports = res_mod() %>% 
      dplyr::group_by (MostPlayed_Day_Sp)%>% 
      dplyr::summarise(
        Gamblers=length(unique(UserID)))
    
    Sports = Sports[1:7,]
    
    
    ggplot(data = Sports, aes(x=`MostPlayed_Day_Sp`, y= Gamblers)) + 
      geom_bar(stat = "identity") +
      coord_flip() + labs(title="Number of Gamblers per Day", x="Days_Played", y="Number of Gamblers") 
    
  })
  
  
  output$bar5<- renderPlot({
    
    Games = res_mod() %>% 
      dplyr::group_by (MostPlayed_Day_Ga)%>% 
      dplyr::summarise(
        Gamblers=length(unique(UserID)))
    
    Games = Games[1:7,]
    
    
    ggplot(data = Games, aes(x=`MostPlayed_Day_Ga`, y= Gamblers)) + 
      geom_bar(stat = "identity") +
      coord_flip() + labs(title="Number of Gamblers per Day", x="Days_Played", y="Number of Gamblers") 
    
  })
  
  output$bar6<- renderPlot({
    
    Poker = res_mod() %>% 
      dplyr::group_by (Most_Played_Day_Po)%>% 
      dplyr::summarise(
        Gamblers=length(unique(UserID)))
    
    Poker = Poker[1:7,]
    
    
    ggplot(data = Poker, aes(x=`Most_Played_Day_Po`, y= Gamblers)) + 
      geom_bar(stat = "identity") +
      coord_flip() + labs(title="Number of Gamblers per Day", x="Days_Played", y="Number of Gamblers") 
    
  })
  
  
  
  
  output$bar7<- renderPlot({
    
    LoyaltyRmark = res_mod() %>% 
      dplyr::group_by (Loyalty_Status)%>% 
      dplyr::summarise(
        Gamblers=length(unique(UserID)))
    
    
    LoyaltyRmark = LoyaltyRmark[order(-LoyaltyRmark$Gamblers),]
    LoyaltyRmark = LoyaltyRmark[1:3,]
    LoyaltyRmark = LoyaltyRmark[order(-LoyaltyRmark$Gamblers),]
    
    
    
    ggplot(data = LoyaltyRmark, aes(x=`Loyalty_Status`, y= Gamblers)) + 
      geom_bar(stat = "identity") +
      coord_flip() + labs(title="Number of Gamblers per Loyalty", x="Loyalty_Status", y="Number of Gamblers")
    
  })
  
  
  output$plot <- renderPlot({
    # Subset the gapminder dataset by the chosen continents
    
    hist(res_mod()$AGE,
         main ="Gamblers Count by Age", 
         ylab = "Number of gamblers",
         xlab = "Age",
         breaks =  6)
  })
  
  options = list(height = 500)
}

shinyApp(ui = ui, server = server)





