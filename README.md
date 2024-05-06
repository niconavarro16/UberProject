# UberProject
## Data Cleaning
- The first step involved loading the raw dataset into the R environment. The dataset was provided in CSV format.
- Date.Time column was converted from character format to datetime format to enable temporal analysis.
- Some categorical variables such as Day_of_Week, Hour, Month, Day, and Week were extracted from the 'Date.Time' column for further analysis.
- After cleaning and preprocessing, a final cleaned dataset was created.


## 1. UberProject Code 
The steps of the code are explained in the code with my own comments.

```
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(knitr)
library(shiny)
library(leaflet)
library(leaflet.extras)


setwd("C:/Users/nicol/OneDrive/Augustana College/3 year/Spring term/DATA 332/Data/UberProject")
csv_files <- list.files(pattern = "uber-raw-data-(apr|aug|jul|jun|may|sep)14.csv") 

#Create an empty data frame to store the combined data
combined_data <- data.frame()

#Loop through each CSV file and combine its data with the existing combined data
for (file in csv_files) {
  # Read the CSV file
  data <- read.csv(file)
  
  combined_data <- rbind(combined_data, data)
}

#Adequate time
combined_data$Date.Time <- as.POSIXct(combined_data$Date.Time, format= "%m/%d/%Y %H:%M:%S")

#Writing the combined data to a new CSV file
write.csv(combined_data, "combined_data.csv", row.names = FALSE)




###PIVOT TABLE TO DISPLAY TRIPS BY HOUR### AND ###TRIPS EVERY HOUR###
#Extract the hour
combined_data$Hour <- hour(combined_data$Date.Time)

#Pivot table Hour
pivot_table_trips_by_hour <- combined_data %>%
  group_by(Hour) %>%
  summarise(Trips = n())

#Trips by the Hour of the day
combined_data$hour <- hour(combined_data$Date.Time)

#Group data by hour and count trips
hourly_trips <- combined_data %>%
  group_by(Hour) %>%
  summarise(trips = n())

#Number of Trips by Hour of Day graph
ggplot(hourly_trips, aes(x = Hour, y = trips)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Number of Trips by Hour of Day",
       x = "Hour of Day",
       y = "Number of Trips") +
  theme_minimal()




###TRIPS BY HOUR AND MONTH###
#Getting the month names and adding the column
combined_data$Month <- month(combined_data$Date.Time, label = TRUE)  

hourly_monthly_trips <- combined_data %>%
  group_by(Hour, Month) %>%
  summarise(trips = n(), .groups = 'drop')


#Monthly trips by hour of day and month graph 
ggplot(hourly_monthly_trips, aes(x = Hour, y = trips, fill = Month)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Trips by Hour and Month",
       x = "Hour of Day",
       y = "Number of Trips",
       fill = "Month") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") 




###Plot data by trips taken during every day of the month###
#Adding the day to the combined data
combined_data$day <- day(combined_data$Date.Time)

#graph
ggplot(daily_trips, aes(x = day, y = trips)) +
  geom_line(group = 1, color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Number of Trips by Day of the Month",
       x = "Day of the Month",
       y = "Number of Trips") +
  theme_minimal()




###I should see a table that shows Trips Every Day (Max 31 days in a month so I should see total trips taken each day)###
#Pivot table for daily trips
daily_trips <- combined_data %>%
  group_by(day) %>%
  summarise(trips = n())




###Chart by Trips by Day and Month (bar chart with each day of the week, x axis as the month). I need a chart that shows number of trips by month###
# Assuming combined_data is already loaded and Date.Time is a POSIXct or Date object
combined_data$Month <- month(combined_data$Date.Time, label = TRUE, abbr = FALSE)
combined_data$Day_of_Week <- wday(combined_data$Date.Time, label = TRUE, abbr = FALSE)#adding day of the week (Monday, Tuesday, ...)

#Pivot table trips by day and month
trips_by_day_and_month <- combined_data %>%
  group_by(Month, Day_of_Week) %>%
  summarise(Trips = n(), .groups = 'drop') %>%
  arrange(Month, Day_of_Week)

#bar chart with each day of the week, x axis as the month
ggplot(trips_by_day_and_month, aes(x = Month, y = Trips, fill = Day_of_Week)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Number of Trips by Day of the Week and Month",
       x = "Month",
       y = "Number of Trips",
       fill = "Day of the Week") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




###Chart Trips by Bases and Month (Base is the X axis and Month is your label)###
#Pivot table for base and month
trips_by_base_and_month <- combined_data %>%
  group_by(Base, Month) %>%
  summarise(Trips = n(), .groups = 'drop') %>%
  arrange(Base, Month)

#bar chart base and month
ggplot(trips_by_base_and_month, aes(x = Base, y = Trips, fill = Month)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Number of Trips by Base and Month",
       x = "Base",
       y = "Number of Trips",
       fill = "Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




###Heat map that displays by hour and day###
#Pivot table for hour and day
trips_by_hour_day <- combined_data %>%
  group_by(Hour, Day_of_Week) %>%
  summarise(Trips = n(), .groups = 'drop')

#Heat map for hour and day
ggplot(trips_by_hour_day, aes(x = Hour, y =  Day_of_Week, fill = Trips)) +
  geom_tile(color = "white") +  
  scale_fill_gradient(low = "lightblue", high = "darkblue") + 
  labs(title = "Trips by Hour and Day of the Week",
       x = "Hour of the Day",
       y = "Day of the Week",
       fill = "Number of Trips") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) 




###Heat map by month and day###
ggplot(trips_by_day_and_month, aes(x = Day_of_Week, y = Month, fill = Trips)) +
  geom_tile(color = "white") +  
  scale_fill_gradient(low = "lightblue", high = "darkblue") + 
  labs(title = "Trips by Month and Day of the Week",
       x = "Day of the Month",
       y = "Month",
       fill = "Number of Trips") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) 




###Heat map by month and week###
#Adding the week to the combined data (starting with 13 because it starts in April!!!!)
combined_data$week <- week(combined_data$Date.Time)

#Pivot table for month and week 
trips_by_month_and_week <- combined_data %>%
  group_by(Month, week) %>%
  summarise(Trips = n(), .groups = 'drop')

#Heatmap by month and week
ggplot(trips_by_month_and_week, aes(x = week, y = Month, fill = Trips)) +
  geom_tile(color = "white") +  
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  
  labs(title = "Trips by Month and Week of the Year",
       x = "Week of the Year",
       y = "Month",
       fill = "Number of Trips") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))




###Heat map Bases and Day of Week###
#Pivot table for Base and Day of the week
trips_by_base_and_dayOfWeek <- combined_data %>%
  group_by(Base, Day_of_Week) %>%
  summarise(Trips = n(), .groups = 'drop')

#Heat map Bases and Day of Week
ggplot(trips_by_base_and_dayOfWeek, aes(x = Base, y = Day_of_Week, fill = Trips)) +
  geom_tile(color = "white") +  # Add borders to tiles
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  
  labs(title = "Trips by Base and Day of the Week",
       x = "Base",
       y = "Day of the Week",
       fill = "Number of Trips") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))




###Leaflet Shiny Geospatial Map###
# A sample of a smaller subset of the data because I can't work with all this data in my computer
small_data <- combined_data %>% sample_n(5000)

#Create the map with the smaller dataset
small_map <- leaflet(small_data) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~Lon, lat = ~Lat,
    popup = ~paste("Time:", Date.Time),
    radius = 5,
    color = '#3388ff',
    fill = TRUE,
    fillOpacity = 0.7
    
  )


#Print the smaller map
print(small_map)




###Building a prediction ride model###
# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(train_data$trips, p = 0.8, list = FALSE)
train_data <- combined_data[train_index, ]
test_data <- combined_data[-train_index, ]

# Fit a random forest model
library(randomForest)
model <- randomForest(train_data[,1] ~ . - Date.Time, data = train_data, ntree = 50, nodesize = 10)

# Evaluate the Model
predictions <- predict(model, newdata = test_data)
mae <- mean(abs(predictions - test_data[,1]))
mse <- mean((predictions - test_data[,1])^2)

# Print evaluation metrics
print(paste("Mean Absolute Error (MAE):", mae))
print(paste("Mean Squared Error (MSE):", mse))

# Plotting actual vs. predicted values
prediction_data <- data.frame(Actual = test_data[,1], Predicted = predictions)
ggplot(prediction_data, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Trips", x = "Actual Trips", y = "Predicted Trips") +
  theme_minimal()

```

## 2. Shiny app code

```
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(shinydashboard)
library(leaflet)


setwd("C:/Users/nicol/OneDrive/Augustana College/3 year/Spring term/DATA 332/Data/UberProject")


#Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Uber Data Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Graphs", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Heatmaps", tabName = "heatmaps", icon = icon("fire")),
      menuItem("Leaflet Map", tabName = "LeafletMap", icon = icon("map-marker"))
      
    )
  ),
  dashboardBody(
    tabItems(
      #Dashboard Tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(width = 6, title = "Trips by Hour", plotOutput("plot_hourly_trips")), #1
                box(width = 6, title = "Trips by Hour and Month", plotOutput("plot_hourly_monthly_trips")) #2
              ),
              fluidRow(
                box(width = 6, title = "Trips during every Day of the Month", plotOutput("plot_daily_trips")),#3
                box(width = 6, title = "Trips by Day of the Week and Month", plotOutput("plot_trips_by_day_and_month")) #4
              ),
              fluidRow(
                box(width = 6, title = "Trips by Base and Month", plotOutput("plot_trips_by_base_and_month")),#5
              ),
              
              fluidRow(
                box(width = 12, #Here the explanation of the graphs
                    HTML("<p><strong>Chart 1: Trips by Hour</strong> - This chart shows the number of Uber trips taken across different hours of the day. The x-axis represents the hour of the day, and the y-axis represents the number of trips. The purple bars indicate the total trips for each hour, showing peak activity periods and quieter times. It's clear that there is a significant increase in trips during the late afternoon and evening hours, suggesting higher demand during these times.</p>
                    <p><strong>Chart 2: Trips by Hour and Month</strong> - This graph provides a view of how trip frequencies vary by hour throughout different months. The x-axis represents the hour of the day, while the y-axis shows the number of trips. Different colors represent different months.The graph highlights the variations in trip patterns, possibly influenced by factors like weather, holidays, or local events occurring in specific months. We can clearly observe how September is the month with the most trips at every hout of the day.</p>
                    <p><strong>Chart 3: Trips during every Day of the Month</strong> - The line graph shows the daily variation in the number of trips throughout a typical month. The x-axis represents the day of the month, and the y-axis shows the number of trips. The line is marked with red points at each day's data point, providing a visual representation of the highs and lows in trip demand across the month. It can be observed that there are more trips at the middle of the month and at the end, every 15 days more or less. Also, we need to take into account that not every month has 31 days and that is why the 31st day has the lowest amount of trips.</p>
                    <p><strong>Chart 4: Trips by Day of the Week and Month</strong> - This bar chart displays the number of trips by day of the week during different months. Each bar's color represents a day of the week and the x-axis shows months. This visualization helps in understanding weekly patterns within each month, highlighting any specific days that consistently have higher trip numbers, which could be useful for planning and resource allocation. By analyzing the graph, I can tell that the days with the most trips are usually Thursday and Friday and that the average ishigher during the summer months. One aspect that surprised me is the amount on trips we can see on a Tuesday in September.</p>
                    <p><strong>Chart 5: Trips by Base and Month</strong> - The chart shows the number of trips organized by Uber base and month. Each base is represented on the x-axis, and different colors indicate the months. This chart is useful for analyzing the performance and demand at different Uber bases over time, showing which bases are busier during specific months, which could indicate seasonal trends or the impact of local factors on demand.</p>")
                )
              )
      ),
      
      #Heatmaps Tab
      tabItem(tabName = "heatmaps",
              fluidRow(
                box(width = 6, title = "Hour and Day Heatmap", plotOutput("heatmap_hour_day")), #1
                box(width = 6, title = "Month and Day Heatmap", plotOutput("heatmap_month_day")) #2
              ),
              fluidRow(
                box(width = 6, title = "Month and Week Heatmap", plotOutput("heatmap_month_week")), #3
                box(width = 6, title = "Bases and Day of the Week Heatmap", plotOutput("heatmap_bases_dayOfWeek")) #4
              ),
              
               fluidRow(
                box(width = 12, #Here the explanation of the heatmaps
                    HTML("<p><strong>Heatmap 1: Trips by Hour and Day</strong> - This heatmap visualizes the frequency of Uber trips across different hours of the day for each day of the week. The color intensity increases with the number of trips, making it easy to identify peak times. For example, late evenings on Friday and Saturday show higher trip frequencies, indicating when Uber is most utilized during the week.</p>
                    <p><strong>Heatmap 2: Trips by Month and Day of the Week</strong> - This heatmap displays the number of trips by each day of the week during different months. By observing the variations in color, we can tell how September is the busiest month for Uber, mostly during the weekends. This could be due to people going out on Fridays and Saturdays. Besides this, Sundays are always the least busy day of the week.</p>
                    <p><strong>Heatmap 3: Trips by Month and Week</strong> - This one shows the number of trips by week of the year across each month. This visualization helps in identifying how weekly trip patterns change over the months. It can be observed a similar pattern as the previous heatmaps where september is the month with the most trips. However, from this heatmap we can see that there are fewer trips in the first week of the month.</p>
                    <p><strong>Heatmap 4: Trips by Base and Day of the Week</strong> - It shows the distribution of trips across different Uber bases for each day of the week. This helps in understanding which bases are more active on certain days, indicating perhaps the areas with higher demand or strategic locations that see more activity on specific days.</p>")
                )
              )
      ),
      
      #Leaflet Map
      tabItem(tabName = "LeafletMap",
              fluidRow(
                box(width = 12, title = "Leaflet Map", leafletOutput("LeafletMap")), #and now the explanation of the leaflet map
                box(width = 12, HTML("<p><strong>About it</strong> - The Leaflet map shows a sample of 5,000 Uber trips from the larger dataset. I used this smaller sample because I couldn't handle all the large dataset with my laptop, so I did a small version of it just taking a random sample of 5000 Uber trips. Each point on the map represents the location of a single Uber trip, with the points colored in blue to enhance visibility against the map's background.<p>
                                     <p><strong>Visual Representation</strong> - The blue points scattered across the map indicate the geographic locations where Uber trips occurred within the New York City area. The density of the points provides a visual indication of areas with higher activity, suggesting popular pickup or dropoff locations.<p>
                                     <p><strong>Interactivity</strong> - By clicking on any of the points, a popup displays the exact time of the trip. This interaction allows users to get more detailed information about individual trips.<p>
                                     <p><strong>")))
      )
      
      
      
      
    )
  )
)





#Define server logic
server <- function(input, output) {
  #Here I am going to put the graphs, heatmaps, and leaflet map in the shiny app
  
  #1Plot: Number of Trips by Hour of Day
  output$plot_hourly_trips <- renderPlot({
    ggplot(hourly_data, aes(x = Hour, y = Trips)) +
      geom_bar(stat = "identity", fill = "purple") +
      labs(title = "Number of Trips by Hour of Day",
           x = "Hour of Day",
           y = "Number of Trips") +
      theme_minimal()
  })
  
  
  #2Plot: Monthly trips by hour of day and month graph
  output$plot_hourly_monthly_trips <- renderPlot({
    ggplot(hourly_monthly_trips, aes(x = Hour, y = trips, fill = Month)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Number of Trips by Hour and Month",
           x = "Hour of Day",
           y = "Number of Trips",
           fill = "Month") +
      theme_minimal() +
      scale_fill_brewer(palette = "Paired") 
  })
  
  #3Plot: trips taken during every day of the month
  output$plot_daily_trips <- renderPlot({
  ggplot(daily_trips, aes(x = day, y = trips)) +
    geom_line(group = 1, color = "blue", size = 1) +
    geom_point(color = "red", size = 2) +
    labs(title = "Number of Trips by Day of the Month",
         x = "Day of the Month",
         y = "Number of Trips") +
    theme_minimal()
  })
  
  #4Plot: trips by Day of the Week and Month
  output$plot_trips_by_day_and_month <- renderPlot({
    ggplot(trips_by_day_and_month, aes(x = Month, y = Trips, fill = Day_of_Week)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Number of Trips by Day of the Week and Month",
           x = "Month",
           y = "Number of Trips",
           fill = "Day of the Week") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #5Plot: trips by Base and Month
  output$plot_trips_by_base_and_month <- renderPlot({
    ggplot(trips_by_base_and_month, aes(x = Base, y = Trips, fill = Month)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Number of Trips by Base and Month",
           x = "Base",
           y = "Number of Trips",
           fill = "Month") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  #1Heatmap: Trips by Hour and Day of the Week
  output$heatmap_hour_day <- renderPlot({
    ggplot(trips_by_hour_day, aes(x = Hour, y = Day_of_Week, fill = Trips)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = "Trips by Hour and Day of the Week",
           x = "Hour of the Day",
           y = "Day of the Week",
           fill = "Number of Trips") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1))
  })
  
  #2Heatmap: Trips by Month and Day of the Week
  output$heatmap_month_day <- renderPlot({
    ggplot(trips_by_day_and_month, aes(x = Day_of_Week, y = Month, fill = Trips)) +
      geom_tile(color = "white") +  
      scale_fill_gradient(low = "lightblue", high = "darkblue") + 
      labs(title = "Trips by Month and Day of the Week",
           x = "Day of the Month",
           y = "Month",
           fill = "Number of Trips") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) 
  })
  
  #3Heatmap: Trips by Month and Week
  output$heatmap_month_week <- renderPlot({
    ggplot(trips_by_month_and_week, aes(x = week, y = Month, fill = Trips)) +
      geom_tile(color = "white") +  
      scale_fill_gradient(low = "lightblue", high = "darkblue") +  
      labs(title = "Trips by Month and Week of the Year",
           x = "Week of the Year",
           y = "Month",
           fill = "Number of Trips") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1))
  })
  
  #4Heatmap: Trips by Bases and Day of the Week 
  output$heatmap_bases_dayOfWeek <- renderPlot({
    ggplot(trips_by_base_and_dayOfWeek, aes(x = Base, y = Day_of_Week, fill = Trips)) +
      geom_tile(color = "white") +  # Add borders to tiles
      scale_fill_gradient(low = "lightblue", high = "darkblue") +  
      labs(title = "Trips by Base and Day of the Week",
           x = "Base",
           y = "Day of the Week",
           fill = "Number of Trips") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1))
  })
  
  
  #Leaflet Map 
  output$LeafletMap <- renderLeaflet({
    small_map <- leaflet(small_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Lon, lat = ~Lat,
        popup = ~paste("Time:", Date.Time),
        radius = 5,
        color = '#3388ff',
        fill = TRUE,
        fillOpacity = 0.7
      )
    
    return(small_map) 
  })
  
}


#Run the application 
shinyApp(ui, server)
```
