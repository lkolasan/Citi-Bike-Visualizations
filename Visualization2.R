# loading the required packages
library(ggplot2)
library(ggmap)

# Importing data from online:
activity_url <- "https://s3.amazonaws.com/tripdata/201703-citibike-tripdata.csv.zip"
temp <- tempfile()
download.file(activity_url, temp)
unzip(temp, "201703-citibike-tripdata.csv")
data <- read.csv("201703-citibike-tripdata.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
unlink(temp)

data$Start.Time <- as.POSIXct(strptime(data$Start.Time, "%Y-%m-%d %H:%M:%S"))
data$Start.Hours <- as.numeric(format(data$Start.Time, "%H"))
attach(data)
data <- subset(data, Trip.Duration >= 300, Trip.Duration <= 7200)
df <- as.data.frame(cbind(Start.Station.Longitude,Start.Station.Latitude,Start.Hours,Trip.Duration))

ui <- {
  library(shiny)
  
  # Define UI for slider demo application
  fluidPage(
    
    # Application title
    titlePanel("NY City Bike Trips Visualization"),
    
    # Sidebar with sliders that demonstrate various available
    # options
    sidebarLayout(
      sidebarPanel(
        # Simple integer interval
        sliderInput("Hour", "Start Hour:",
                    min=0, max=23, value=2),
        sliderInput("TripSeconds", "Trip Seconds >=",
                    min=300, max=7200, value=600)
      ),
      
      mainPanel(
        plotOutput('plot1')
      )
    )
  )
}

server <- {
  library(shiny)
  # Define server logic for slider examples
  function(input, output) {
    
    # Reactive expression to compose a data frame containing all of
    # the values
    sliderValues <- reactive({
      
      attach(df)
      df <- subset(df, Start.Hours == input$Hour)
      df <- subset(df, Trip.Duration >= input$TripSeconds)
      
      # getting the map
      mapgilbert <- get_map(location = c(lon = mean(df$Start.Station.Longitude), lat = mean(df$Start.Station.Latitude)), zoom = 12,
                            maptype = "terrain", scale = 2)
      
      # plotting the map with points 
      plot1 <- ggmap(mapgilbert) +
        geom_point(data = df, aes(x = Start.Station.Longitude, y = Start.Station.Latitude ,colour = Trip.Duration), size = 2, shape = 21) + 
        scale_colour_gradient(low = "red", high = "blue") + ggtitle("Trip durations at starting stations")
      source("http://peterhaschke.com/Code/multiplot.R")
      multiplot(plot1, cols=1)
      
    }) 
    # Show the values 
    output$plot1 <- renderPlot({
      sliderValues()
    })
  }
}

shinyApp(ui = ui, server = server)