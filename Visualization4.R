# loading the required packages
library(ggplot2)
library(ggmap)
library(rjson)

# capturing data from json feed and converting to data frame
url <- 'https://feeds.citibikenyc.com/stations/stations.json'
bike <- content(GET(url))
bikestations <- bike$stationBeanList
bikeunlist <- data.frame(matrix(unlist(bikestations), ncol=18, byrow=T))
colnames(bikeunlist) <- names(unlist(bikestations)[0:18])
longitude <- as.numeric(as.character(bikeunlist$longitude))
latitude <- as.numeric(as.character(bikeunlist$latitude))
availableDocks <- as.numeric(as.character(bikeunlist$availableDocks))
availableBikes <- as.numeric(as.character(bikeunlist$availableBikes))
df <- as.data.frame(cbind(longitude,latitude,availableDocks,availableBikes))

# getting the map
mapgilbert <- get_map(location = c(lon = mean(df$longitude), lat = mean(df$latitude)), zoom = 12,
                      maptype = "roadmap", scale = 2)

# plotting the map with points 
plot1 <- ggmap(mapgilbert) +
  geom_point(data = df, aes(x = longitude, y = latitude ,colour = availableDocks), size = 1, shape = 21) + scale_colour_gradient(low = "red", high = "blue") + ggtitle("Realtime plot showing the density of available Docks at stations")

plot2 <- ggmap(mapgilbert) +
  geom_point(data = df, aes(x = longitude, y = latitude ,colour = availableBikes), size = 1, shape = 21) + scale_colour_gradient(low = "red", high = "blue") + ggtitle("Realtime plot showing the density of available Bikes at stations")

source("http://peterhaschke.com/Code/multiplot.R")
multiplot(plot1, plot2, cols=2)
