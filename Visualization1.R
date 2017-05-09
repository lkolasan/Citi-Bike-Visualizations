# loading the required packages
library(ggplot2)
library(ggmap)
library(rjson)
library(ggrepel)
library(httr)

# Importing data from json feed and converting to data frame
url <- 'https://feeds.citibikenyc.com/stations/stations.json'
bike <- content(GET(url))
bikestations <- bike$stationBeanList
bikeunlist <- data.frame(matrix(unlist(bikestations), ncol=18, byrow=T))
colnames(bikeunlist) <- names(unlist(bikestations)[0:18])
bikeunlist$longitude <- as.numeric(as.character(bikeunlist$longitude))
bikeunlist$latitude <- as.numeric(as.character(bikeunlist$latitude))
bikeunlist$statusValue <- as.factor(bikeunlist$statusValue)
df <- bikeunlist[c(6,5,7,2)]

# getting the map
mapgilbert <- get_map(location = c(lon = mean(df$longitude), lat = mean(df$latitude)), zoom = 12,
                      maptype = "roadmap", scale = 2)

# plotting the map with points 
plot1 <- ggmap(mapgilbert) +
  geom_point(data = df, aes(x = longitude, y = latitude ,colour = factor(statusValue),fill = factor(statusValue)), size = 2, shape = 21) + 
  scale_fill_manual(values=c("blue", "black")) + 
  scale_colour_manual(values=c("white", "black")) +
  geom_label_repel(data=subset(df, df$statusValue=="Not In Service"), aes(x = longitude, y = latitude, label=stationName)) +
  ggtitle("Realtime plot of Stations not in service")

source("http://peterhaschke.com/Code/multiplot.R")
multiplot(plot1, cols=1)

