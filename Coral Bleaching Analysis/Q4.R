library(leaflet)


# Reading the data file
bleach <- read.csv("assignment-02-data-formated.csv")

head(data)

# Creating LeafMap based on longitude and latitude, popup will give the name of locations
leaflet(data = bleach) %>% 
  
  # Default Open Street map
  addTiles() %>% 
  addMarkers(~longitude, ~latitude, popup = ~as.character(location)) 






# assignment-02-data-formated.csv