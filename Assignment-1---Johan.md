Assignment 1
================
Johan Horsmans
6/2/2021

## Loading packages:

``` r
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(tidyverse)
```

    ## Warning: replacing previous import 'vctrs::data_frame' by 'tibble::data_frame'
    ## when loading 'dplyr'

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.0.4     v dplyr   1.0.0
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

Load
data:

``` r
map_data<-read.csv("RCFeature.csv") %>% na.omit() #Read data as .csv-file and omit NA's
```

Color specifications:

``` r
#Identify unique feature types:
unique(map_data$FeatureType)
```

    ## [1] "Other"                    "Artefact scatter"        
    ## [3] "Masonry"                  "Isolated find"           
    ## [5] "Metal Feature"            "Hole drilled in the rock"
    ## [7] "Platform"                 "Boulder"

``` r
# Define funcion which assigns a unique color to each feature type:
getColor <- function(map_data) {
  lapply(map_data$FeatureType, function(FeatureType) {
  if(FeatureType == "Boulder") { #If feature type is a "Boulder"...
    "green" #... Make the marker green
  } else if(FeatureType == "Masonry") { #If the feature type is a "Masonry"...
    "orange" #Make the marker orange.
  } else if(FeatureType == "Artefact scatter") {
    "blue"
  } else if(FeatureType == "Hole drilled in the rock") {
    "pink"
  } else if(FeatureType == "Isolated find") {
    "white"
  } else if(FeatureType == "Metal Feature") {
    "black"
  } else if(FeatureType == "Platform") {
    "brown"
  } else {
    "red"
  } })
}

#Define how I want my icoos to look like in my map: 
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(map_data)
)

# Define the same color scheme again for the map-legend:
beatCol <- colorFactor(palette = c("green","orange","blue","pink","white","black","brown","red"), 
levels = c("Boulder","Masonry","Artefact scatter","Hole drilled in the rock","Isolated find","Metal Feature","Platform","Other"))
```

Specify where the map should be centered as a
default:

``` r
l_bm <- leaflet(map_data) %>%   #Assign the base location of "Blue Mountains" to an object
  setView(150.25, -33.5, zoom = 7) #Retrieved the coordinates from: https://www.distancesto.com/coordinates/au/blue-mountains-latitude-longitude/history/276123.html
```

Define the maps to be included in the map:

``` r
#Using regular expression to grap all maps beginning with "Esri".
esri <- grep("^Esri", providers, value = TRUE)

#Add Esri-maps
for (provider in esri) {
  l_bm <- l_bm %>% addProviderTiles(provider, group = provider)
}
```

\#Create the map-object. The following code is based on the code from
class.

I only comment the lines that have customized:

``` r
BMmap <- l_bm %>% addTiles() %>% 
    addAwesomeMarkers(lng = map_data$Longitude, #Adding awesome markers to make it possible to assing each pin a unique color. Assigning longitude for each point.
             lat = map_data$Latitude, #Assigning latitude for each point.
             popup = paste("<b>Feature Type:</b>", map_data$FeatureType, "<br>", #Defining the features which should be included in the popup. <b> makes the font bold.
                           "<b>Feature ID:</b>", map_data$FeatureID, "<br>",
                           "<b>Description:</b>", map_data$Description), icon = icons,
             clusterOptions = markerClusterOptions(), group = "Cluster") %>% #Add clustering and save this layer to a group called "Cluster" 
  addCircles(lng = map_data$Longitude, #Add an additional layer with circle-markers. Define longitude.
             lat = map_data$Latitude, #Assign latitude
             popup = paste("<b>Feature Type:</b>", map_data$FeatureType, "<br>", #Defining features in the popup.
                           "<b>Feature ID:</b>", map_data$FeatureID, "<br>",
                           "<b>Description:</b>", map_data$Description), radius =  ~c(sqrt(map_data$Accuracy)*3), group = "Raw", color = getColor(map_data)) %>% #Set the radius of the circles to vary according to the square root of accuracy of the entry. Save this layer to a group called "Raw".
    addLayersControl(baseGroups = names(esri), 
                   options = layersControlOptions(collapsed = T), overlayGroups = c("Cluster","Raw")) %>% #Add a toggle switch to alternate between the clustered and circle visualization + the various maps.
  addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE, #Add mini map.
             position = "bottomright") %>%
  addMeasure( #Add measure tools.
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479") %>% 
  addLegend('bottomright', pal = beatCol, values = map_data$FeatureType,
            title = 'Legend:',
            opacity = 1) %>% #Add legend to bottom right corner (using the colorscheme from earlier).
  htmlwidgets::onRender("
                         function(el, x) {
                         var myMap = this;
                         myMap.on('baselayerchange',
                         function (e) {
                         myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                         })
                         }") %>% 
addControl("", position = "topright")
```

Save map as a html document

``` r
saveWidget(BMmap, "BMmap.html", selfcontained = TRUE)
```
