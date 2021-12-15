
#DataVis Final Project
#Group 4E - Megos, Pritchett, Saunders
#South Bend Abandoned Properties Reclamation Dashboard - Shiny App
#12/15/21
#Version 1

#Load Libraries
library(sf)
library(ggmap)
library(tidyverse)
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(ggmap)
library(rgdal)

# Load Data
#We are using multiple geospatial data sources around park & school locations, 
#City Council Districts, and Abandoned Properties 
#Our parks data is a csv while the other data sources are shapefiles.
Parks = read.csv("Parks_Locations_and_Features.csv")
School_Boundaries = st_read("School_Boundaries/School_Boundaries.shp", stringsAsFactors = FALSE)
Abandoned_Properties = st_read("Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp", stringsAsFactors = FALSE)
City_Council_Dist = st_read("City_Council_Districts/City_Council_Districts.shp", stringsAsFactors = FALSE)



#Define our Shiny server app object
#We are using a selector menu on the left panel with 3 interactive tabs in the main panel
ui <- fluidPage(
  headerPanel(title = "Group 4 - Data Vis Final Project"),
  verticalLayout(
    wellPanel(
      textOutput("widgets")
    )), 
  sidebarLayout(
    sidebarPanel(
      selectInput('pickedDistrict', 'District', City_Council_Dist$OBJECTID)
    ),
    mainPanel( 
      tabsetPanel
      (type = "tab",
        tabPanel("Interactive Map",leafletOutput(outputId = "mymap")),
        tabPanel("School Type",plotOutput("schooltype")),
        tabPanel("Abandoned by Council Dist",tableOutput("abandonByCouncil"))
        
      )
    )
  )
)


#In order to use our shape files, we must project them into a coordinate space that Leaflet can consume
#This block projects/transforms these datasets for use wth our Leaflet map, using the WGS84 coordinate system
st_transform(School_Boundaries,CRS("+proj=longlat +datum=WGS84")) -> schoolBoundTrans
st_transform(Abandoned_Properties,CRS("+proj=longlat +datum=WGS84")) -> abandonedTrans
st_transform(City_Council_Dist,CRS("+proj=longlat +datum=WGS84")) -> cityCouncil



#Create our Shiny app server function and call it with our data
server <- function(input, output) {

  #Implement our abandoned property-by-district interactive tab
  output$abandonByCouncil <- renderTable({
    Abandoned_Properties %>% 
      filter(Council_Di == input$pickedDistrict) %>% 
      select(Council_Di,Street_Nam, Code_Enfor, Structures) %>%
      st_set_geometry(NULL) %>% rename('Council District' = 1, 'Street Name' = 2, 'Code Enforced' = 3)
  }
  )
  
  #Implement a static visualization plot for public/private school mix in South Bend
  #This would be better-implemented as an interactive element, tied to council district, but our data 
  #did not have elements that would allow us to join the school data with council data.
  #This element would be made interactive if this dashboard is adopted. We would only need to acquire 
  #or derive the linkage data.
  output$schooltype <- renderPlot({
    private <- sum(schoolBoundTrans$SchoolType=="Private")
    public <- sum(schoolBoundTrans$SchoolType=="Public")
    
    slices <- c(private,public)
    lbls <- c("Private School", "Public School")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels
    lbls <- paste(lbls,"%, n = ",c(private,public),sep="") # ad % to labels
    pie3D(slices,labels = lbls, col = c("#3182bd","#A71B4B"),
          main="School Type",labelpos=c(1.559742,5.01335))
  }
  )
  
  #Implement our interactive Leaflet map 
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      #Add visual enhancements toggles
      addTiles(group = 'OSM (default)') %>% 
      addProviderTiles(providers$Stamen.Toner, group = 'Toner') %>% 
      addProviderTiles(providers$Stamen.TonerLite, group = 'TonerLite') %>% 
      #Set our default view to be geo coordinates for South Bend
      setView(lng = -86.25041, lat = 41.67916, zoom = 12)  %>% 
      #Create school boundary layer object
      addPolygons(data=schoolBoundTrans,weight=5,col = 'blue',group = 'Schools') %>%
      #Create abandoned properties layer object
      addPolygons(data=abandonedTrans,weight=5,col = 'red',group = 'AbandonedProperty') %>%
      #Create the City Council layer object - using filter to allow for interaction with picker
      addPolygons(data=(cityCouncil %>% filter(OBJECTID == input$pickedDistrict)),weight=5,
                  col = 'yellow', layerId = cityCouncil$Num, group = 'CouncilDistricts') %>%
      addCircles(data=Parks,lat = ~ Lat, lng = ~ Lon, radius = 100, stroke = F,group='Parks', 
                 col = 'green', weight = 500, fillOpacity = .7) %>% 
      #Add layer visibility & visual enhancement controls
      addLayersControl(
        baseGroups = c('OSM (default)', 'Toner', 'TonerLite'),
        overlayGroups =c('CouncilDistricts', 'Schools','AbandonedProperty','Parks'),
        options = layersControlOptions(collapsed=FALSE)
      )
  }
  )
  
  
  
  
}

#Execute & Render our app
shinyApp(ui, server)






