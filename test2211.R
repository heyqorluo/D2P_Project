library(dplyr)
library(sf)
library(RColorBrewer)
library(leaflet)
library(ggplot2)
library(leaflet.extras)

borough <-  st_read("london-boroughs_1179.geojson")
print(borough)
ui <- fluidPage(
  titlePanel("Interactive Leaflet Map"),
  sidebarLayout(
    sidebarPanel(
      actionButton("clear_lines", "Clear Lines")  # Button to clear lines
    ),
    mainPanel(
      leafletOutput("map")  # Display the leaflet map
    )
  )
)

server <- function(input, output, session) {
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  # Use CartoDB tiles
      #define custom panes for the map
      addMapPane("basepane", zIndex = 400) %>%
      addMapPane("Restaurants", zIndex = 350) %>%
      
      addPolygons(
        data = borough,
        fillColor = "lightblue",
        color = "blue",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.5,
        label = ~name,
        layerId = ~code,
        options = pathOptions(pane="basepane"),
        group = "basepane"  # Add to a separate group for base layer
      ) %>%
      
      addPolygons(
        data = borough[borough$code == "E09000028" ,],
        fillColor = "yellow",
        color = "yellow",
        weight = 5,
        fillOpacity = 0.2,
        options = pathOptions(pane="Restaurants"),
        group = "Restaurants"
      )%>%
      
      
      # addLayersControl(
      #   overlayGroups = c("Restaurants"),  # Allow users to toggle highlighted regions
      #   options = layersControlOptions(collapsed = FALSE)
      # ) %>%
      setView(lng = -0.1276, lat = 51.5074, zoom = 10)
  })
  
  # Clear the lines when the button is clicked
  observeEvent(input$clear_lines, {
    leafletProxy("map") %>%
      clearGroup("Restaurants")
  })
}
shinyApp(ui, server)