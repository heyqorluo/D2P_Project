library(dplyr)
library(sf)
library(RColorBrewer)
library(leaflet)
library(ggplot2)
library(leaflet.extras)

rand_lng <- function(n = 10) rnorm(n, -93.65, .01)
rand_lat <- function(n = 10) rnorm(n, 42.0285, .01)

random_data <- data.frame(
  lng = rand_lng(50),
  lat = rand_lat(50),
  radius = runif(50, 50, 150),
  circleId = paste0("circle #", 1:50),
  lineId = paste0("circle #", 1:50)
)
print(random_data)


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
  addTiles() %>%
  # move the center to Snedecor Hall
  setView(-93.65, 42.0285, zoom = 14) %>%
  addMapPane("ameslines", zIndex = 410) %>% # shown below ames_circles
  addMapPane("amescircles", zIndex = 420) %>% # shown above ames_lines
  addMapPane("hello", zIndex = 430) %>% # shown above ames_lines
  # lines in 'ames_lines' pane
  addPolygons(
    data = random_data, ~lng, ~lat, color = "#F00", weight = 20,
    options = pathOptions(pane = "ameslines"),
    group = "ameslines"
  )%>%
    addPolygons(
      data = random_data, ~lng, ~lat, color = "blue",fillOpacity = 0.5, weight = 20,
      options = pathOptions(pane = "hello"),
      group = "hello"
    )
})
  
  # Clear the lines when the button is clicked
  observeEvent(input$clear_lines, {
    leafletProxy("map") %>%
      clearGroup("hello")
  })
}
shinyApp(ui, server)