# Load necessary libraries
library(sf)
library(dplyr)
library(shiny)
library(leaflet)
library(bslib)
library(stringr)
library(readxl)
library(readr)
library(tidyr)

# Read the GeoJSON file
# borough <- read_sf("https://raw.githubusercontent.com/radoi90/housequest-data/refs/heads/master/london_boroughs.geojson")
borough <-  st_read("london-boroughs_1179.geojson")
# print (borough)

# Read the amenities data
Amenities_Data <- read_excel("Amenities-Data.xls", sheet = "Cleaned_data")
# Delete the first two row using negative indexing
Amenities_Data <- Amenities_Data[-c(1, 2), ]


split_names <- strsplit(Amenities_Data$Boroughs, ":")
# Create a data frame with the split parts
df <- data.frame(
  First_Name = sapply(split_names, `[`, 1),  # Extract the first part
  Last_Name = sapply(split_names, `[`, 2)    # Extract the second part
)
df$First_Name <- gsub(" ", "", df$First_Name)
Amenities_Data$Boroughs <- df$First_Name

#read retailers data
Retailers_Data <- read_csv("geolytix_retailpoints_v33_202408.csv")

#filter the data only in county Greater London
Retailers_Data <- Retailers_Data %>% filter(county == "Greater London")
# View(Retailers_Data)


#restaurant scale vector
restaurant_scale <- c(min(Amenities_Data$`Licensed restaurants`), quantile(Amenities_Data$`Licensed restaurants`, 0.25), quantile(Amenities_Data$`Licensed restaurants`, 0.5), quantile(Amenities_Data$`Licensed restaurants`, 0.75), max(Amenities_Data$`Licensed restaurants`))
# cafe scale vector
cafe_scale <- c(min(Amenities_Data$`Unlicensed restaurants and cafes`), quantile(Amenities_Data$`Unlicensed restaurants and cafes`, 0.25), quantile(Amenities_Data$`Unlicensed restaurants and cafes`, 0.5), quantile(Amenities_Data$`Unlicensed restaurants and cafes`, 0.75), max(Amenities_Data$`Unlicensed restaurants and cafes`))
# take away scale vector
take_away_scale <- c(min(Amenities_Data$`Take-away food shops and mobile food stands`), quantile(Amenities_Data$`Take-away food shops and mobile food stands`, 0.25), quantile(Amenities_Data$`Take-away food shops and mobile food stands`, 0.5), quantile(Amenities_Data$`Take-away food shops and mobile food stands`, 0.75), max(Amenities_Data$`Take-away food shops and mobile food stands`))
# club scale vector
club_scale <- c(min(Amenities_Data$`Licensed clubs`), quantile(Amenities_Data$`Licensed clubs`, 0.25), quantile(Amenities_Data$`Licensed clubs`, 0.5), quantile(Amenities_Data$`Licensed clubs`, 0.75), max(Amenities_Data$`Licensed clubs`))
# bar scale vector
bar_scale <- c(min(Amenities_Data$`Public houses and bars`), quantile(Amenities_Data$`Public houses and bars`, 0.25), quantile(Amenities_Data$`Public houses and bars`, 0.5), quantile(Amenities_Data$`Public houses and bars`, 0.75), max(Amenities_Data$`Public houses and bars`))


# Define UI for the app
ui <- navbarPage(
  title = "Renting in London",
  
  # First Tab
  tabPanel(
    title = "London Amenity Map",
    sidebarLayout(
      sidebarPanel(
        # Add a slider with custom text labels
        uiOutput("slider"),
        uiOutput("slider_value"),
        uiOutput("slider_cafe"),
        uiOutput("slider_cafe_value"),
        uiOutput("slider_take_away"),
        uiOutput("slider_take_away_value"),
        uiOutput("slider_club"),
        uiOutput("slider_club_value"),
        uiOutput("slider_bar"),
        uiOutput("slider_bar_value"),
      ),
      mainPanel(
        leafletOutput("Amenity", width = "100%", height = "600px"),
        helpText("Click on a region to view the number of amenities in that borough.
                 Use the sliders on the side to highlight the regions with your desired number of amenities.")
      ),
      position = "right"
    )
  ),
  
  # Second Tab
  # tabPanel(
  #   title = "London Crime Map",
  #   sidebarLayout(
  #     sidebarPanel(
  #       sliderInput("restaurants", "Number of Restaurants per Area:", min = 0, max = 5, value = 2)
  #     ),
  #     mainPanel(
  #       leafletOutput("crimeMap", width = "100%", height = "600px")
  #     ),
  #     position = "right"
  #   )
  # )
)

create_slider_input <- function(input_id, label, ticks, value) {
  # Set up arguments for the slider
  args <- list(inputId = input_id, label = label, ticks = ticks, value = value)
  args$min <- 1
  args$max <- length(ticks)
  
  # Check the shiny version and modify the ticks accordingly
  if (sessionInfo()$otherPkgs$shiny$Version >= "0.11") {
    # This part works with shiny >= 1.5.0
    ticks_str <- paste0(ticks, collapse = ',')
    args$ticks <- TRUE
    html <- do.call('sliderInput', args)
    
    # Update the 'data-values' attribute for the ticks
    html$children[[2]]$attribs[['data-values']] <- ticks_str
    
  } else {
    # For shiny < 1.5.0, create the slider normally
    html <- do.call('sliderInput', args)
  }
  
  return(html)
}

customised_tick = c("Min", "Less", "Average", "More", "Max")


