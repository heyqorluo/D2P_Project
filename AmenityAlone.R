# Load necessary libraries
library(sf)
library(dplyr)
library(shiny)
library(leaflet)
library(bslib)
library(stringr)
library(readxl)
library(readr)


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
        
        # fluidRow(
        #   column(3,div(style = "display: flex; justify-content: center; align-items: center; padding-top: 10px; padding-bottom: 20px;",
        #                img(src = "restaurant.png", height = "70px")),
        #          p("Number of Licensed restaurants")),
        #   column(3,div(style = "display: flex; justify-content: center; align-items: center; padding-top: 20px; padding-bottom: 20px;",
        #                img(src = "cafe.png", height = "60px")),
        #          p("Number of Unlicensed restaurants and cafes")),
        #   column(3,div(style = "display: flex; justify-content: center; align-items: center; padding-top: 20px; padding-bottom: 30px;",
        #                img(src = "takeaway.png", height = "50px")),
        #          p("Number of Take-away food shops and mobile food stands")),
        #   column(3,div(style = "display: flex; justify-content: center; align-items: center; padding-top: 20px; padding-bottom: 20px;",
        #                img(src = "coffee.png", height = "60px")),
        #          p("Number of Licensed clubs")),
        #   
        # )
      )
      ,
      mainPanel(
        leafletOutput("Amenity", width = "100%", height = "600px"),
        helpText("Click on a region to view the number of amenities in that borough.
                 Use the sliders on the side to highlight the regions with your desired number of amenities.")
      ),
      position = "right"
    )
  ),
  
  # Second Tab
  tabPanel(
    title = "London Crime Map",
    sidebarLayout(
      sidebarPanel(
        sliderInput("restaurants", "Number of Restaurants per Area:", min = 0, max = 5, value = 2)
      ),
      mainPanel(
        leafletOutput("crimeMap", width = "100%", height = "600px")
      ),
      position = "right"
    )
  )
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

# Define server logic for the app
server <- function(input, output, session) {
  
  # IMPORTANT: kills the process when closing the app
  session$onSessionEnded(function() { stopApp() })
  
  thematic::thematic_shiny()
  
  # Show the modal dialog when the app starts
  showModal(modalDialog(
    title = HTML('Welcome to    <img src="logo.png" style="height: 50px; width: auto;"/>'),
    HTML("Explore the area for first-time renters in London.<br><br> 
    Use the checkboxes to toggle between crash data, bike lanes and pollution metrics.<br><br>
    Disclaimer: There may be discreprencies between PEDAL's data and
    real-world observations. PEDAL is still under development and is
    not resposible for any insights drawn. <br><br> By pressing OK, you agree to this."),
    
    easyClose = TRUE, # When true, clicking outside the popup will close it
    # Add an 'OK' button to the modal dialog, when clicked the popup is closed
    footer = modalButton("Accept. Let's go!", icon = icon("thumbs-up"))
  ))
  
  
  # Use the helper function to render the slider for restaurants
  output$slider <- renderUI({
    create_slider_input(input_id = "foo", label = "Choose the Level of Restaurants:", ticks = customised_tick, value = 2)
  })
  # Use the helper function to render the slider for cafes
  output$slider_cafe <- renderUI({
    create_slider_input(input_id = "foo_cafe", label = "Choose the Level of Cafes:", ticks = customised_tick, value = 2)
  })
  # Use the helper function to render the slider for take-away
  output$slider_take_away <- renderUI({
    create_slider_input(input_id = "foo_take_away", label = "Choose the Level of Take-away:", ticks = customised_tick, value = 2)
  })
  # Use the helper function to render the slider for clubs
  output$slider_club <- renderUI({
    create_slider_input(input_id = "foo_club", label = "Choose the Level of Clubs:", ticks = customised_tick, value = 2)
  })
  # Use the helper function to render the slider for bars
  output$slider_bar <- renderUI({
    create_slider_input(input_id = "foo_bar", label = "Choose the Level of Bars:", ticks = customised_tick, value = 2)
  })
  
  
  # Reactive value to store clicked region information
  selected_region <- reactiveVal(NULL)
  
  # Render the leaflet map
  output$Amenity <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "Original") %>%  # Add default OpenStreetMap tiles
      addProviderTiles(providers$CartoDB.Positron,group = "Minimal") %>%  # Use CartoDB tiles
      #define custom panes for the map
      addMapPane(name = "Retailers", zIndex = 460) %>%
      addMapPane(name = "BaseLayer", zIndex = 450) %>%
      addMapPane(name = "Restaurants", zIndex = 400) %>%
      addMapPane(name = "Cafes", zIndex = 410) %>%
      addMapPane(name = "Take-away", zIndex = 420) %>%
      addMapPane(name = "Clubs", zIndex = 430) %>%
      addMapPane(name = "Bars", zIndex = 440) %>%
      addPolygons(
        data = borough,
        fillColor = "lightgrey",
        color = "grey",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.1,
        highlightOptions = highlightOptions(
          weight = 5,
          opacity = 0.9,
          color = "yellow",
          bringToFront = TRUE),
        label = ~name,
        layerId = ~code,
        options = pathOptions(pane="BaseLayer"),
        group = "BaseLayer"  # Add to a separate group for base layer
      ) %>%
      
      addCircleMarkers(
        data = Retailers_Data,~long_wgs, ~lat_wgs,
        popup = ~store_name,
        color = "orange",fillOpacity = 0.5,
        weight = 0, radius = 3,
        options = pathOptions(pane="Retailers"),
        group = "Retailers"
      )%>%
      
      addLayersControl(
        baseGroups = c("Minimal","Original" ),  
        overlayGroups = c("Retailers","Restaurants","Cafes","Take-away","Clubs","Bars"), 
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("Retailers","Restaurants","Cafes","Take-away","Clubs","Bars")) %>%
      setView(lng = -0.1276, lat = 51.5074, zoom = 10)
  })
  
  
  observe({
    #read slider value
    slider_value <-input$foo
    #print(slider_value)
    if(!is.null(slider_value)){
      # Filter regions where the "Licensed restaurants" exceed the threshold
      highlighted_regions <- Amenities_Data %>%
        filter(`Licensed restaurants` >= restaurant_scale[input$foo+1])
      
      # # Update the map with highlighted regions
      leafletProxy("Amenity") %>%  # Update the "Amenity" map
        clearGroup(group = "Restaurants") %>%  # Clear the highlighted regions
        addPolygons(
          data = borough[borough$code %in% highlighted_regions$Boroughs, ],
          fillColor = "lightblue",
          color = "lightblue",
          weight = 5,
          fillOpacity = 0.4,
          options = pathOptions(pane="Restaurants"),
          group = "Restaurants"
        )
    }
  })
  #obeserve for cafe
  observe({
    #read slider value
    slider_value <-input$foo_cafe
    #print(slider_value)
    if(!is.null(slider_value)){
      # Filter regions where the "Unlicensed restaurants and cafes" exceed the threshold
      highlighted_regions <- Amenities_Data %>%
        filter(`Unlicensed restaurants and cafes` >= cafe_scale[input$foo_cafe+1])
      
      # # Update the map with highlighted regions
      leafletProxy("Amenity") %>%  # Update the "Amenity" map
        clearGroup(group = "Cafes") %>%  # Clear the highlighted regions
        addPolygons(
          data = borough[borough$code %in% highlighted_regions$Boroughs, ],
          fillColor = "lightblue",
          color = "lightblue",
          weight = 5,
          fillOpacity = 0.4,
          options = pathOptions(pane="Cafes"),
          group = "Cafes"
        )
    }
  })
  #obeserve for take-away
  observe({
    #read slider value
    slider_value <-input$foo_take_away
    #print(slider_value)
    if(!is.null(slider_value)){
      # Filter regions where the "Take-away food shops and mobile food stands" exceed the threshold
      highlighted_regions <- Amenities_Data %>%
        filter(`Take-away food shops and mobile food stands` >= take_away_scale[input$foo_take_away+1])
      
      # # Update the map with highlighted regions
      leafletProxy("Amenity") %>%  # Update the "Amenity" map
        clearGroup(group = "Take-away") %>%  # Clear the highlighted regions
        addPolygons(
          data = borough[borough$code %in% highlighted_regions$Boroughs, ],
          fillColor = "lightblue",
          color = "lightblue",
          weight = 5,
          fillOpacity = 0.4,
          options = pathOptions(pane="Take-away"),
          group = "Take-away"
        )
    }
  })
  #obeserve for club
  observe({
    #read slider value
    slider_value <-input$foo_club
    #print(slider_value)
    if(!is.null(slider_value)){
      # Filter regions where the "Licensed clubs" exceed the threshold
      highlighted_regions <- Amenities_Data %>%
        filter(`Licensed clubs` >= club_scale[input$foo_club+1])
      
      # # Update the map with highlighted regions
      leafletProxy("Amenity") %>%  # Update the "Amenity" map
        clearGroup(group = "Clubs") %>%  # Clear the highlighted regions
        addPolygons(
          data = borough[borough$code %in% highlighted_regions$Boroughs, ],
          fillColor = "lightblue",
          color = "lightblue",
          weight = 5,
          fillOpacity = 0.4,
          options = pathOptions(pane="Clubs"),
          group = "Clubs"
        )
    }
  })
  #obeserve for bar
  observe({
    #read slider value
    slider_value <-input$foo_bar
    #print(slider_value)
    if(!is.null(slider_value)){
      # Filter regions where the "Public houses and bars" exceed the threshold
      highlighted_regions <- Amenities_Data %>%
        filter(`Public houses and bars` >= bar_scale[input$foo_bar+1])
      
      # # Update the map with highlighted regions
      leafletProxy("Amenity") %>%  # Update the "Amenity" map
        clearGroup(group = "Bars") %>%  # Clear the highlighted regions
        addPolygons(
          data = borough[borough$code %in% highlighted_regions$Boroughs, ],
          fillColor = "lightblue",
          color = "lightblue",
          weight = 5,
          fillOpacity = 0.4,
          options = pathOptions(pane="Bars"),
          group = "Bars"
        )
    }
  })
  
  output$slider_value <- renderUI({
    # Create dynamic text using the slider value
    text <- paste("You have selected a threshold value of:", restaurant_scale[input$foo+1])
    # Return the text with HTML styling
    tags$i(style = "font-size: 10px; font-weight: bold; color: #6DAFDC", text)
  })
  output$slider_cafe_value <- renderUI({
    # Create dynamic text using the slider value
    text <- paste("You have selected a threshold value of:", cafe_scale[input$foo_cafe+1])
    # Return the text with HTML styling
    tags$i(style = "font-size: 10px; font-weight: bold; color: #6DAFDC", text)
  })
  output$slider_take_away_value <- renderUI({
    # Create dynamic text using the slider value
    text <- paste("You have selected a threshold value of:", take_away_scale[input$foo_take_away+1])
    # Return the text with HTML styling
    tags$i(style = "font-size: 10px; font-weight: bold; color: #6DAFDC", text)
  })
  output$slider_club_value <- renderUI({
    # Create dynamic text using the slider value
    text <- paste("You have selected a threshold value of:", club_scale[input$foo_club+1])
    # Return the text with HTML styling
    tags$i(style = "font-size: 10px; font-weight: bold; color: #6DAFDC", text)
  })
  output$slider_bar_value <- renderUI({
    # Create dynamic text using the slider value
    text <- paste("You have selected a threshold value of:", bar_scale[input$foo_bar+1])
    # Return the text with HTML styling
    tags$i(style = "font-size: 10px; font-weight: bold; color: #6DAFDC", text)
  })
  
  
  
  # Event listener for click on boroughs (regions)
  observeEvent(input$Amenity_shape_click, {
    click <- input$Amenity_shape_click
    clicked_borough <- click$id
    Amenities_Data$Boroughs <- as.character(Amenities_Data$Boroughs)
    
    # Store the clicked region's information
    selected_region(clicked_borough)
    
    # pull Licensed restaurants number
    restaurant_num <- Amenities_Data[Amenities_Data$Boroughs == clicked_borough , "Licensed restaurants"]
    # pull Unlicensed restaurants and cafes
    un_cafe_num <- Amenities_Data[Amenities_Data$Boroughs == clicked_borough , "Unlicensed restaurants and cafes"]
    # pull Take-away food shops and mobile food stands
    take_away_num <- Amenities_Data[Amenities_Data$Boroughs == clicked_borough , "Take-away food shops and mobile food stands"]
    # pull Licensed clubs
    club_num <- Amenities_Data[Amenities_Data$Boroughs == clicked_borough , "Licensed clubs"]
    # pull Public houses and bars
    bar_num <- Amenities_Data[Amenities_Data$Boroughs == clicked_borough , "Public houses and bars"]
    
    mytext <- paste(
      "Number of Licensed restaurants: ", restaurant_num, "<br/>",
      "Number of Unlicensed restaurants and cafes: ", un_cafe_num, "<br/>",
      "Number of Take-away food shops and mobile food stands: ", take_away_num, "<br/>",
      "Number of Licensed clubs: ", club_num, "<br/>",
      "Number of Public houses and bars: ", bar_num, "<br/>"
    ) %>%
      lapply(htmltools::HTML)
    mycontent <- HTML(paste0(
      '<div class="popup-content" style="max-width: 180px; word-wrap: break-word; padding: 10px;">',
      '<div class="row">',
      # Restaurants
      '<div class="col-3" style="display: flex; align-items: center;">',
      '<img src="restaurant.png" height="30px" style="margin-right: 10px;"  /><br>',
      '<p>Licensed Restaurants: ', restaurant_num, '</p>',
      '</div>',
      
      # Cafes
      '<div class="col-3" style="display: flex; align-items: center;">',
      '<img src="cafe.png" height="30px" style="margin-right: 10px;" /><br>',
      '<p>Unlicensed Restaurants and Cafes: ', un_cafe_num, '</p>',
      '</div>',
      
      # Takeaways
      '<div class="col-3" style="display: flex; align-items: center;">',
      '<img src="takeaway.png" height="25px" style="margin-right: 10px;" /><br>',
      '<p>Take-away Food Shops: ', take_away_num, '</p>',
      '</div>',
      
      # Clubs
      '<div class="col-3" style="display: flex; align-items: center;">',
      '<img src="club.png" height="30px" style="margin-right: 10px;" /><br>',
      '<p>Licensed Clubs: ', club_num, '</p>',
      '</div>',
      
      # Bars
      '<div class="row">',
      '<div class="col-3" style="display: flex; justify-content: center; align-items: center;">',
      '<img src="bar.png" height="30px" style="margin-right: 10px;" /><br>',
      '<p>Public Houses and Bars: ', bar_num, '</p>',
      '</div>',
      '</div>',
      '</div>'
    ))
    
    # Update the map with a popup
    leafletProxy("Amenity") %>%
      clearPopups() %>%
      addPopups(lng = click$lng, lat = click$lat, popup = mycontent)
    
  })
  
  output$crimeMap <-renderLeaflet({
    leaflet(data = borough) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  # Use CartoDB tiles
      addPolygons(
        fillColor = "lightblue",
        color = "blue",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.4,
        label = ~name
      ) %>%
      setView(lng = -0.1276, lat = 51.5074, zoom = 10)
  })
}


# Run the application
shinyApp(ui = ui, server = server)