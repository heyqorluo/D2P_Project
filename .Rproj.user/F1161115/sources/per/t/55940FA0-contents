  # Load necessary libraries
# run the App with the Run App functionality on the top right of this R project
  library(sf)
  library(dplyr)
  library(shiny)
  library(leaflet)
  library(bslib)
  library(ggplot2)
  library(plotly)
  library(RColorBrewer)
  library(viridis)
  library(ggbreak)
  
  # Import tab info
  # print(summary_data)
  source("CrimeTest.R")
  source("PricingMap.R")
  source("AmenityMap.R")

  
  # read data
  pricing_data <- readxl::read_xlsx("Pricing_Data.xlsx")

  source("optimiser.R")
  
  # Read data

  borough <-  st_read("london-boroughs_1179.geojson")
  lsoa <-  st_read("lsoa.geojson")
  borough1 <- st_read("boundary.geojson")

  # Define UI for the app
  ui <- tagList(
    # Add custom CSS to adjust the Title and Tabs position
    tags$style(HTML("
    .navbar-default .navbar-nav {
      margin-top: 50px; /* Add space between title and tabs */
    }
    .navbar-brand {
      display: block;
      text-align: centre; /* Center the title if needed */
    }
    .navbar { 
      border-bottom: 2px solid #ddd; /*Add a border below the navbar */
    }
  ")),
    
    #Adding title above the tabs
    navbarPage(
      title = div(
        style = "flex-grow: 1; display: flex; justify-content: center; align-items: center; font-size: 24px; font-weight: bold; height: 100%;",
        "BoroughMatch"
      ),
    
    # First Tab - Crime data decriptive analysis
    tabPanel(
      title = "London Crime Data",
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(
              width= 12,
              plotlyOutput("crimePieChart", height = "600px") #initialise pie chart
            )
          )
        ),
        mainPanel(
          fluidRow(
            column(
              width = 12,
              leafletOutput("boroughMap", width = "100%", height = "600px"), #initialise crime map
              helpText("Click on a borough to see the composition of crimes in that area in pie chart.")
            )
          )
        ),
        position = "right"
      )
    ),
    
    
    # Second Tab - Renting data descriptive analysis

    tabPanel(
      title = "London House Renting Map",
      sidebarLayout(
        sidebarPanel(
          selectInput(        #select from drop down menu for types of houses
            "house_type", 
            "Select House Type:", 
            choices = c(
              "One bedroom", "Two bedrooms", 
              "Three bedrooms", "Four bedrooms", 
              "Five bedrooms", "Six bedrooms"
            ),
            selected = "One bedroom"
          ), 
          plotlyOutput("PriceBarChart", height = "600px") #  initialise price bar chart
        ),
        mainPanel(
          leafletOutput("Pricing", width = "100%", height = "600px") #initialise price map
        ),
        position = "right"
      )
    ),
    
    # Third Tab - Amenity data desriptive analysis
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
          leafletOutput("Amenity", width = "100%", height = "600px"), #initialise amenity map
          helpText("Click on a region to view the number of amenities in that borough.
                 Use the sliders on the side to highlight the regions with your desired number of amenities.")
        ),
        position = "right"
      )

    ),

    
    # Fourth Tab - Prescriptive Analysis for recommending area
    tabPanel(
      title = "Recommendation Tab",
      
      #slider inputs to define weights for the prescriptive analysis optimiser
      sidebarLayout(
        sidebarPanel(
          sliderInput("Amenitiestoggle", label = "How much consideration would you give to amenities?", min = 0, max = 1, value = 0, step = NULL),
          selectInput(
            "AmenityTypes", 
            "Select Your Most Wanted Amenity Type:", 
            choices = c(
              "Licensed restaurants", "Unlicensed restaurants and cafes", 
              "Take-away food shops and mobile food stands", "Licensed clubs", 
              "Public houses and bars"
            ),
            selected = "Licensed restaurants"
          ),
          sliderInput("Crimetoggle", label = "How much consideration would you give to low crime rates?", min = 0, max = 1, value = 0, step = NULL),
          selectInput(
            "CrimeTypes", 
            "Select Your Least Wanted Crime Type:", 
            choices = c(
              "ARSON AND CRIMINAL DAMAGE",
              "BURGLARY",
              "DRUG OFFENCES",
              "FRAUD AND FORGERY",
              "MISCELLANEOUS CRIMES AGAINST SOCIETY",
              "POSSESSION OF WEAPONS",
              "PUBLIC ORDER OFFENCES",
              "ROBBERY",
              "SEXUAL OFFENCES",
              "THEFT",
              "VEHICLE OFFENCES",
              "VIOLENCE AGAINST THE PERSON"
            ),
            selected = "ARSON AND CRIMINAL DAMAGE"
          ),
          
          #select input for type of houses based on number of rooms
          selectInput(
            "HouseTypes", 
            "Select House Type:", 
            choices = c(
              "One bedroom", "Two bedrooms", 
              "Three bedrooms", "Four bedrooms", 
              "Five bedrooms", "Six bedrooms"
            ),
            selected = "One bedroom"
          ), 
          #selecting price range
          sliderInput("Range", label = "Select your monthly price range", min = 600, max = 1900, value = c(700, 1000), step = 50, ticks = TRUE)
        ),
        mainPanel(
          leafletOutput("Prescriptive", width = "100%", height = "600px") #initialise map for prescribing area
        ),
        position = "right"
      )
    )

))
  

  
  # Define server logic for the app
  server <- function(input, output, session) {

    # IMPORTANT: kills the process when closing the app
    session$onSessionEnded(function() { stopApp() })
    
    thematic::thematic_shiny()
    
    # Show the welcome dialog when the app starts
    showModal(modalDialog(
      title = HTML('Welcome to    <img src="logo.png" style="height: 50px; width: auto;"/> BoroughMatch'),
      HTML(" 
    Discover the ideal place to rent in London with our interactive dashboard! <br><br>
    Explore crime data across boroughs, access detailed amenities maps for grocery stores, restaurants, and more.
           Finally, let us recommend the perfect area tailored to your needs. Use the tabs to start your journey to smarter renting!<br><br> 
           By pressing OK, you agree to this."),
      
      easyClose = TRUE, # When true, clicking outside the popup will close it
      # Add an 'OK' button to the modal dialog, when clicked the popup is closed
      footer = modalButton("Accept. Let's go!", icon = icon("thumbs-up"))
    ))
    
    
    
    
    # Amenity Code:
    # Use the helper function from AmenityMap.R to render the slider for restaurants
    output$slider <- renderUI({
      create_slider_input(input_id = "foo", label = "Choose the Level of Restaurants:", ticks = customised_tick, value = 2)
    })
    # Use the helper function from AmenityMap.R to render the slider for cafes
    output$slider_cafe <- renderUI({
      create_slider_input(input_id = "foo_cafe", label = "Choose the Level of Cafes:", ticks = customised_tick, value = 2)
    })
    # Use the helper function from AmenityMap.R to render the slider for take-away
    output$slider_take_away <- renderUI({
      create_slider_input(input_id = "foo_take_away", label = "Choose the Level of Take-away:", ticks = customised_tick, value = 2)
    })
    # Use the helper function from AmenityMap.R to render the slider for clubs
    output$slider_club <- renderUI({
      create_slider_input(input_id = "foo_club", label = "Choose the Level of Clubs:", ticks = customised_tick, value = 2)
    })
    # Use the helper function from AmenityMap.R to render the slider for bars
    output$slider_bar <- renderUI({
      create_slider_input(input_id = "foo_bar", label = "Choose the Level of Bars:", ticks = customised_tick, value = 2)
    })
    
    
    # Reactive value to store clicked region information for AMENITY MAP
    selected_region <- reactiveVal(NULL)
    # Reactive value to store clicked region information for CRIME MAP
    selected_region1 <- reactiveVal(NULL) 
    
    # Render the leaflet map
    # Render Amenity Map
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
        # Add dots for retailers location
        addCircleMarkers(
          data = Retailers_Data,~long_wgs, ~lat_wgs,
          popup = ~store_name,
          color = "orange",fillOpacity = 0.5,
          weight = 0, radius = 3,
          options = pathOptions(pane="Retailers"),
          group = "Retailers"
        )%>%
        # Add layercontrol panel to allow users choose different layers
        addLayersControl(
          baseGroups = c("Minimal","Original" ),  
          overlayGroups = c("Retailers","Restaurants","Cafes","Take-away","Clubs","Bars"), 
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup(c("Retailers","Restaurants","Cafes","Take-away","Clubs","Bars")) %>%
        setView(lng = -0.1276, lat = 51.5074, zoom = 10)
    })
    
    # Monitor the slider inputs and update the map accordingly
    #obeserve for restaurant
    observe({
      #read slider value for restaurant
      slider_value <-input$foo
      if(!is.null(slider_value)){
        # Filter regions where the "Licensed restaurants" exceed the threshold
        highlighted_regions <- Amenities_Data %>%
          filter(`Licensed restaurants` >= restaurant_scale[input$foo+1])
        
        # Update the map with highlighted regions
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
      # read slider value for cafe
      slider_value <-input$foo_cafe
      #print(slider_value)
      if(!is.null(slider_value)){
        # Filter regions where the "Unlicensed restaurants and cafes" exceed the threshold
        highlighted_regions <- Amenities_Data %>%
          filter(`Unlicensed restaurants and cafes` >= cafe_scale[input$foo_cafe+1])
        
        # Update the map with highlighted regions
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
      # read slider value for take-away
      slider_value <-input$foo_take_away
      #print(slider_value)
      if(!is.null(slider_value)){
        # Filter regions where the "Take-away food shops and mobile food stands" exceed the threshold
        highlighted_regions <- Amenities_Data %>%
          filter(`Take-away food shops and mobile food stands` >= take_away_scale[input$foo_take_away+1])
        
        # Update the map with highlighted regions
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
      # read slider value for club
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
      # read slider value for bar
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
    # Render text for threshold value input by user
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
      # Create popup box including number of amenity and icons
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
    

    # Crime Code:
    
    #read crime data file
    crime_data_new <- read.csv("crime-data-sorted.csv")
    
    # Render the leaflet map
    output$boroughMap <- renderLeaflet({
        m #referenced from CrimeTest.R
    })
    
    
    # Event listener for click on boroughs (regions)
    observeEvent(input$boroughMap_shape_click, {
      click <- input$boroughMap_shape_click
      clicked_borough <- click$id
      selected_region1(clicked_borough) 
    
    })   
    
    #Render for output pie chart when a borough is selected
      output$crimePieChart <- renderPlotly({
        req(selected_region1())  # Ensure a borough is selected
     
        selected_data <- subset(crime_data_new, BoroughName == selected_region1())
        
        #dynamically change borough selected and store the data under crime_summary
        crime_summary <- aggregate(Total ~ MajorText, data = selected_data, sum)
        
        #check for unique crime categories for the selected borough
        crime_categories <- unique(crime_summary$MajorText)
        
        # Generate a color palette using RColorBrewer library
        colors <- rev(colorRampPalette(brewer.pal(11, "Set3"))(length(crime_categories)))  # 15 is the number of required colors
        
        #Map the Colours to the crime categories for the pie chart
        color_mapping <- setNames(colors, crime_categories)
        
        crime_summary$color <- color_mapping[crime_summary$MajorText]
      
        #plot the pie chart for borough selected
        plot <- plot_ly(
          labels = paste(
            crime_summary$MajorText #name of type of crime
          ),
          values = crime_summary$Total, #total number of crimes in the borough of a specific
          type = "pie",
          textinfo = "none",    # Remove static text on slices
          hovertemplate="%{label}<br>Total = %{value}<br>Percentage = %{percent}",  # Show details on hover
          marker = list(colors = crime_summary$color)  # Use the fixed color mapping
        ) %>%
          layout(
            title = list(
              text = paste("Crime Distribution in", selected_region1()),
              font = list(size = 20),  # Increase the title size
              xanchor = "center",  # Center-align the title
              yanchor = "top"
            ),
            showlegend = TRUE,  # Enable the legend
            legend = list(
              orientation = "h",  # Place the legend horizontally
              x = 0.5,  # Center-align the legend
              y = -0.2,  # Position below the chart
              xanchor = "center",  # Center-align the legend
              yanchor = "top"
            ),
            margin = list(t = 50, b = 150, l = 50, r = 50),  # Adjust margins for spacing
            height = 600,  # Adjust the chart's height
            width = 450  # Adjust the chart's width
            )
        
  
  
      })
        
      #update the map everytime a new borough is clicked
      leafletProxy("boroughMap") 

      # Reactive to calculate frequency-based bins for the selected house type
      bins_list <- reactive({
        req(input$house_type)  # Ensure input is available
        
        # Extract the selected column values
        selected_values <- standardised_pricing[[input$house_type]]
        
        # Calculate quantile-based breaks (frequency distribution bins)
        breaks <- quantile(selected_values, probs = seq(0, 1, length.out = 11), na.rm = TRUE)
    
        # Ensure breaks are unique
        unique_breaks <- unique(breaks)
        print(unique_breaks)
        # If there are not enough unique breaks, expand slightly to avoid issues
        if (length(unique_breaks) < 2) {
          range <- range(selected_values, na.rm = TRUE)
          unique_breaks <- seq(range[1], range[2], length.out = 11)
        }
        
        return(unique_breaks)
      })
      
      # Reactive function to create the color palette
      mypalette_Pricing <- reactive({
        req(input$house_type)  # Ensure input is available
        
        # Use the selected house type values and bins
        selected_values <- standardised_pricing[[input$house_type]]
        selected_bins <- bins_list()  # Get frequency-based bins
        
        colorBin(
          palette = rev(colorRampPalette(brewer.pal(10, "RdYlGn"))(length(selected_bins) - 1)),  # Choose a color scheme
          domain = selected_values,  # Use selected house type values
          bins = selected_bins, 
          na.color = "transparent"
        )
      })
    
  
    #pricing map default for room type : one bedroom
    output$Pricing <-renderLeaflet({
      leaflet(data = borough1) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%  # Use CartoDB tiles
        addPolygons(
          fillColor = ~mypalette_Pricing()(
            standardised_pricing[[input$house_type]][
              match(borough1$NAME, standardised_pricing$`pricing_data$\`Local authority\``)
            ]),
          color = "grey",
          highlightOptions = highlightOptions(
            weight = 5,
            opacity = 0.9,
            color = "yellow",
            bringToFront = TRUE),
          weight = 1,
          opacity = 1,
          fillOpacity = 0.7,
          label = ~borough1$NAME
          ) %>%
        setView(lng = -0.1276, lat = 51.5074, zoom = 10)
      
    })
    
    #Update the map dynamically
    observe({
      leafletProxy("Pricing", data = borough) %>%
        clearShapes() # Clear existing polygons
    })
    
    
    scale_breaks <- reactive({
      req(input$house_type)  # Ensure an option is selected
      
      # Example: Define custom break ranges for each house type
      switch(input$house_type,
             "One bedroom" = c(0, 800),  # Break for "One bedroom"
             "Two bedrooms" = c(0, 900),  # Break for "Two bedrooms"
             "Three bedrooms" = c(0, 1000),  # Break for "Three bedrooms"
             "Four bedrooms" = c(0, 1200),  # Break for "Four bedrooms"
             "Five bedrooms" = c(0, 1300),  # Break for "Five bedrooms"
             "Six bedrooms" = c(0, 1400),  # Break for "Five bedrooms"
             c(1000, 1100)  # Default break if no match
      )
    })
    output$PriceBarChart <- renderPlotly({
      req(input$house_type)  # Ensure an option is selected from the dropdown
      
      breaks <- scale_breaks()
      # Dynamically select the column based on the input
      selected_column <- averaged_pricing_data[[input$house_type]]
      
      # Create the ggplot bar chart with an axis break
      plot3 <- ggplot(averaged_pricing_data, aes(
        x = reorder(`pricing_data$\`Local authority\``, selected_column),  # Reorder boroughs
        y = selected_column,
        text = paste(
          "Borough:", `pricing_data$\`Local authority\``, "<br>",
          "Price:", round(selected_column, 2)  # Add hover text
        )
      )) +
        geom_bar(stat = "identity", fill = "steelblue" ) +
        coord_flip() +  # Horizontal bar chart
        scale_y_continuous(
          breaks = c(seq(0, breaks[1], by = 100), seq(breaks[2], max(selected_column), by = 300)),  # Custom tick positions
          labels = function(x) {
            ifelse(x > breaks[2], paste0(x, " "), x)  
          }
        ) + 
        labs(
          title = paste("House Prices for", input$house_type),
          x = "Borough",
          y = "Average Price (£)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, hjust = 0.5),  # Center and size title
          axis.text.y = element_text(size = 10),  # Adjust y-axis text size
          axis.text.x = element_text(size = 10),  # Adjust x-axis text size
          axis.title = element_text(size = 12),  # Adjust axis title size
          plot.margin = margin(t = 10, b = 10, l = 50, r = 20)  # Adjust plot margins
        )
      ggplotly(plot3, tooltip = "text") %>%
        layout(
          autosize = TRUE,  # Enable automatic resizing
          height = 600,     # Set plot height
          title = list(
            text = paste("House Prices for", input$house_type),  # Title text
            font = list(size = 20),  # Title font size
            x = 0.3,  # Center the title horizontally
            xanchor = "center"  # Anchor the title to the center
          ),
          margin = list(l = 50, r = 50, t = 80, b = 50)  # Increase top margin for the title
        )
    })

    
    #Prescriptive analysis
    #create reactive variables for optimiser input values

    W_a <- reactive({ #weight for amenities
      as.numeric(input$Amenitiestoggle)
    })
    
    W_c <- reactive({ #weight for crimes
      as.numeric(input$Crimetoggle)
    })
    
    P_min <- reactive({
      input$Range[1]  # Access the lower value of the range slider
    })
    
    P_max <- reactive({
      input$Range[2]  # Access the upper value of the range slider
    })
    
    chosen_amenity <- reactive({ #chosen amenity type
      input$AmenityTypes
    })
    
    least_wanted_crime <- reactive({ #chosen type of crime
      input$CrimeTypes
    })
    
    chosen_housetype <- reactive({ #chosen house type
      input$HouseTypes
    })
    
    #run the optimiser to give a value of 1 or 0 
    #1 means the borough is recommended
    #0 means its not eligible
    result <- reactive({
      optimise_preferences(
        crime_data_optimise,
        amenity_data_optimise,
        Restructure_pricing_data,
        W_a(),  # Use W_a() to access the numeric value
        W_c(),  # Use W_c() to access the numeric value
        P_min(),  # Use P_min() to access the numeric value
        P_max(),  # Use P_max() to access the numeric value
        chosen_amenity(),  # Access reactive input
        least_wanted_crime(),  # Access reactive input
        chosen_housetype()  # Access reactive input
      )
    })
    
    
    
    # Render the initial Leaflet map
    output$Prescriptive <- renderLeaflet({
      leaflet(data = borough1) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = "lightgrey",
          color = "grey",
          highlightOptions = highlightOptions(
            weight = 5,
            opacity = 0.9,
            color = "yellow",
            bringToFront = TRUE
          ),
          weight = 1,
          opacity = 1,
          fillOpacity = 0.1,
          group = "prescritive_base",
          label = ~borough1$NAME
        ) %>%
        
        setView(lng = -0.1276, lat = 51.5074, zoom = 10)
    })
    
    # Dynamically update borough colors based on optimiser output
    observe({
      req(result())  # Ensure result is available
      if (is.list(result()) && "optimal_boroughs" %in% names(result())) {

        # Extract the optimal borough names
        optimal_boroughs <- result()$optimal_borough$BoroughName

        # Update colors for boroughs
        leafletProxy("Prescriptive") %>%
          clearGroup(group = "recommendation") %>% # Clear the previous polygons
          addPolygons(
            data = borough1[borough1$NAME %in% optimal_boroughs, ],
            fillColor =  "lightblue",  # Highlight optimal boroughs
            color = "grey",
            highlightOptions = highlightOptions(
              weight = 5,
              opacity = 0.9,
              color = "yellow",
              bringToFront = TRUE
            ),
            weight = 1,
            opacity = 1,
            fillOpacity = 0.6,
            group = "recommendation",
            label = ~borough1$NAME
          )
      }

      #update the borough everytime the optimiser inputs change
      leafletProxy("Prescriptive")
      popupOptions()
    })
    
     
  }
  
  
  # Run the application
  shinyApp(ui = ui, server = server)
