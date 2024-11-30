  # Load necessary libraries
  library(sf)
  library(dplyr)
  library(shiny)
  library(leaflet)
  library(bslib)
  library(ggplot2)
  library(plotly)
  library(RColorBrewer)
  library(viridis)
  
  
  # print(summary_data)
  source("CrimeTest.R")
  source("PricingMap.R")
  source("AmenityMap.R")
  
  # read data
  pricing_data <- readxl::read_xlsx("Pricing_Data.xlsx")
  borough <-  st_read("london-boroughs_1179.geojson")
  lsoa <-  st_read("lsoa.geojson")
  borough1 <- st_read("boundary.geojson")
  
  #averaging pricing data across the various electoral wards in the borough
  averaged_pricing_data <- pricing_data %>%
    group_by(pricing_data$`Local authority`)%>%
    summarise(
      `One bedroom` = mean(`One bedroom`, na.rm = TRUE),
      `Two bedrooms` = mean(`Two bedrooms` , na.rm = TRUE),
      `Three bedrooms` = mean(`Three bedrooms`, na.rm = TRUE),
      `Four bedrooms` = mean(`Four bedrooms` , na.rm = TRUE),
      `Five bedrooms` = mean(`Five bedrooms`, na.rm = TRUE),
      `Six bedrooms` = mean(`Six bedrooms` , na.rm = TRUE)
    )
  
  # print(averaged_pricing_data)
  
  mybins_pricing <- c(0, 800, 900, 1000, 1050, 1100, 1150, 1300, Inf)
  
  # Define UI for the app
  ui <- tagList(
    # Add custom CSS to adjust the Title and Tabs position
    tags$style(HTML("
    .navbar-default .navbar-nav {
      margin-top: 50px; /* Add space between title and tabs */
    }
    .navbar-brand {
      display: block;
      text-align: center; /* Center the title if needed */
    }
    .navbar { 
      border-bottom: 2px solid #ddd; /* Optional: Add a border below the navbar */
    }
  ")),
    
    navbarPage(
      title = div(
        style = "text-align: center; font-size: 24px; font-weight: bold;",
        "Renting in London"
      ),
    
    # First Tab
    tabPanel(
      title = "London Crime Data",
      sidebarLayout(
        sidebarPanel(
          # sliderInput("crimes", "Number of Crimes per Area:", min = 1, max = 10, value = 5),
          fluidRow(
            column(
              width= 12,
              plotlyOutput("crimePieChart", height = "600px")
            )
          )
        ),
        mainPanel(
          fluidRow(
            column(
              width = 12,
              leafletOutput("boroughMap", width = "100%", height = "600px")
            )
          )
        ),
        position = "right"
      )
    ),
    
    # Second Tab
    tabPanel(
      title = "London House Pricing Map",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "house_type", 
            "Select House Type:", 
            choices = c(
              "One bedroom", "Two bedrooms", 
              "Three bedrooms", "Four bedrooms", 
              "Five bedrooms", "Six bedrooms"
            ),
            selected = "One bedroom"
          ), 
          plotlyOutput("PriceBarChart", height = "600px")
        ),
        mainPanel(
          leafletOutput("Pricing", width = "100%", height = "600px")
        ),
        position = "right"
      )
    ),
    
    # Third Tab
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
    )
  ))

  

  
  # Define server logic for the app
  server <- function(input, output, session) {

    # IMPORTANT: kills the process when closing the app
    session$onSessionEnded(function() { stopApp() })
    
    thematic::thematic_shiny()
    
    # Show the welcome dialog when the app starts
    showModal(modalDialog(
      title = HTML('Welcome to    <img src="logo.png" style="height: 50px; width: auto;"/> Find you Flat'),
      HTML(" 
    Discover the ideal place to rent in London with our interactive dashboard! <br><br>
    Explore crime data across boroughs, access detailed amenities maps for grocery stores, restaurants, and more.
           Finally, let us recommend the perfect area tailored to your needs. Use the tabs to start your journey to smarter renting!<br><br> 
           By pressing OK, you agree to this."),
      
      easyClose = TRUE, # When true, clicking outside the popup will close it
      # Add an 'OK' button to the modal dialog, when clicked the popup is closed
      footer = modalButton("Accept. Let's go!", icon = icon("thumbs-up"))
    ))
    
    selected_region1 <- reactiveVal(NULL) # Store the selected region
    
    
    
    
    
    # Amenity Code:
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
    

    
    
    
    
    
  
    # Crime Code:
    crime_data_new <- read.csv("crime-data-sorted.csv")
    
    # Render the leaflet map
    output$boroughMap <- renderLeaflet({
        m
    })
    
    
    
    observeEvent(input$boroughMap_shape_click, {
      click <- input$boroughMap_shape_click
      clicked_borough <- click$id
      selected_region1(clicked_borough) 
    
    })   
      output$crimePieChart <- renderPlotly({
        req(selected_region1())  # Ensure a borough is selected
     
        selected_data <- subset(crime_data_new, BoroughName == selected_region1())
        
        crime_summary <- aggregate(Total ~ MajorText, data = selected_data, sum)
        crime_categories <- unique(crime_summary$MajorText)
        # Generate a color palette using RColorBrewer
        colors <- rev(colorRampPalette(brewer.pal(11, "RdYlGn"))(length(crime_categories)))  # 15 is the number of required colors
        # colors <- viridis(length(crime_categories), option = "C")  # Choose the 'D' option for the palette
        color_mapping <- setNames(colors, crime_categories)
        
        crime_summary$color <- color_mapping[crime_summary$MajorText]
      
        
        plot <- plot_ly(
          labels = paste(
            crime_summary$MajorText,
            paste0(round(100 * crime_summary$Total / sum(crime_summary$Total), 1), "%")
          ),  # Include crime type and percentage as labels
          values = crime_summary$Total,
          type = "pie",
          textinfo = "none",    # Remove static text on slices
          hoverinfo = "label+value+percent",  # Show details on hover
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
        
      leafletProxy("boroughMap") 
        # clearPopups() %>%
        # addPopups(lng = click$lng, lat = click$lat, popup = mytext)
  
    
      
      mypalette_Pricing <- reactive({
        req(input$house_type)
        selected_bins <- bins_list[[input$house_type]]
        selected_values <-averaged_pricing_data[[input$house_type]]
        colorBin(
          palette = rev(colorRampPalette(brewer.pal(9, "RdYlGn"))(length(selected_bins) - 1)),  # Choose a color scheme
          domain = selected_values,  # Dynamically select column
          bins = selected_bins, 
          na.color = "transparent"
        )
      })  
    
  

    output$Pricing <-renderLeaflet({
      leaflet(data = borough1) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%  # Use CartoDB tiles
        addPolygons(
          fillColor = ~mypalette_Pricing()(
            averaged_pricing_data[[input$house_type]][
              match(borough1$NAME, averaged_pricing_data$`pricing_data$\`Local authority\``)
            ]),
          color = "grey",
          highlightOptions = highlightOptions(
            weight = 5,
            opacity = 0.9,
            color = "yellow",
            bringToFront = TRUE),
          weight = 1,
          opacity = 1,
          fillOpacity = 0.9,
          label = ~borough1$NAME
          ) %>%
        setView(lng = -0.1276, lat = 51.5074, zoom = 10)
      
    })
    
    # Use leafletProxy to update the map dynamically
    observe({
      leafletProxy("Pricing", data = borough) %>%
        clearShapes() # Clear existing polygons
    })
    
    output$PriceBarChart <- renderPlotly({
      req(input$house_type) #ensure an option is selected from the select input
      
      selected_column <- averaged_pricing_data[[input$house_type]]  # Dynamically selected column name
      plot2 <- plot_ly(
        data = averaged_pricing_data,
        x = ~selected_column,  # Dynamically access the selected column
        y = ~averaged_pricing_data$`pricing_data$\`Local authority\``,  # Dynamically reorder boroughs
        type = "bar",
        orientation = "h",  # Make bars horizontal
        text = ~paste0(
          averaged_pricing_data$`pricing_data$\`Local authority\``, ": £", round(selected_column, 2)
        ),  # Tooltip text
        hoverinfo = "text",  # Show tooltip details
        marker = list(color = "skyblue"),  # Use a fixed color
        height = 600,  # Specify height directly
        width = 450   # Specify width directly
      ) %>%
        layout(
          title = list(
            text = paste("House Prices for", input$house_type),
            font = list(size = 20),
            xanchor = "center",
            yanchor = "top"
          ),
          xaxis = list(title = "Average Price (£)"),  # Update x-axis label
          yaxis = list(title = "Borough"),  # Update y-axis label
          margin = list(t = 50, b = 150, l = 150, r = 50)  # Adjust margins for spacing
        )
      
      plot2
      
    })
  }
  
  
  # Run the application
  shinyApp(ui = ui, server = server)
