  library(RColorBrewer)
  
  pricing_data <- readxl::read_xlsx("Pricing_Data.xlsx")
  
  

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
  
  # mybins_pricing <- c(0, 900, 1000, 1100, 1200, 1300, 1400, 1500, Inf)
  
  # Function to generate bins
  generate_bins <- function(start, n_bins, increment = 50) {
    bins <- seq(start, by = increment, length.out = n_bins)
    return(c(bins, Inf))  # Add Inf for the last bin
  }
  
  
  increments <- list(
    `One bedroom`= (max(averaged_pricing_data$`One bedroom`) - min(averaged_pricing_data$`One bedroom`))/9, 
    `Two bedrooms` = (max(averaged_pricing_data$`Two bedrooms`) - min(averaged_pricing_data$`Two bedrooms`))/9,
    `Three bedrooms` = (max(averaged_pricing_data$`Three bedrooms`) - min(averaged_pricing_data$`Three bedrooms`))/9,
    `Four bedrooms` = (max(averaged_pricing_data$`Four bedrooms`) - min(averaged_pricing_data$`Four bedrooms`))/9,
    `Five bedrooms` = (max(averaged_pricing_data$`Five bedrooms`) - min(averaged_pricing_data$`Five bedrooms`))/9,
    `Six bedrooms` = (max(averaged_pricing_data$`Six bedrooms`) - min(averaged_pricing_data$`Six bedrooms`))/9
  )
  # Generate bins for each house type
  bins_list <- list(
    `One bedroom` = generate_bins(start = min(averaged_pricing_data$`One bedroom`), n_bins = 9, increment = increments$`One bedroom`),
    `Two bedrooms` = generate_bins(start = min(averaged_pricing_data$`Two bedrooms`), n_bins = 9 , increment = increments$`Two bedrooms`),
    `Three bedrooms` = generate_bins(start = min(averaged_pricing_data$`Three bedrooms`), n_bins = 9 , increment = increments$`Three bedrooms`),
    `Four bedrooms` = generate_bins(start = min(averaged_pricing_data$`Four bedrooms`), n_bins = 9, increment = increments$`Four bedrooms`),
    `Five bedrooms` = generate_bins(start = min(averaged_pricing_data$`Five bedrooms`), n_bins = 9, increment = increments$`Five bedrooms`),
    `Six bedrooms` = generate_bins(start = min(averaged_pricing_data$`Six bedrooms`), n_bins = 9, increment = increments$`Six bedrooms`)
  )
  

  increments
  bins_list
  #pallete Pricing
  # mypalette_Pricing <- colorBin(
  #   palette = rev(brewer.pal(n = 8, name = "RdYlGn")), domain = averaged_pricing_data$`pricing_data$\`Local authority\``,
  #   na.color = "transparent", bin = mybins_pricing
  # )
  
  # p <- leaflet(data = borough) %>%
  #   addProviderTiles(providers$CartoDB.Positron) %>%  # Use CartoDB tiles
  #   addPolygons(
  #     fillColor = ~mypalette_Pricing(averaged_pricing_data$`One bedroom`[match(borough$NAME, averaged_pricing_data$`pricing_data$\`Local authority\``)]),
  #     color = "white",
  #     weight = 1,
  #     opacity = 1,
  #     fillOpacity = 0.9,
  #     label = ~borough$NAME
  #   ) %>%
  #   setView(lng = -0.1276, lat = 51.5074, zoom = 10)
  
  # p