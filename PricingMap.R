  library(RColorBrewer)
  library(dplyr)
  
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
  
  # write.csv(averaged_pricing_data, "average_borough_pricing.csv")
  #noramlise pricing = (x - xmin)/(xmax-xmin)
  # normalise <- function(x) {
  #   return((x - min(x)) / (max(x) - min(x)))
  # }
  # 
  # # Apply the normalisation to each column according to house type
  # 
  # print(averaged_pricing_data[,-1])
  # normalised_pricing <- as.data.frame(lapply(averaged_pricing_data[,-1],normalise))
  # 
  # # Add the Local authority (boroughs) column back accroding to house type
  # normalised_pricing <- cbind(averaged_pricing_data$`pricing_data$\`Local authority\``, normalised_pricing)
  # 
  # head(normalised_pricing)
  # # Rename columns for clarity
  # colnames(normalised_pricing)[1] <- "pricing_data$`Local authority`"
  # colnames(normalised_pricing)[2] <- "One bedroom"
  # colnames(normalised_pricing)[3] <- "Two bedrooms"
  # colnames(normalised_pricing)[4] <- "Three bedrooms"
  # colnames(normalised_pricing)[5] <- "Four bedrooms"
  # colnames(normalised_pricing)[6] <- "Five bedrooms"
  # colnames(normalised_pricing)[7] <- "Six bedrooms"
  
  # Standardization function
  standardise <- function(x) {
    return((x - mean(x)) / sd(x))
  }
  
  # Apply the standardization to each column according to house type
  standardised_pricing <- as.data.frame(lapply(averaged_pricing_data[,-1], standardise))
  
  # Add the Local authority (boroughs) column back according to house type
  standardised_pricing <- cbind(averaged_pricing_data$`pricing_data$\`Local authority\``, standardised_pricing)
  
  # Rename columns for clarity
  colnames(standardised_pricing)[1] <- "pricing_data$`Local authority`"
  colnames(standardised_pricing)[2] <- "One bedroom"
  colnames(standardised_pricing)[3] <- "Two bedrooms"
  colnames(standardised_pricing)[4] <- "Three bedrooms"
  colnames(standardised_pricing)[5] <- "Four bedrooms"
  colnames(standardised_pricing)[6] <- "Five bedrooms"
  colnames(standardised_pricing)[7] <- "Six bedrooms"
  
  # head(standardised_pricing)
  
  # Function to generate bins
  generate_bins <- function(start, n_bins, increment = 0.1) {
    bins <- seq(start, by = increment, length.out = n_bins)
    return(c(bins, Inf))  # Add Inf for the last bin
  }
  

  # increments <- list(
  #   `One bedroom`= (max(averaged_pricing_data$`One bedroom`) - min(averaged_pricing_data$`One bedroom`))/9,
  #   `Two bedrooms` = (max(averaged_pricing_data$`Two bedrooms`) - min(averaged_pricing_data$`Two bedrooms`))/9,
  #   `Three bedrooms` = (max(averaged_pricing_data$`Three bedrooms`) - min(averaged_pricing_data$`Three bedrooms`))/9,
  #   `Four bedrooms` = (max(averaged_pricing_data$`Four bedrooms`) - min(averaged_pricing_data$`Four bedrooms`))/9,
  #   `Five bedrooms` = (max(averaged_pricing_data$`Five bedrooms`) - min(averaged_pricing_data$`Five bedrooms`))/9,
  #   `Six bedrooms` = (max(averaged_pricing_data$`Six bedrooms`) - min(averaged_pricing_data$`Six bedrooms`))/9
  # )
  #Generate bins for each house type
  # bins_list <- list(
  #   `One bedroom` = generate_bins(start = min(standardised_pricing$`One bedroom`), n_bins = 9),
  #   `Two bedrooms` = generate_bins(start = min(standardised_pricing$`Two bedrooms`), n_bins = 9),
  #   `Three bedrooms` = generate_bins(start = min(standardised_pricing$`Three bedrooms`), n_bins = 9),
  #   `Four bedrooms` = generate_bins(start = min(standardised_pricing$`Four bedrooms`), n_bins = 9),
  #   `Five bedrooms` = generate_bins(start = min(standardised_pricing$`Five bedrooms`), n_bins = 9),
  #   `Six bedrooms` = generate_bins(start = min(standardised_pricing$`Six bedrooms`), n_bins = 9)
  # )
  
  # bins_list <- generate_bins(start = 0, n_bins = 10, increment = 0.1)
  
  # barplot(standardised_pricing$`Six bedrooms`,
  #         main = "Bar Graph of One Bedroom Prices",
  #         xlab = "Index",
  #         ylab = "Standardised Price",
  #         col = "blue",
  #         names.arg = NULL) # Set names.arg if you want custom x-axis labels
  # 
  
  hist(standardised_pricing$`Six bedrooms`)
  
  # Calculate bins based on histogram of standardized data
  # bins_list <- hist(unlist(standardised_pricing[,-1]), breaks = 10, plot = FALSE)$breaks
  # bins_list

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