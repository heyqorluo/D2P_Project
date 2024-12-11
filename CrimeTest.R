library(dplyr)
library(sf)
library(RColorBrewer)
library(leaflet)
library(ggplot2)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
jscode <- readLines(system.file("js/uglify.min.js", package="js"), warn = FALSE)

# Borough Data for Oct 2023-Oct 2024
crime_data <- read.csv("Crime-Data-Borough.csv")
# print(crime_data)

crime_data$Total <- rowSums(crime_data[,15:27]) 

sums <- tapply(crime_data$Total, crime_data$BoroughName, sum)
# print(sums)
df <- data.frame(
  Borough = names(sums),
  Total = as.numeric(sums)
)


summary_data <- crime_data %>%
  group_by( BoroughName,MajorText) %>%
  summarise(across(sum(na.rm = TRUE)), 
            Total = sum(Total, na.rm = TRUE),
            .groups = "drop")


# plot <- plot_ly(
#   data = summary_data,
#   labels = ~MajorText,  # Labels for the pie slices
#   values = ~Total,      # Values for the pie slices
#   type = 'pie',
#   textinfo = 'label+percent',  # Show labels and percentages
#   hoverinfo = 'label+value'    # Show labels and values on hover
# ) %>%
#   layout(
#     title = paste("Crime Distribution in London"),
#     showlegend = FALSE# Display a legend
#   )
# 
# plot
# print(summary_data)
# 
# 
# write.csv(summary_data, "crime-data-sorted.csv")
#LSOA Data for Oct 2023-Oct 2024
crime_data_LSOA <- read.csv("Crime-Data-LSOA.csv")

crime_data_LSOA$Total <- rowSums(crime_data_LSOA[,15:27])

sums2 <- tapply(crime_data_LSOA$Total, crime_data_LSOA$LSOA.Name, sum)

df2 <- data.frame(
  LSOA = names(sums2),
  Total = as.numeric(sums2)
)



#summary(df2)
#barplot(df2$Total, col = "blue", names.arg = df2$LSOA ,xlab = "LSOA Name", ylab = "Sums")
# print(df2)

#colourpallete
mybins_borough <- c(0, 5000, 10000, 20000, 30000, 40000, 50000, 60000, Inf)
mybins_lsoa <- c(0, 100, 200, 300, 400, 500, 600, 700, Inf)
borough1 <- st_read("boundary.geojson")
lsoa <- st_read("lsoa.geojson")

#pallete boroughs
mypalette_borough <- colorBin(
  palette = rev(brewer.pal(n = 8, name = "RdYlGn")), domain = borough1$NAME,
  na.color = "transparent", bin = mybins_borough
)

#pallete lsoa
mypalette_lsoa <- colorBin(
  palette = rev(brewer.pal(n = 8, name = "RdYlGn")), domain = borough1$NAME,
  na.color = "transparent", bin = mybins_lsoa
)



m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  # Use CartoDB tiles
  #borough layer
  addPolygons(
    data = borough1,
    fillColor = ~mypalette_borough(df$Total[match(borough1$NAME, df$Borough)]),
    group = "Boroughs",
    stroke = TRUE,
    fillOpacity = 0.7,
    color = "grey",
    highlightOptions = highlightOptions(
      weight = 5,
      opacity = 0.9,
      color = "yellow",
      bringToFront = TRUE),
    weight = 1,
    opacity = 1,
    label = ~NAME,
    layerId = ~NAME
  ) %>%

  # #lsoa layer
  # addPolygons(
  #   data = lsoa,
  #   fillColor = ~mypalette_lsoa(df2$Total[match(lsoa$LSOA11NM, df2$LSOA)]),
  #   group = "LSOAs",
  #   fillOpacity = 0.9,
  #   color = "white",
  #   weight = 1,
  #   opacity = 1,
  #   label = ~LSOA11NM
  # ) %>%

  # addLayersControl(
  #   overlayGroups = c("Boroughs", "LSOAs"),
  #   options = layersControlOptions(collapsed = FALSE),
  # ) %>%

  setView(lng = -0.1276, lat = 51.5074, zoom = 10)

m


