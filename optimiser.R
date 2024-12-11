# source("AmenityMap.R")
source("CrimeTest.R")
source("PricingMap.R")


#Amenities
Amenities_Data_new <- read_excel("Amenities-Data.xls", sheet = "Cleaned_data")
# Delete the first two row using negative indexing
Amenities_Data_new <- Amenities_Data_new[-c(1, 2), ]


split_names_1 <- strsplit(Amenities_Data_new$Boroughs, ":")

df_new <- data.frame(
  First_Name1 = sapply(split_names_1, `[`, 1),  # Extract the first part
  Last_Name1 = sapply(split_names_1, `[`, 2)    # Extract the second part
)

df_new$Last_Name1 <- trimws(df_new$Last_Name1)
Amenities_Data_new$Boroughs <- df_new$Last_Name1
Restructure_amenities_data <- Amenities_Data_new%>%
  pivot_longer(
    cols = c("Licensed restaurants", "Unlicensed restaurants and cafes",
             "Take-away food shops and mobile food stands", 
             "Licensed clubs", "Public houses and bars"), 
    names_to = "AmenityType", 
    values_to = "TotalInBorough"
  )

head(Restructure_amenities_data)

amenity_data_optimise <- Restructure_amenities_data%>%
  group_by(AmenityType)%>%
  mutate(
    TotalAmenityType = sum(TotalInBorough),
    AmenityPercentage = (TotalInBorough/TotalAmenityType) #fraction of each borough
  )%>%
  ungroup()#removes grouping to return in normal format

amenity_data_optimise <- amenity_data_optimise %>%
  rename(BoroughName = Boroughs)

head(amenity_data_optimise)

#crime
crime_data_optimise <- summary_data%>%
  group_by(MajorText)%>%
  mutate(
    TotalCrimeType = sum(Total),
    CrimePercentage = (Total/TotalCrimeType) #fraction of each borough
    
  )%>%
  ungroup()#removes grouping to return in normal format

head(crime_data_optimise)
#pricing
Restructure_pricing_data <- pricing_max_min%>%
  pivot_longer(
    cols = c("One bedroom max", "One bedroom min", "Two bedrooms max","Two bedrooms min", 
             "Three bedrooms max", "Three bedrooms min",
             "Four bedrooms max", "Four bedrooms min", "Five bedrooms max","Five bedrooms min" , "Six bedrooms max", "Six bedrooms min"), 
    names_to = "HouseTypes", 
    values_to = "AverageInborough"
  )

Restructure_pricing_data <- Restructure_pricing_data %>%
  rename(BoroughName = `pricing_data$\`Local authority\``)

head(Restructure_pricing_data)



library(lpSolve)
library(dplyr)
optimise_preferences <- function(crimeData, AmenitiesData, pricingData, W_a, W_c, P_min, P_max, chosen_amenity, least_wanted_crime, chosen_housetype){

  filtered_amenities <- AmenitiesData %>%
    filter(AmenityType == chosen_amenity) %>%
    select(BoroughName, AmenityPercentage)

  filtered_crime <- crimeData %>%
    filter(MajorText == least_wanted_crime) %>%
    select(BoroughName, CrimePercentage)

  merged_data <- merge(filtered_amenities, filtered_crime, by = "BoroughName")

  
  m_a <- W_a/2
  m_c <- W_c/2
  # Calculate the objective score for amenities and crime
  merged_data <- merged_data %>%
    mutate(Score = m_a * AmenityPercentage - m_c * CrimePercentage)

  
  # Filter pricing data based on the chosen house type (max and min values)
  filtered_price <- pricingData %>%
    filter(HouseTypes == paste(chosen_housetype, "max") | HouseTypes == paste(chosen_housetype, "min")) %>%
    group_by(BoroughName) %>%
    summarize(
      MaxPrice = max(AverageInborough),
      MinPrice = min(AverageInborough)
    )

  merged_data <- merge(merged_data, filtered_price, by = "BoroughName")


  objective <- merged_data$Score
  
  # Create the constraint matrix for pricing
  constraint_matrix <- matrix(c(
    merged_data$MaxPrice,  # Upper price limit
    merged_data$MinPrice  # Lower price limit
  ), nrow = 2, byrow = TRUE)

  constraint_rhs <- c(P_min, P_max)

  constraint_dir <- c(">=", "<=")

  # Solve the linear programming problem
  result <- lp("max", objective, constraint_matrix, constraint_dir, constraint_rhs, all.bin = TRUE)
  
  # print(result$solution)
  # Check if an optimal solution was found
  if (result$status == 0) {
    optimal_boroughs <- merged_data[result$solution == 1, ]
    return(list(
      optimal_boroughs = optimal_boroughs,
      objective_value = result$objval
    ))
  } else {
    return("No feasible solution found!")
  }

}


# # Define parameters
# W_a <- 0.2  # Weight for amenities
# W_c <- 0.8  # Weight for crime
# P_min <- 900  # Minimum price
# P_max <- 950  # Maximum price
# chosen_amenity <- "Licensed restaurants"  # User's chosen amenity type
# least_wanted_crime <- "BURGLARY"  # User's least wanted crime type
# chosen_housetype <- "One bedroom"  # User's chosen house type
# 
# # Call the function
# result <- optimise_preferences(
#   crime_data_optimise,
#   amenity_data_optimise,
#   Restructure_pricing_data,
#   W_a,
#   W_c,
#   P_min,
#   P_max,
#   chosen_amenity,
#   least_wanted_crime,
#   chosen_housetype
# )
# 
# # View results
# if (is.list(result)) {
#   print("Optimal Boroughs and Scores:")
#   print(result$optimal_boroughs)  # Best boroughs
#   print(paste("Maximum Objective Value:", result$objective_value))  # Maximum score
# } else {
#   print(result)  # Error or no feasible solution message
# }
