library(tidyverse)
library(lubridate)

# Load data
cleaned_data_housing <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/Housepricingcombine.csv")

# Data cleaning and processing
cleaned_data <- cleaned_data_housing %>%
  as_tibble() %>%  
  na.omit() %>%   
  mutate(
    Price = as.numeric(Price),  
    `Date of Transfer` = ymd(Date), 
    `Date of Transfer` = year(`Date of Transfer`),  
    S_No = row_number()  
  ) %>%
  filter(County %in% c("CITY OF BRISTOL", "CORNWALL")) %>%   
  select(S_No, Price, `Date of Transfer`, Postcode, Town, District, County) %>%  # Select required columns
  mutate(
    `Partial_Postcode` = substr(Postcode, 1, 5) 
  ) %>%
  distinct()  

housing_path ="/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/House_rate_cleaned.csv"

# Save the cleaned dataset
write.csv(cleaned_data, housing_path, row.names = FALSE)

# View cleaned data structure and summary
str(cleaned_data)
View(cleaned_data)
summary(cleaned_data)
