library(tidyverse)
library(readr)
library(dplyr)

# Step 1: Load the postcode dataset and house price
postcode_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/Postcode_to_LSOA.csv")
house_price_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/House_rate_cleaned.csv")

# Step 2: Add a serial number column for distinct identification
postcode_data <- postcode_data %>%
  mutate(S_No = row_number())

postcode_cleaned <- postcode_data %>%
  select(S_No, Postcode = pcds, LSOA_Code = lsoa11cd)

house_price_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/House_rate_cleaned.csv")

# Perform a right join with the house price data on the Postcode column
combined_data <- house_price_data %>%
  right_join(postcode_cleaned, by = "Postcode")

# Select only the needed columns
final_data <- combined_data %>%
  select(LSOA_Code, Postcode, `Partial_Postcode`, `Town`, District, County)

# Filter the final dataset for "CITY OF BRISTOL" and "CORNWALL"
filtered_data <- final_data %>%
  filter(County %in% c("CITY OF BRISTOL", "CORNWALL"))

output_path <- "/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Postcode_cleaned_LSOA.csv"
write_csv(filtered_data, output_path)
cat("Cleaned and combined dataset saved to:", output_path, "\n")
