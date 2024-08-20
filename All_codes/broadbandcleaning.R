library(tidyverse)

# Set working directory (adjust as needed)
setwd("/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data")

# Step 1: Load the broadband data and the cleaned Postcode to LSOA dataset
broadband_data <- read_csv("combined_broadband_speed_data.csv")
postcode_lsoa_cleaned <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Postcode_cleaned_LSOA.csv")

# Step 2: Select and rename important columns in broadband data
broadband_cleaned <- broadband_data %>%
  select(Postcode = postcode_space, 
         Maximum_Download_Speed = `Maximum download speed (Mbit/s)`, 
         Average_Download_Speed = `Average download speed (Mbit/s)`)

# Perform a right join with the cleaned Postcode_to_lsoa
broadband_lsoa_combined <- broadband_cleaned %>% 
  right_join(postcode_lsoa_cleaned, by = "Postcode")

# Step 4: Add a Partial Postcode column
broadband_lsoa_combined <- broadband_lsoa_combined %>%
  mutate(Partial_Postcode = substr(Postcode, 1, 5)) # Assuming 'Partial_Postcode' uses the first 5 characters

# Step 5: Select only the required columns
final_broadband_data <- broadband_lsoa_combined %>%
  select(Average_Download_Speed, 
         Maximum_Download_Speed, 
         Postcode, 
         Partial_Postcode, 
         Town, 
         District, 
         County)

# Step 6: Eliminate any rows with missing values
final_broadband_data <- final_broadband_data %>%
  na.omit()

output_path <- "/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Broadband_speed_dataset_cleaned.csv"
write_csv(final_broadband_data, output_path)

