# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)

# Load the datasets
house_price_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/House_rate_cleaned.csv")
broadband_speed_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Broadband_speed_dataset_cleaned.csv")
school_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/School_Dataset_Cleaned.csv")
crime_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Cleaned_Crime_Data.csv")

# Filter the data to include only 2023 and Cornwall
house_price_data <- house_price_data %>% filter(County == "CORNWALL")
broadband_speed_data <- broadband_speed_data %>% filter(County == "CORNWALL")
school_data <- school_data %>% filter(County == "CORNWALL" & Year == 2023)
crime_data <- crime_data %>% filter(County == "CORNWALL" & format(Crime_Date, "%Y") == "2023")

# Normalize each factor using Min-Max Scaling
house_price_data$HouseScore <- (house_price_data$Price - min(house_price_data$Price, na.rm = TRUE)) / 
  (max(house_price_data$Price, na.rm = TRUE) - min(house_price_data$Price, na.rm = TRUE))

broadband_speed_data$BroadbandScore <- (broadband_speed_data$Average_Download_Speed - min(broadband_speed_data$Average_Download_Speed, na.rm = TRUE)) / 
  (max(broadband_speed_data$Average_Download_Speed, na.rm = TRUE) - min(broadband_speed_data$Average_Download_Speed, na.rm = TRUE))

school_data$SchoolScore <- (school_data$`Attainment Score` - min(school_data$`Attainment Score`, na.rm = TRUE)) / 
  (max(school_data$`Attainment Score`, na.rm = TRUE) - min(school_data$`Attainment Score`, na.rm = TRUE))

# Aggregate crime data by Partial_Postcode and calculate CrimeScore
crime_summary <- crime_data %>%
  group_by(Partial_Postcode) %>%
  summarize(Crime_Count = n(), .groups = 'drop')

crime_summary$CrimeScore <- (max(crime_summary$Crime_Count, na.rm = TRUE) - crime_summary$Crime_Count) / 
  (max(crime_summary$Crime_Count, na.rm = TRUE) - min(crime_summary$Crime_Count, na.rm = TRUE))

# Weighted Scoring: Assign weights to each factor (adjust as needed based on client preferences)
house_weight <- 0.25
broadband_weight <- 0.25
school_weight <- 0.25
crime_weight <- 0.25

# Combine all scores into one dataframe using the Partial_Postcode as the key
combined_scores <- house_price_data %>%
  select(Partial_Postcode, Town, HouseScore) %>%
  left_join(broadband_speed_data %>% select(Partial_Postcode, BroadbandScore), by = "Partial_Postcode") %>%
  left_join(school_data %>% select(Partial_Postcode, SchoolScore), by = "Partial_Postcode") %>%
  left_join(crime_summary %>% select(Partial_Postcode, CrimeScore), by = "Partial_Postcode") %>%
  mutate(Overall_Score = house_weight * HouseScore + broadband_weight * BroadbandScore +
           school_weight * SchoolScore + crime_weight * CrimeScore)

# Filter to remove any rows with missing values in key factors
combined_scores <- combined_scores %>%
  filter(!is.na(HouseScore) & !is.na(BroadbandScore) & !is.na(SchoolScore) & !is.na(CrimeScore))

# Remove duplicates by selecting distinct rows based on Town
final_unique_towns <- combined_scores %>%
  distinct(Town, .keep_all = TRUE) %>%
  slice(1:10)  # Ensure we get 10 unique towns

# Print the final top 10 towns
print(final_unique_towns)

# Create a stacked bar chart showing contributions of different factors to the Overall Score for 2023
ggplot(final_unique_towns, aes(x = reorder(Town, -Overall_Score), y = Overall_Score, fill = "Overall Score")) +
  geom_bar(aes(y = HouseScore, fill = "House Prices"), stat = "identity") +
  geom_bar(aes(y = BroadbandScore, fill = "Broadband Speed"), stat = "identity", position = "stack") +
  geom_bar(aes(y = SchoolScore, fill = "School Performance"), stat = "identity", position = "stack") +
  geom_bar(aes(y = CrimeScore, fill = "Crime Rate"), stat = "identity", position = "stack") +
  geom_text(aes(label = round(Overall_Score, 2), y = Overall_Score + 0.05), 
            position = position_dodge(width = 0.9), vjust = -0.25) +
  theme_minimal() +
  labs(title = "Top 10 Towns in Cornwall with Factor Contributions to Overall Score (2023)",
       x = "Town/City",
       y = "Score",
       fill = "Factors") +
  coord_flip()
