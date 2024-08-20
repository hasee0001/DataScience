# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)

# Load the datasets
house_price_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/House_rate_cleaned.csv")
broadband_speed_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Broadband_speed_dataset_cleaned.csv")
school_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/School_Dataset_Cleaned.csv")
crime_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Cleaned_Crime_Data.csv")

# Filter the data to include only Bristol
house_price_data_bristol <- house_price_data %>% filter(County == "CITY OF BRISTOL")
broadband_speed_data_bristol <- broadband_speed_data %>% filter(County == "CITY OF BRISTOL")
school_data_bristol <- school_data %>% filter(County == "CITY OF BRISTOL")
crime_data_bristol <- crime_data %>% filter(County == "CITY OF BRISTOL")

# Normalize each factor using Min-Max Scaling for Bristol
house_price_data_bristol$HouseScore <- (house_price_data_bristol$Price - min(house_price_data_bristol$Price, na.rm = TRUE)) / 
  (max(house_price_data_bristol$Price, na.rm = TRUE) - min(house_price_data_bristol$Price, na.rm = TRUE))

broadband_speed_data_bristol$BroadbandScore <- (broadband_speed_data_bristol$Average_Download_Speed - min(broadband_speed_data_bristol$Average_Download_Speed, na.rm = TRUE)) / 
  (max(broadband_speed_data_bristol$Average_Download_Speed, na.rm = TRUE) - min(broadband_speed_data_bristol$Average_Download_Speed, na.rm = TRUE))

school_data_bristol$SchoolScore <- (school_data_bristol$`Attainment Score` - min(school_data_bristol$`Attainment Score`, na.rm = TRUE)) / 
  (max(school_data_bristol$`Attainment Score`, na.rm = TRUE) - min(school_data_bristol$`Attainment Score`, na.rm = TRUE))

# Aggregate crime data by Partial_Postcode and calculate CrimeScore for Bristol
crime_summary_bristol <- crime_data_bristol %>%
  group_by(Partial_Postcode) %>%
  summarize(Crime_Count = n(), .groups = 'drop')

crime_summary_bristol$CrimeScore <- (max(crime_summary_bristol$Crime_Count, na.rm = TRUE) - crime_summary_bristol$Crime_Count) / 
  (max(crime_summary_bristol$Crime_Count, na.rm = TRUE) - min(crime_summary_bristol$Crime_Count, na.rm = TRUE))

# Weighted Scoring: Assign weights to each factor (adjust as needed based on client preferences)
house_weight <- 0.25
broadband_weight <- 0.25
school_weight <- 0.25
crime_weight <- 0.25

# Combine all scores into one dataframe using the Partial_Postcode as the key
combined_scores_bristol <- house_price_data_bristol %>%
  select(Partial_Postcode, Town, HouseScore) %>%
  left_join(broadband_speed_data_bristol %>% select(Partial_Postcode, BroadbandScore), by = "Partial_Postcode") %>%
  left_join(school_data_bristol %>% select(Partial_Postcode, SchoolScore), by = "Partial_Postcode") %>%
  left_join(crime_summary_bristol %>% select(Partial_Postcode, CrimeScore), by = "Partial_Postcode") %>%
  mutate(Overall_Score = house_weight * HouseScore + broadband_weight * BroadbandScore +
           school_weight * SchoolScore + crime_weight * CrimeScore)

# Filter to remove any rows with missing values in key factors
combined_scores_bristol <- combined_scores_bristol %>%
  filter(!is.na(HouseScore) & !is.na(BroadbandScore) & !is.na(SchoolScore) & !is.na(CrimeScore))

# Select the top 10 unique partial postcodes in Bristol based on the Overall Score
final_scores_bristol <- combined_scores_bristol %>%
  distinct(Partial_Postcode, .keep_all = TRUE) %>%
  slice(1:10)

# Print the final top 10 partial postcodes
print(final_scores_bristol)

# Create a stacked bar chart showing contributions of different factors to the Overall Score for Bristol
ggplot(final_scores_bristol, aes(x = reorder(Partial_Postcode, -Overall_Score), y = Overall_Score, fill = "Overall Score")) +
  geom_bar(aes(y = HouseScore, fill = "House Prices"), stat = "identity") +
  geom_bar(aes(y = BroadbandScore, fill = "Broadband Speed"), stat = "identity", position = "stack") +
  geom_bar(aes(y = SchoolScore, fill = "School Performance"), stat = "identity", position = "stack") +
  geom_bar(aes(y = CrimeScore, fill = "Crime Rate"), stat = "identity", position = "stack") +
  geom_text(aes(label = round(Overall_Score, 2), y = Overall_Score + 0.05), 
            position = position_dodge(width = 0.9), vjust = -0.25) +
  theme_minimal() +
  labs(title = "Top 10 Unique Locations in Bristol with Factor Contributions to Overall Score",
       x = "Partial Postcode",
       y = "Score",
       fill = "Factors") +
  coord_flip()
