# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)

# Load the datasets
house_price_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/House_rate_cleaned.csv")
broadband_speed_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Broadband_speed_dataset_cleaned.csv")
school_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/School_Dataset_Cleaned.csv")
crime_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Cleaned_Crime_Data.csv")

# Filter the data to include only the year 2023 for Bristol
house_price_data_bristol_2023 <- house_price_data %>% filter(County == "CITY OF BRISTOL", format(as.Date(`Date of Transfer`), "%Y") == "2023")
broadband_speed_data_bristol_2023 <- broadband_speed_data %>% filter(County == "CITY OF BRISTOL")
school_data_bristol_2023 <- school_data %>% filter(County == "CITY OF BRISTOL", Year == 2023)
crime_data_bristol_2023 <- crime_data %>% filter(County == "CITY OF BRISTOL", format(as.Date(Crime_Date), "%Y") == "2023")

# Normalize each factor using Min-Max Scaling for 2023
house_price_data_bristol_2023$HouseScore <- (house_price_data_bristol_2023$Price - min(house_price_data_bristol_2023$Price, na.rm = TRUE)) / 
  (max(house_price_data_bristol_2023$Price, na.rm = TRUE) - min(house_price_data_bristol_2023$Price, na.rm = TRUE))

broadband_speed_data_bristol_2023$BroadbandScore <- (broadband_speed_data_bristol_2023$Average_Download_Speed - min(broadband_speed_data_bristol_2023$Average_Download_Speed, na.rm = TRUE)) / 
  (max(broadband_speed_data_bristol_2023$Average_Download_Speed, na.rm = TRUE) - min(broadband_speed_data_bristol_2023$Average_Download_Speed, na.rm = TRUE))

school_data_bristol_2023$SchoolScore <- (school_data_bristol_2023$`Attainment Score` - min(school_data_bristol_2023$`Attainment Score`, na.rm = TRUE)) / 
  (max(school_data_bristol_2023$`Attainment Score`, na.rm = TRUE) - min(school_data_bristol_2023$`Attainment Score`, na.rm = TRUE))

# Aggregate crime data by Partial_Postcode and calculate CrimeScore for 2023 in Bristol
crime_summary_bristol_2023 <- crime_data_bristol_2023 %>%
  group_by(Partial_Postcode) %>%
  summarize(Crime_Count = n(), .groups = 'drop')

crime_summary_bristol_2023$CrimeScore <- (max(crime_summary_bristol_2023$Crime_Count, na.rm = TRUE) - crime_summary_bristol_2023$Crime_Count) / 
  (max(crime_summary_bristol_2023$Crime_Count, na.rm = TRUE) - min(crime_summary_bristol_2023$Crime_Count, na.rm = TRUE))

# Combine all scores into one dataframe using the Partial_Postcode as the key
combined_scores_bristol_2023 <- house_price_data_bristol_2023 %>%
  select(Partial_Postcode, Town, HouseScore) %>%
  left_join(broadband_speed_data_bristol_2023 %>% select(Partial_Postcode, BroadbandScore), by = "Partial_Postcode") %>%
  left_join(school_data_bristol_2023 %>% select(Partial_Postcode, SchoolScore), by = "Partial_Postcode") %>%
  left_join(crime_summary_bristol_2023 %>% select(Partial_Postcode, CrimeScore), by = "Partial_Postcode") %>%
  mutate(Overall_Score = rowMeans(select(., HouseScore, BroadbandScore, SchoolScore, CrimeScore), na.rm = TRUE))

# Filter to remove any rows with missing values in key factors
combined_scores_bristol_2023 <- combined_scores_bristol_2023 %>%
  filter(!is.na(HouseScore) & !is.na(BroadbandScore) & !is.na(SchoolScore) & !is.na(CrimeScore))

# Remove duplicates by selecting distinct rows based on Partial_Postcode and select top 10
final_unique_towns_bristol_2023 <- combined_scores_bristol_2023 %>%
  distinct(Partial_Postcode, .keep_all = TRUE) %>%
  slice(1:10)

# Print the final top 10 partial postcodes
print(final_unique_towns_bristol_2023)

# Create the stacked bar chart showing contributions of different factors to the Overall Score for Bristol in 2023
ggplot(final_unique_towns_bristol_2023, aes(x = reorder(Partial_Postcode, -Overall_Score), y = Overall_Score, fill = "Overall Score")) +
  geom_bar(aes(y = HouseScore, fill = "House Prices"), stat = "identity") +
  geom_bar(aes(y = BroadbandScore, fill = "Broadband Speed"), stat = "identity", position = "stack") +
  geom_bar(aes(y = SchoolScore, fill = "School Performance"), stat = "identity", position = "stack") +
  geom_bar(aes(y = CrimeScore, fill = "Crime Rate"), stat = "identity", position = "stack") +
  geom_text(aes(label = round(Overall_Score, 2), y = Overall_Score + 0.05), 
            position = position_dodge(width = 0.9), vjust = -0.25) +
  theme_minimal() +
  labs(title = "Top 10 Unique Partial Postcodes in Bristol with Factor Contributions to Overall Score (2023)",
       x = "Partial Postcode",
       y = "Score",
       fill = "Factors") +
  coord_flip()
