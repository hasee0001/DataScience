# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(scales)

# Load the datasets
broadband_speed_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Broadband_speed_dataset_cleaned.csv")
crime_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Cleaned_Crime_Data.csv")
population_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Cleaned_Population_Data.csv")

# Step 1: Filter crime data for drug-related offenses
drug_offense_data <- crime_data %>%
  filter(grepl("Drugs", Crime_Type, ignore.case = TRUE))

# Aggregate drug offenses by Partial_Postcode
drug_offense_counts <- drug_offense_data %>%
  group_by(Partial_Postcode) %>%
  summarize(Drug_Offenses_Count = n())

# Aggregate broadband data by Partial_Postcode to avoid duplication
aggregated_broadband_data <- broadband_speed_data %>%
  group_by(Partial_Postcode) %>%
  summarize(Average_Download_Speed = mean(Average_Download_Speed))

# Step 2: Merge population data with drug offense counts to calculate drug offense rates
merged_crime_population <- inner_join(drug_offense_counts, population_data, by = "Partial_Postcode")

# Calculate the drug offense rate per 10,000 people using the actual population
merged_crime_population <- merged_crime_population %>%
  mutate(Drug_Offense_Rate_per_10000 = (Drug_Offenses_Count / Population2023) * 10000)

# Step 3: Merge with Broadband Speed Data
merged_data_final <- inner_join(broadband_speed_data, 
                                merged_crime_population %>% select(Partial_Postcode, Drug_Offense_Rate_per_10000), 
                                by = "Partial_Postcode")

# Step 4: Generate the updated visualization
ggplot(merged_data_final, aes(x = Average_Download_Speed, y = Drug_Offense_Rate_per_10000)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Average Download Speed vs. Drug Offense Rates per 10,000 People (Adjusted by Population)",
       x = "Average Download Speed (Mbps)",
       y = "Drug Offense Rate per 10,000 People") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)  # Format y-axis with commas
