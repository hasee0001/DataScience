# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(scales)

# Load the datasets
broadband_speed_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Broadband_speed_dataset_cleaned.csv")
school_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/School_Dataset_Cleaned.csv")

# Merge the school dataset with the broadband speed dataset using the Partial_Postcode column
merged_data <- inner_join(school_data, broadband_speed_data, by = "Partial_Postcode")

# Drop any rows with missing values in the relevant columns
merged_data_cleaned <- merged_data %>%
  filter(!is.na(`Attainment Score`) & !is.na(Average_Download_Speed))

# Create the scatter plot with a regression line
ggplot(merged_data_cleaned, aes(x = Average_Download_Speed, y = `Attainment Score`)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Average Download Speed vs. Attainment 8 Score",
       x = "Average Download Speed (Mbps)",
       y = "Attainment 8 Score") +
  theme_minimal()

# Perform linear regression
model <- lm(`Attainment Score` ~ Average_Download_Speed, data = merged_data_cleaned)

# Summarize the model
summary(model)
