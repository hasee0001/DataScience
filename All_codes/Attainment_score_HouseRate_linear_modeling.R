# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(scales)

# Load the datasets
school_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/School_Dataset_Cleaned.csv")
house_price_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/House_rate_cleaned.csv")

# Merge the school dataset with the house price dataset using the Partial_Postcode column
merged_data <- inner_join(school_data, house_price_data, by = "Partial_Postcode")

# Drop any rows with missing values in the relevant columns
merged_data_cleaned <- merged_data %>%
  filter(!is.na(`Attainment Score`) & !is.na(Price))

# Create the scatter plot with a regression line
ggplot(merged_data_cleaned, aes(x = `Attainment Score`, y = Price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Exploring the Relationship Between School Performance and Property Values",
       x = "Attainment 8 Score",
       y = "House Price") +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  theme_minimal()
