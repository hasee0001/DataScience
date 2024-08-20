# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(scales)  # This library helps with formatting

# Load the datasets
house_price_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/House_rate_cleaned.csv")
broadband_speed_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Broadband_speed_dataset_cleaned.csv")

# Aggregate broadband data to get the mean download speed per Partial_Postcode
aggregated_broadband_data <- broadband_speed_data %>%
  group_by(Partial_Postcode) %>%
  summarize(Average_Download_Speed = mean(Average_Download_Speed, na.rm = TRUE),
            Maximum_Download_Speed = mean(Maximum_Download_Speed, na.rm = TRUE))

# Merge the datasets using the 'Partial_Postcode' column
merged_data <- inner_join(house_price_data, aggregated_broadband_data, by = "Partial_Postcode")

# Drop any rows with missing values in the relevant columns
merged_data_cleaned <- merged_data %>%
  filter(!is.na(Price) & !is.na(Average_Download_Speed))

# Take a random subset of available points (without replacement)
set.seed(42)
subset_data <- merged_data_cleaned %>% sample_n(size = nrow(merged_data_cleaned))

# Create the scatter plot with a regression line and formatted y-axis
ggplot(subset_data, aes(x = Average_Download_Speed, y = Price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Analyzing the Influence of Internet Connectivity on Housing Markets (Subset of 10,000 points)",
       x = "Average Download Speed (Mbps)",
       y = "House Price") +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  theme_minimal()
