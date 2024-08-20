# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(scales)

# Load the datasets
house_price_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/House_rate_cleaned.csv")
crime_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Cleaned_Crime_Data.csv")

# Filter crime data for drug-related offenses in 2023
drug_data_2023 <- crime_data %>%
  filter(grepl("^2023", Crime_Date) & grepl("Drugs", Crime_Type, ignore.case = TRUE))

# Aggregate drug offenses by Partial_Postcode
drug_rate_2023 <- drug_data_2023 %>%
  group_by(Partial_Postcode) %>%
  summarize(Drug_Offenses_2023 = n())

# Merge the aggregated drug rate data with the house price data using the Partial_Postcode column
merged_data <- inner_join(house_price_data, drug_rate_2023, by = "Partial_Postcode")

# Drop any rows with missing values in the relevant columns
merged_data_cleaned <- merged_data %>%
  filter(!is.na(Price) & !is.na(Drug_Offenses_2023))

# Create the scatter plot with a regression line
ggplot(merged_data_cleaned, aes(x = Drug_Offenses_2023, y = Price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "The Impact of Drug-Related Crime on Property Values in 2023",
       x = "Drug Offenses in 2023",
       y = "House Price") +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  theme_minimal()

# Perform linear regression
model <- lm(Price ~ Drug_Offenses_2023, data = merged_data_cleaned)

# Summarize the model
summary(model)
