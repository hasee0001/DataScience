# Load necessary libraries
library(ggplot2)
library(dplyr)
library(scales) # For formatting the y-axis labels

# Load data
house_data <- read_csv('/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/House_rate_cleaned.csv')

# Filter data for the year 2023
house_data_2023 <- house_data %>% filter(`Date of Transfer` == 2023)

# Calculate the average house price for each Town/City, District, and County in 2023
average_price_town <- house_data_2023 %>% 
  group_by(Town) %>% 
  summarize(Average_Price = mean(Price, na.rm = TRUE))
average_price_district <- house_data_2023 %>% 
  group_by(District) %>% summarize(Average_Price = mean(Price, na.rm = TRUE))
average_price_county <- house_data_2023 %>% 
  group_by(County) %>% summarize(Average_Price = mean(Price, na.rm = TRUE))

# Box Plot for 2023 Average House Prices By County
ggplot(house_data_2023, aes(x = County, y = Price, fill = County)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16) +
  scale_fill_manual(values = c("CITY OF BRISTOL" = "#F08000", "CORNWALL" = "#FFD700")) + # Custom colors
  scale_y_continuous(labels = scales::comma, limits = c(0, 3000000), breaks = seq(0, 3000000, 500000)) + # Adjusted y-scale
  labs(title = "Distribution of House Prices in Bristol and Cornwall (2023)",
       x = "County",
       y = "House Price") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none")

# Bar Chart for 2023 Average House Prices by County
ggplot(average_price_county, aes(x = County, y = Average_Price, fill = County)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("CITY OF BRISTOL" = "#F08000", "CORNWALL" = "#FFD700")) + # Custom colors
  scale_y_continuous(labels = scales::comma, limits = c(0, 350000), breaks = seq(0, 350000, 50000)) + # Adjusted y-scale
  labs(title = "Average House Prices in Bristol and Cornwall (2023)",
       x = "County",
       y = "Average House Price") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none")

# Calculate average house prices by county and year
avg_houseprices2 <- house_data %>%
  group_by(County, `Date of Transfer`) %>%
  summarise(`Average Price` = mean(Price, na.rm = TRUE), .groups = "drop")

# Line graph for average house prices from 2020-2023
ggplot(avg_houseprices2 %>%
         filter(`Date of Transfer` %in% 2020:2023),
       aes(x = `Date of Transfer`, y = `Average Price`, color = County, group = County)) +
  geom_line(size = 2) +  # Thicker line width for better visibility
  geom_point(size = 4) +  # Larger points to stand out
  scale_y_continuous(labels = scales::comma, limits = c(0, 400000), breaks = seq(0, 400000, 50000)) + # Adjusted y-scale
  scale_color_manual(values = c("CITY OF BRISTOL" = "#F08000", "CORNWALL" = "#FFD700")) + # Custom colors
  labs(title = "Average House Prices from 2020 to 2023",
       x = "Year",
       y = "Average Price") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 12))  # Adjust legend text size