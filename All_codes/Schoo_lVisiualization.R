library(ggplot2)
library(dplyr)
library(readr)

# Load the cleaned school dataset
school_data_cleaned <- read_csv('/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Dataset_cleaned_school.csv')

# Convert the 'Year' column to a date format to filter data easily
school_data_cleaned <- school_data_cleaned %>%
  mutate(Year = as.Date(paste0(substr(Year, nchar(Year) - 3, nchar(Year)), "-01-01")))

# Filter data for the year 2023
school_data_2023 <- school_data_cleaned %>%
  filter(format(Year, "%Y") == "2023")

# Custom theme for the visualizations
custom_theme <- theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "gray20"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )

# Create a box plot for average attainment 8 score of 2023 for both counties
ggplot(school_data_2023, aes(x = County, y = Attainment_Score, fill = County)) +
  geom_boxplot() +
  ggtitle('Average Attainment 8 Score of 2023 for Both Counties') +
  ylab('Attainment Score') +
  xlab('County') +
  custom_theme

# Line graph for Cornwall County's average attainment 8 score district-wise

# Filter data for Cornwall
cornwall_data <- school_data_cleaned %>%
  filter(County == 'Cornwall', format(Year, "%Y") %in% c("2022", "2023"))

# Group by Town and Year and calculate the average attainment score
cornwall_grouped <- cornwall_data %>%
  group_by(Town, Year) %>%
  summarise(Average_Attainment_Score = mean(Attainment_Score, na.rm = TRUE), .groups = 'drop')

# Create a line graph for Cornwall
ggplot(cornwall_grouped, aes(x = Year, y = Average_Attainment_Score, color = Town, group = Town)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  ggtitle('Cornwall County Average Attainment 8 Score District-Wise (2022-2023)') +
  ylab('Average Attainment Score') +
  xlab('Year') +
  custom_theme

# Line graph for Bristol County's average attainment 8 score district-wise

# Filter data for Bristol
bristol_data <- school_data_cleaned %>%
  filter(County == 'Bristol', format(Year, "%Y") %in% c("2022", "2023"))

# Group by Town and Year and calculate the average attainment score
bristol_grouped <- bristol_data %>%
  group_by(Town, Year) %>%
  summarise(Average_Attainment_Score = mean(Attainment_Score, na.rm = TRUE), .groups = 'drop')

# Create a line graph for Bristol
ggplot(bristol_grouped, aes(x = Year, y = Average_Attainment_Score, color = Town, group = Town)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  ggtitle('Bristol County Average Attainment 8 Score District-Wise (2022-2023)') +
  ylab('Average Attainment Score') +
  xlab('Year') +
  custom_theme
