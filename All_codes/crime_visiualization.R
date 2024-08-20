library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales) # For formatting the y-axis labels
library(fmsb) # For radar charts
library(reshape2) # For data reshaping for radar chart
library(patchwork) # For combining plots if needed

# Load data
crime_data_cleaned  <- read_csv('/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Cleaned_Crime_Data.csv')
population_data_cleaned <- read_csv('/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Cleaned_Population_Data.csv')

#Check the first few rows of the cleaned crime data
head(crime_data_cleaned)

# Check the unique values in the 'Crime_Type' column
unique(crime_data_cleaned$Crime_Type)
# Check the unique years in the data
unique(crime_data_cleaned$Year)


crime_data_cleaned <- crime_data_cleaned %>%
  mutate(Year = lubridate::year(as.Date(Crime_Date)))

# Filter for drug offenses in 2023 for Bristol and Cornwall
drug_offense_2023 <- crime_data_cleaned %>%
  filter(Year == 2023, Crime_Type == "Drugs", County %in% c("CITY OF BRISTOL", "CORNWALL"))

# Calculate the rate per 10,000 people for each county
drug_offense_rate <- drug_offense_2023 %>%
  group_by(County) %>%
  summarise(Total_Crimes = n()) %>%
  left_join(population_data_cleaned %>% select(County, Population2023), by = "County") %>%
  mutate(Rate_per_10K = (Total_Crimes / Population2023) * 10000)
print(drug_offense_rate)

# a. Bar Plot for Drug Offense Rate in 2023
ggplot(drug_offense_rate, aes(x = County, y = Rate_per_10K, fill = County)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Drug Offense Rate for 2023 in Bristol and Cornwall",
    x = "County",
    y = "Rate per 10,000 People"
  ) +
  theme_minimal()

# b. Radar Chart for an Alternative Crime Type (e.g., "Burglary")
alternative_crime_data <- crime_data_cleaned %>%
  filter(Year %in% 2020:2023, Crime_Type == "Burglary", County %in% c("CITY OF BRISTOL", "CORNWALL"))

# Assuming `alternative_crime_data` is already prepared, let's aggregate the data by year and county:
alternative_crime_agg <- alternative_crime_data %>%
  group_by(Year, County) %>%
  summarise(Total_Crimes = n()) %>%
  spread(County, Total_Crimes, fill = 0)

# Prepare data for radar chart
radar_data <- alternative_crime_agg %>%
  select(-Year) %>%
  as.data.frame()

# Setting the row names to the years
row.names(radar_data) <- alternative_crime_agg$Year

# Adding max and min values for scaling purposes in the radar chart
radar_data <- rbind(rep(max(radar_data, na.rm = TRUE), ncol(radar_data)),
                    rep(0, ncol(radar_data)),
                    radar_data)

# Generating and saving the radar chart with specified colors
png(filename = "radar_chart.png", width = 800, height = 800)
radarchart(radar_data, axistype = 1, 
           pcol = c("red", "blue"), pfcol = c("#FF6347", "#4682B4"), plwd = 2,
           cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, max(radar_data[-c(1,2),], na.rm = TRUE), length.out = 5),
           title = "Crime Rate by County and Year (Radar Chart)")
legend(x = 1, y = 1, legend = row.names(radar_data)[-c(1, 2)], col = c("red", "blue"), lty = 1)
dev.off()


# Ensure correct filtering for December 2023
robbery_data_dec_2023 <- crime_data_cleaned %>%
  filter(Year == 2023, Crime_Type == "Robbery", month(Crime_Date) == 12)

# Aggregate by Town
robbery_agg_dec_2023 <- robbery_data_dec_2023 %>%
  group_by(Town) %>%
  summarise(Total_Robberies = n())

# Generate pie chart
ggplot(robbery_agg_dec_2023, aes(x = "", y = Total_Robberies, fill = Town)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Pie Chart of Robbery Rate by Town/City for December 2023") +
  theme_minimal()

# d. Line Chart for Drug Offense Rate per 10,000 People (2020-2023)
crime_summary <- crime_data_cleaned %>%
  mutate(`Crime Date` = year(Crime_Date)) %>% 
  filter(`Crime Date` %in% c(2021, 2022, 2023)) %>% 
  group_by(`Partial_Postcode`, `Crime_Type`, `County`, `Crime Date`) %>%
  tally() %>%  
  rename(`Crime Count` = n) %>%  
  filter(`Crime_Type` == "Drugs")  # Filter for drug-related crimes

# Join with population data and rename columns
joined_data <- crime_summary %>%
  left_join(population_data_cleaned, by = "Partial_Postcode", suffix = c("_crime", "_pop")) %>%
  rename(`County` = `County_crime`)  

# Calculate the Drug Offense Rate
crime_dataset_with_rate <- joined_data %>%
  mutate(`Drug Offense Rate` = (`Crime Count` / Population2023) * 10000) %>%
  select(`County`, `Crime Date`, `Drug Offense Rate`, `Partial_Postcode`, `Crime_Type`) %>%
  group_by(`County`, `Crime Date`) %>%
  summarize(`Drug Offense Rate` = mean(`Drug Offense Rate`, na.rm = TRUE), .groups = 'drop')

# Convert the 'Crime Date' to a factor to treat it as discrete data
crime_dataset_with_rate$`Crime Date` <- factor(crime_dataset_with_rate$`Crime Date`)

# Create the line chart
ggplot(crime_dataset_with_rate, aes(x = `Crime Date`, y = `Drug Offense Rate`, color = `County`, group = `County`)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Drug Offense Rate by County (2021-2023)",
       x = "Year",
       y = "Drug Offense Rate (per 10,000 residents)") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(limits = c("2021", "2022", "2023")) +  # No warning here, as 'Crime Date' is now a factor
  theme_classic()  # Using a clean and simple theme
