library(tidyverse)

# Set the path to the population data file and postcode_to_laso
population_file <- "/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/Population2011_1656567141570.csv"
postcode_to_LSOA_cleaned <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Postcode_cleaned_LSOA.csv")

# Load the population dataset
population_data <- read_csv(population_file)

# Postcode Formatting: Remove spaces and standardize structure
cleaned_population_data <- population_data %>%
  rename(Partial_Postcode = Postcode) %>%  # Rename Postcode to Partial_Postcode
  mutate(Partial_Postcode = gsub(" ", "", Partial_Postcode),  # Remove all spaces
         Partial_Postcode = if_else(nchar(Partial_Postcode) == 5, 
                                    paste0(substr(Partial_Postcode, 1, 4), " ", substr(Partial_Postcode, 5, 6)), 
                                    paste0(substr(Partial_Postcode, 1, 3), " ", substr(Partial_Postcode, 4, 5))))  # Standardize formatting

# Update population to 2023 figures using a growth factor
cleaned_population_data <- cleaned_population_data %>%
  mutate(Population2023 = 1.00561255390388033 * Population) %>%  # Apply growth factor to update population
  select(-Population)  


# Merging with LSOA Data: Join population data with LSOA data
cleaned_population_data <- cleaned_population_data %>%
  as_tibble() %>%  # Convert to tibble for better handling
  right_join(postcode_to_LSOA_cleaned, by = "Partial_Postcode") %>%  # Join using Partial_Postcode as the key
  na.omit()  # Remove rows with missing values after the join

# Data Cleaning: Remove any duplicate rows
cleaned_population_data <- cleaned_population_data %>% distinct()

# Saving the Data: Save the cleaned dataset to a new CSV file
write_csv(cleaned_population_data, "/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Cleaned_Population_Data.csv")

# View the cleaned dataset (optional)
View(cleaned_population_data)
