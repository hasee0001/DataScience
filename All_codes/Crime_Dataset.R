library(tidyverse)

crime_rates_directory <- "/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/Crime_rates"
postcode_lsoa_dataset_path <- "/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Postcode_cleaned_LSOA.csv"

postcode_lsoa_dataset <- read_csv(postcode_lsoa_dataset_path, 
                                  col_types = cols(LSOA_Code = col_character(), 
                                                   Postcode = col_character(), 
                                                   Partial_Postcode = col_character(), 
                                                   Town = col_character(), 
                                                   County = col_character()))

combined_crime_data <- list.files(crime_rates_directory, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  select(`LSOA code`, `Crime type`, Month, `Last outcome category`) %>%
  rename(LSOA_Code = `LSOA code`, 
         Crime_Type = `Crime type`, 
         Crime_Date = Month, 
         Outcome_Category = `Last outcome category`) %>%
  mutate(Crime_Date = as.Date(paste0(Crime_Date, "-01"), format = "%Y-%m-%d"),  
         Crime_Year = substr(Crime_Date, 1, 4),  
         Crime_Month = substr(Crime_Date, 6, 7)) %>%
  filter(!is.na(LSOA_Code) & !is.na(Crime_Type)) %>%
  right_join(postcode_lsoa_dataset, by = "LSOA_Code", relationship = "many-to-many") %>%
  mutate(Falls_Within = ifelse(is.na(Outcome_Category), "Status update unavailable", Outcome_Category),
         Serial_Number = row_number()) %>%
  select(Crime_Date, Falls_Within, Crime_Type, LSOA_Code, 
         Postcode, Partial_Postcode, Town, County) %>%
  distinct() %>%
  as_tibble()

# Check for missing values
missing_values_summary <- sapply(combined_crime_data, function(x) sum(is.na(x)))
print(missing_values_summary)

# Check for duplicates
row_count_before <- nrow(combined_crime_data)
combined_crime_data <- combined_crime_data %>% distinct()
row_count_after <- nrow(combined_crime_data)
print(row_count_before - row_count_after)  # Number of duplicates removed

write_csv(combined_crime_data, "/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Cleaned_Crime_Data.csv")

# Check the first few rows of the cleaned crime data
head(combined_crime_data)

# Check the unique values in the 'Crime_Type' column
unique(combined_crime_data$Crime_Type)
