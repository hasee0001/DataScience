library(tidyverse)
library(dplyr)
library(lubridate)

# Define the file paths for each dataset
bristol_2021_2022_path <- "/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/Bristoldataset2021to2022.csv"
bristol_2022_2023_path <- "/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/Infobristoldataset2022to2023.csv"
cornwall_2021_2022_path <- "/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/Cornwalldataset2021to2022.csv"
cornwall_2022_2023_path <- "/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/cornwalldataset2022to2023.csv"

# Clean the 2021-2022 Bristol School Dataset
Bristol_2021_2022_school_cleaning <- read_csv(bristol_2021_2022_path) %>%
  select(SCHNAME, ATT8SCR, TOWN, PCODE) %>%
  rename(`School Name` = SCHNAME, Town = TOWN, `Postcode` = PCODE, `Attainment Score` = ATT8SCR) %>%
  as_tibble() %>%
  mutate(Partial_Postcode = substr(Postcode, 1, 5)) %>%
  na.omit() %>%
  filter(`Attainment Score` != "NE" & `Attainment Score` != "SUPP") %>%
  mutate(County = "CITY OF BRISTOL") %>%
  mutate(Year = "2022")

# Clean the 2022-2023 Bristol School Dataset
Bristol_2022_2023_school_cleaning <- read_csv(bristol_2022_2023_path) %>%
  select(SCHNAME, ATT8SCR, TOWN, PCODE) %>%
  rename(`School Name` = SCHNAME, Town = TOWN, `Postcode` = PCODE, `Attainment Score` = ATT8SCR) %>%
  as_tibble() %>%
  mutate(Partial_Postcode = substr(Postcode, 1, 5)) %>%
  na.omit() %>%
  filter(`Attainment Score` != "NE" & `Attainment Score` != "SUPP") %>%
  mutate(County = "CITY OF BRISTOL") %>%
  mutate(Year = "2023")

# Clean the 2021-2022 Cornwall School Dataset
Cornwall_2021_2022_school_cleaning <- read_csv(cornwall_2021_2022_path) %>%
  select(SCHNAME, ATT8SCR, TOWN, PCODE) %>%
  rename(`School Name` = SCHNAME, Town = TOWN, `Postcode` = PCODE, `Attainment Score` = ATT8SCR) %>%
  as_tibble() %>%
  mutate(Partial_Postcode = substr(Postcode, 1, 5)) %>%
  na.omit() %>%
  filter(`Attainment Score` != "NE" & `Attainment Score` != "SUPP") %>%
  mutate(County = "CORNWALL") %>%
  mutate(Year = "2022")

# Clean the 2022-2023 Cornwall School Dataset
Cornwall_2022_2023_school_cleaning <- read_csv(cornwall_2022_2023_path) %>%
  select(SCHNAME, ATT8SCR, TOWN, PCODE) %>%
  rename(`School Name` = SCHNAME, Town = TOWN, `Postcode` = PCODE, `Attainment Score` = ATT8SCR) %>%
  as_tibble() %>%
  mutate(Partial_Postcode = substr(Postcode, 1, 5)) %>%
  na.omit() %>%
  filter(`Attainment Score` != "NE" & `Attainment Score` != "SUPP") %>%
  mutate(County = "CORNWALL") %>%
  mutate(Year = "2023")

# Combine all cleaned datasets into a single tibble
school_dataset_cleaned_combined <- bind_rows(
  Bristol_2021_2022_school_cleaning,
  Bristol_2022_2023_school_cleaning,
  Cornwall_2021_2022_school_cleaning,
  Cornwall_2022_2023_school_cleaning
)

# Save the combined cleaned dataset to CSV
cleanedSchool_path <- "/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/School_Dataset_Cleaned.csv"
write_csv(school_dataset_cleaned_combined, cleanedSchool_path)

# View the cleaned and combined dataset
View(school_dataset_cleaned_combined)
