#Housing dataset 
# Load necessary libraries
library(dplyr)
library(purrr)
library(readr)# Define meaningful column names based on your data inspection
col_names <-c('ID','Price','Date','Postcode','Property_Type','Is_New','Tenure','PAON','SAON','Street','Locality','Town','District','County','Record_Status')# Load each dataset with the correct column names
df1 <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/Housing/pp-2020.csv", col_names = col_names)
df2 <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/Housing/pp-2021.csv", col_names = col_names)
df3 <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/Housing/pp-2022.csv", col_names = col_names)
df4 <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/Housing/pp-2023.csv", col_names = col_names)# Combine the datasets into one data frame
combined_data <- bind_rows(df1, df2, df3, df4)# Save the combined data to a CSV file in the Obtaine_data folder
output_path <-"/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/combined_housing_data_2020_2023.csv"
View(output_path)
write_csv(combined_data, output_path)# Confirmation message
print("Combined housing dataset has been saved successfully.")
View(output_path)


#Broadband speed
coverage_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/broadband speed/broadband/coverage.csv")

performance_data <- read_csv("/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/broadband speed/broadband/performance.csv")

merged_data <- merge(coverage_data, performance_data, by = "postcode", all = TRUE)

summary(merged_data)

output_path_broadband <- "/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/combined_broadband_speed.csv"
write_csv(merged_data, output_path_broadband)


#crimerate 
combine_files <-function(file_paths){
  data_list <- lapply(file_paths, read_csv)
  combined_data <- bind_rows(data_list)
  return(combined_data)}# Get all file paths for Avon and Somerset and Devon and Cornwall
file_paths_avon <- list.files(path ="/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/Crime_rates", 
                              pattern ="avon-and-somerset-street.csv$", full.names =TRUE, recursive =TRUE)

file_paths_devon <- list.files(path ="/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/Crime_rates", 
                               pattern ="devon-and-cornwall-street.csv$", full.names =TRUE, recursive =TRUE)# Combine all Avon and Somerset files
combined_avon <- combine_files(file_paths_avon)# Combine all Devon and Cornwall files
combined_devon <- combine_files(file_paths_devon)# Combine both regions into one dataframe
combined_crime_data <- bind_rows(combined_avon, combined_devon)# Save the combined data to a CSV file in the Obtaine_data folder
output_path_crime <-"/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/combined_crime_data.csv"
write_csv(combined_crime_data, output_path_crime)# Confirmation message
print("Combined crime dataset has been saved successfully.")



#school dataset 
# Load necessary libraries
library(dplyr)
library(readr)

# Define the directories for Bristol and Cornwall
bristol_2021_2022 <- list.files("/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/School dataset/Bristol School info/2021-2022", full.names = TRUE, pattern = "\\.csv$")
bristol_2022_2023 <- list.files("/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/School dataset/Bristol School info/2022-2023", full.names = TRUE, pattern = "\\.csv$")
cornwall_2021_2022 <- list.files("/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/School dataset/Cornwall School info/2021-2022", full.names = TRUE, pattern = "\\.csv$")
cornwall_2022_2023 <- list.files("/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/School dataset/Cornwall School info/2022-2023", full.names = TRUE, pattern = "\\.csv$")

# Combine Bristol datasets for 2021-2022
Bristoldataset2021to2022 <- 
  bristol_2021_2022 %>%
  lapply(read_csv, show_col_types = FALSE) %>%
  lapply(function(df) df %>% mutate(across(everything(), as.character))) %>%
  bind_rows()

# Combine Bristol datasets for 2022-2023
Infobristoldataset2022to2023 <- 
  bristol_2022_2023 %>%
  lapply(read_csv, show_col_types = FALSE) %>%
  lapply(function(df) df %>% mutate(across(everything(), as.character))) %>%
  bind_rows()

# Combine Cornwall datasets for 2021-2022
Cornwalldataset2021to2022 <- 
  cornwall_2021_2022 %>%
  lapply(read_csv, show_col_types = FALSE) %>%
  lapply(function(df) df %>% mutate(across(everything(), as.character))) %>%
  bind_rows()

# Combine Cornwall datasets for 2022-2023
cornwalldataset2022to2023 <- 
  cornwall_2022_2023 %>%
  lapply(read_csv, show_col_types = FALSE) %>%
  lapply(function(df) df %>% mutate(across(everything(), as.character))) %>%
  bind_rows()


# Save the combined datasets to CSV
write_csv(Bristoldataset2021to2022, "/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/Bristoldataset2021to2022.csv")
write_csv(Infobristoldataset2022to2023, "/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/Infobristoldataset2022to2023.csv")
write_csv(Cornwalldataset2021to2022, "/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/Cornwalldataset2021to2022.csv")
write_csv(cornwalldataset2022to2023, "/Users/hasu/Desktop/TownRecommendationSystem /Obtaine_data/cornwalldataset2022to2023.csv")
