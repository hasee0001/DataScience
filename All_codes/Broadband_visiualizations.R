# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales) # For formatting the y-axis labels

# Set the working directory to save the visualizations
setwd("/Users/hasu/Desktop/TownRecommendationSystem /Graphs")

# Load the broadband data
broadband_data <- read_csv('/Users/hasu/Desktop/TownRecommendationSystem /cleaned_datasets/Broadband_speed_dataset_cleaned.csv')

# Filter data by county
bristol_data <- broadband_data %>% filter(County == 'CITY OF BRISTOL')
cornwall_data <- broadband_data %>% filter(County == 'CORNWALL')

# Define custom color palette
custom_palette <- c("#4d3061","#405a95","#0085bc","#00b0d2","#0fd9d5","#87ffcc")

# 1. Box Plot for Average Download Speed of Bristol and Cornwall Combined
box_plot <- ggplot(broadband_data, aes(x = County, y = Average_Download_Speed, fill = County)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16) +
  scale_fill_manual(values = c("CITY OF BRISTOL" = custom_palette[1], "CORNWALL" = custom_palette[2])) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 150), breaks = seq(0, 150, 25)) + # Adjusted y-scale for clarity
  labs(title = "Average Download Speed of Bristol and Cornwall (2023)",
       x = "County",
       y = "Average Download Speed (Mbps)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none")

# Save the box plot
ggsave(filename = "Br_Average_Download_Speed_Boxplot.png", plot = box_plot, width = 14, height = 12)

# 2. Bar Chart for Average and Maximum Download Speeds by Town for Cornwall
# Reshape data to long format
cornwall_long <- cornwall_data %>%
  group_by(Town) %>%
  summarise(avg_download_speed = mean(Average_Download_Speed, na.rm = TRUE),
            max_download_speed = mean(Maximum_Download_Speed, na.rm = TRUE)) %>%
  pivot_longer(cols = c(avg_download_speed, max_download_speed), names_to = "Speed_Type", values_to = "Speed")

# Bar Chart for Cornwall
cornwall_bar_chart <- ggplot(cornwall_long, aes(y = Town, x = Speed, fill = Speed_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black", width = 0.7) +
  scale_fill_manual(name = "Speed_Type", values = custom_palette[3:4]) +
  scale_x_continuous(labels = scales::comma, limits = c(0, 350), breaks = seq(0, 350, 50)) + # Adjusted x-scale
  labs(title = "Average and Maximum Download Speeds by Town for Cornwall (2023)",
       x = "Speed (Mbps)",
       y = "Town/City") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "top")

# Save the Cornwall bar chart
ggsave(filename = "Br_Cornwall_Speed_Bar_Chart.png", plot = cornwall_bar_chart, width = 14, height = 12)

# 3. Bar Chart for Average and Maximum Download Speeds by Town for Bristol
# Reshape data to long format
bristol_long <- bristol_data %>%
  group_by(Town) %>%
  summarise(avg_download_speed = mean(Average_Download_Speed, na.rm = TRUE),
            max_download_speed = mean(Maximum_Download_Speed, na.rm = TRUE)) %>%
  pivot_longer(cols = c(avg_download_speed, max_download_speed), names_to = "Speed_Type", values_to = "Speed")

# Bar Chart for Bristol
bristol_bar_chart <- ggplot(bristol_long, aes(y = Town, x = Speed, fill = Speed_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black", width = 0.7) +
  scale_fill_manual(name = "Speed_Type", values = custom_palette[5:6]) +
  scale_x_continuous(labels = scales::comma, limits = c(0, 200), breaks = seq(0, 200, 50)) + # Adjusted x-scale
  labs(title = "Average and Maximum Download Speeds by Town for Bristol (2023)",
       x = "Speed (Mbps)",
       y = "Town/City") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "top")

# Save the Bristol bar chart
ggsave(filename = "Br_Bristol_Speed_Bar_Chart.png", plot = bristol_bar_chart, width = 14, height = 12)
