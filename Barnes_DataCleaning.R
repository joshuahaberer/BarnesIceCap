install.packages("dplyr")
library(dplyr)

library(tidyr)
library(readr)
library(purrr)
library(ggplot2)

############## Load ICESat Data
# Load the ICESat 2008 data
icesat_2008 <- read_csv("~/Desktop/icesat_2008.csv")

# Preview the data
head(icesat_2008)

# Check for missing values and basic structure
summary(icesat_2008)

############## Load ICESat_2 Data
# Define the folder path containing the ICESat-2 track files
icesat2_folder <- "~/Desktop/2020_IceSat2_ATL06/"

# Read all track files and combine into a single data frame
icesat2_data <- list.files(path = icesat2_folder, pattern = "*.csv", full.names = TRUE) %>%
  map_dfr(read_csv)

# Preview the combined ICESat-2 data
head(icesat2_data)

# Check structure and summary
summary(icesat2_data)

############### Inspect and Clean Data

icesat_2008 <- icesat_2008 %>%
  filter(!is.na(Latitude), !is.na(Longitude), !is.na(Elevation))

icesat2_data <- icesat2_data %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(h_li))

# Rename ICESat-2 elevation column to match ICESat
icesat2_data <- icesat2_data %>%
  #rename(Elevation = h_li) %>%
  #rename(Latitude = latitude) %>%
  #rename(Longitude = longitude) %>%
  mutate(Year = 2020)  # Add the year column

# Add the Year column to ICESat if not already added
icesat_2008 <- icesat_2008 %>%
  mutate(Year = 2008)

# Combine the datasets
combined_data <- bind_rows(
  icesat_2008 %>% select(Longitude, Latitude, Elevation, Year),
  icesat2_data %>% select(Longitude, Latitude, Elevation, Year)
)

write_csv(combined_data, "combined_elevation_data.csv")

# Check the combined dataset
head(combined_data)

# Remove outliers and filter valid elevation values
cleaned_data <- combined_data %>%
  filter(Elevation >= 0, Elevation <= 1400)

# Check the updated summary
summary(cleaned_data$Elevation)

# Plot elevation vs. latitude for a quick check
ggplot(cleaned_data, aes(x = Latitude, y = Elevation, color = factor(Year))) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Elevation Comparison", x = "Latitude", y = "Elevation (m)", color = "Year")
