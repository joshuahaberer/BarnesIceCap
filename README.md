# Barnes Ice Cap Elevation and Mass Change Analysis

### Project Overview
This project aims to quantify the elevation and mass changes of the Barnes Ice Cap using altimetry data from ICESat and ICESat-2. The analysis involves:

- Combining datasets from 2008 and 2020.
- Interpolating elevation changes using Inverse Distance Weighting (IDW).
- Analyzing trends by elevation bands.
- Calculating total volume and mass changes.

## Load and Preview ICESat and ICESat-2 Data

### Loading ICESat Data
The ICESat (2008) dataset is stored in a .csv file containing fields such as longitude, latitude, and elevation. The following steps load the data, preview its structure, and check for missing or invalid values.

```
############## Load ICESat Data
# Load the ICESat 2008 data
icesat_2008 <- read_csv("~/Desktop/icesat_2008.csv")

# Preview the data
head(icesat_2008)

# Check for missing values and basic structure
summary(icesat_2008)
```
### Loading ICESat-2 Data
ICESat-2 data is distributed across multiple .csv files, with each file representing a track (e.g., T306.csv). The files are read into R and combined into a single data frame for consistency.

```
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
```

## Data Inspection, Cleaning, and Preparation
This section describes how the ICESat (2008) and ICESat-2 (2020) datasets are cleaned, harmonized, combined, and pre-processed for further analysis. These steps ensure data consistency and remove invalid or outlier values.

### Inspect and Clean Data
Remove missing values and ensure valid elevation data for both ICESat and ICESat-2 datasets.

```
# Filter ICESat data to remove rows with missing values
icesat_2008 <- icesat_2008 %>%
  filter(!is.na(Latitude), !is.na(Longitude), !is.na(Elevation))

# Filter ICESat-2 data similarly
icesat2_data <- icesat2_data %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(h_li))
```

### Combine Datasets
Standardize column names, add year information, and combine datasets into a unified structure.

```
# Rename ICESat-2 columns to match ICESat
icesat2_data <- icesat2_data %>%
  rename(Elevation = h_li, Latitude = latitude, Longitude = longitude) %>%
  mutate(Year = 2020)  # Add Year column for ICESat-2

# Add the Year column to ICESat
icesat_2008 <- icesat_2008 %>%
  mutate(Year = 2008)

# Combine the datasets
combined_data <- bind_rows(
  icesat_2008 %>% select(Longitude, Latitude, Elevation, Year),
  icesat2_data %>% select(Longitude, Latitude, Elevation, Year)
)

# Preview the combined dataset
head(combined_data)
```

### Remove Outliers
Filter the combined dataset to retain only valid elevation values within the range [0, 1400] meters.

```
# Filter elevation values within valid range
cleaned_data <- combined_data %>%
  filter(Elevation >= 0, Elevation <= 1400)

# Summarize the cleaned dataset
summary(cleaned_data$Elevation)
```

### Visualize Data
Plot elevation data against latitude to ensure the cleaned data is consistent and identify any trends.

```
# Plot elevation vs. latitude, colored by Year
ggplot(cleaned_data, aes(x = Latitude, y = Elevation, color = factor(Year))) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Elevation Comparison", x = "Latitude", y = "Elevation (m)", color = "Year")
```

## Spatial Matching and Elevation Change Analysis
This section outlines the methodology for identifying spatial crossover points between ICESat (2008) and ICESat-2 (2020) data, calculating elevation changes, and analyzing spatial and statistical trends. Key equations are used to calculate elevation differences and visualize the results.

### Identify Spatial Crossover Points
Find the closest spatial points between ICESat (2008) and ICESat-2 (2020) datasets and calculate elevation differences.

```
library(sf)        # For spatial data handling
library(FNN)       # For nearest neighbor searches

# Convert datasets to spatial data
icesat_2008_sf <- st_as_sf(icesat_2008, coords = c("Longitude", "Latitude"), crs = 4326)
icesat2_sf <- st_as_sf(icesat2_data, coords = c("Longitude", "Latitude"), crs = 4326)

# Extract coordinates for matching
icesat_2008_coords <- st_coordinates(icesat_2008_sf)
icesat2_coords <- st_coordinates(icesat2_sf)

# Find nearest ICESat-2008 neighbor for each ICESat-2 point
nn <- get.knnx(icesat_2008_coords, icesat2_coords, k = 1)

# Add matched elevation data from ICESat-2008
matched_data <- icesat2_data %>%
  mutate(
    Elevation_2008 = icesat_2008$Elevation[nn$nn.index[, 1]],
    Elevation_Change = Elevation - Elevation_2008
  )
```






