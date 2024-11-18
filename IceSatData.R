install.packages("dplyr")
library(dplyr)

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


############# Identify Spatial Crossover Points
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

############# Analyze and Visualize Elevation Changes
summary(matched_data$Elevation_Change)

# Remove outliers and filter valid elevation values
cleaned_data <- matched_data %>%
  filter(Elevation >= 0, Elevation <= 1400)

# Check the updated summary
summary(cleaned_data$Elevation)

# Recalculate elevation change with cleaned data
cleaned_data <- cleaned_data %>%
  mutate(Elevation_Change = Elevation - Elevation_2008)


# Histogram of elevation change
ggplot(matched_data %>% filter(Elevation_Change >= -500, Elevation_Change <= 500), 
       aes(x = Elevation_Change)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(
    title = "Distribution of Elevation Changes",
    x = "Elevation Change (m)",
    y = "Frequency"
  )

# Spatial visualization
ggplot() +
  geom_sf(data = st_as_sf(matched_data, coords = c("Longitude", "Latitude"), crs = 4326), 
          aes(color = Elevation_Change), size = 1) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Spatial Distribution of Elevation Changes", color = "Change (m)")

# Save cleaned data for later use
write_csv(cleaned_data, "cleaned_elevation_data.csv")

################ Set Up for Spatial Interpolation
library(gstat)
library(sf)

# Convert cleaned data to spatial points
cleaned_sf <- st_as_sf(cleaned_data, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform to a projected CRS (e.g., UTM) for interpolation
cleaned_sf <- st_transform(cleaned_sf, crs = 32617)  # Adjust UTM zone as needed

# Create a bounding box for the study area
grid <- st_make_grid(cleaned_sf, cellsize = 500, what = "centers") %>%  # 500m resolution
  st_as_sf() %>%
  mutate(ID = 1:nrow(.))  # Add an ID column for reference

# Set up IDW model
idw_model <- gstat::gstat(
  formula = Elevation_Change ~ 1,
  locations = cleaned_sf,
  nmax = 15
)

# Interpolate over the grid
idw_result <- predict(idw_model, newdata = grid, debug.level = 0)

# Add results back to the grid
grid$Elevation

# Assign the interpolated values to the grid
grid$Elevation_Change <- idw_result$var1.pred

# Verify the assignment
head(grid$Elevation_Change)

grid_df <- as.data.frame(st_coordinates(grid)) %>%
  cbind(Elevation_Change = grid$Elevation_Change)

# Verify the data frame
head(grid_df)


library(ggplot2)

# Convert the grid to a data frame for ggplot visualization
grid_df <- as.data.frame(st_coordinates(grid)) %>%
  cbind(Elevation_Change = grid$Elevation_Change)

# Plot the interpolated elevation change
ggplot(grid_df, aes(X, Y, fill = Elevation_Change)) +
  geom_raster() +
  scale_fill_viridis_c() +
  coord_equal() +
  theme_minimal() +
  labs(title = "Interpolated Elevation Change", x = "Longitude", y = "Latitude", fill = "Change (m)")


########## Kriging
variogram_model <- gstat::variogram(Elevation_Change ~ 1, locations = cleaned_sf)
fit_variogram <- gstat::fit.variogram(variogram_model, model = vgm(1, "Sph", 500, 1))

kriging_model <- gstat(
  formula = Elevation_Change ~ 1,
  locations = cleaned_sf,
  model = fit_variogram
)

#kriging_result <- predict(kriging_model, newdata = grid)
#grid$Elevation_Change <- kriging_result$var1.pred


################# Volume Change Calculation

#define cell grid
cell_area <- 500 * 500  # 500m x 500m in square meters

#Compute total volume change
# Total volume change (m³)
total_volume_change <- sum(grid_df$Elevation_Change * cell_area, na.rm = TRUE)

# Convert to km³ for easier interpretation
total_volume_change_km3 <- total_volume_change / 1e9

# Print the result
cat("Total Volume Change:", total_volume_change_km3, "km³\n")

# Mass Change Calculation
# Define densities (kg/m³)
pure_ice_density <- 917
firn_density <- 850

# Mass change (in Gt, gigatonnes)
mass_change_pure_ice <- total_volume_change * pure_ice_density / 1e12
mass_change_firn <- total_volume_change * firn_density / 1e12

# Print the results
cat("Mass Change (Pure Ice):", mass_change_pure_ice, "Gt\n")
cat("Mass Change (Firn Adjusted):", mass_change_firn, "Gt\n")

#Breakdown by Elevation Bands (Hypsometry)
# Create elevation bands (e.g., every 100m)
grid_df <- grid_df %>%
  mutate(Elevation_Band = cut(Elevation_Change, breaks = seq(-50, 1400, by = 100)))

# Summarize volume change by band
volume_by_band <- grid_df %>%
  group_by(Elevation_Band) %>%
  summarize(
    Volume_Change = sum(Elevation_Change * cell_area, na.rm = TRUE) / 1e9  # Convert to km³
  )

# Print summary
print(volume_by_band)

# Visulalize 
ggplot(volume_by_band %>% filter(!is.na(Elevation_Band)), aes(x = Elevation_Band, y = Volume_Change)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Volume Change by Elevation Band",
    x = "Elevation Band (m)",
    y = "Volume Change (km³)"
  )




