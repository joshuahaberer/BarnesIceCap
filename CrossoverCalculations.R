
############# Identify Spatial Crossover Points
library(sf)        #for spatial data handling
library(FNN)       #for nearest neighbor searches

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
