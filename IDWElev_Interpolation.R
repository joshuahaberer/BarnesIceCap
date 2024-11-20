
################ Set Up for Spatial Interpolation
library(gstat)
library(sf)

# Convert cleaned data to spatial points
cleaned_sf <- st_as_sf(cleaned_data, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform to a projected CRS for interpolation
cleaned_sf <- st_transform(cleaned_sf, crs = 32617)  # Adjust UTM

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


########## Kriging DONT RUN
variogram_model <- gstat::variogram(Elevation_Change ~ 1, locations = cleaned_sf)
fit_variogram <- gstat::fit.variogram(variogram_model, model = vgm(1, "Sph", 500, 1))

kriging_model <- gstat(
  formula = Elevation_Change ~ 1,
  locations = cleaned_sf,
  model = fit_variogram
)

#kriging_result <- predict(kriging_model, newdata = grid)
#grid$Elevation_Change <- kriging_result$var1.pred

