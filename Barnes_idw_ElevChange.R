library(sf)
library(raster)

# Load the Barnes Ice Cap extent shapefile
barnes_extent <- st_read("~/Desktop/2024_BarnesIceCap_Extent.shp")

# Inspect the shapefile
plot(st_geometry(barnes_extent))

# Define the resolution
resolution <- 500  # 500 meters

# Create a grid that matches the shapefile extent
barnes_grid <- st_make_grid(barnes_extent, cellsize = resolution, what = "centers")

# Clip the grid to the Barnes Ice Cap perimeter
barnes_grid_clipped <- barnes_grid[barnes_extent, ]

# Convert to sf object for compatibility
barnes_grid_clipped <- st_as_sf(barnes_grid_clipped)

# Plot the clipped grid over the Barnes Ice Cap extent
plot(st_geometry(barnes_extent), col = NA, border = "blue")
plot(st_geometry(barnes_grid_clipped), add = TRUE, col = "red", pch = 20)

library(gstat)

# Perform IDW interpolation
idw_model <- gstat::gstat(
  formula = Elevation_Change ~ 1,
  locations = cleaned_sf,  # Your cleaned elevation data as sf object
  nmax = 15
)

# Interpolate over the clipped grid
idw_result <- predict(idw_model, newdata = barnes_grid_clipped)

# Add results to the grid
barnes_grid_clipped$Elevation_Change <- idw_result$var1.pred

library(ggplot2)

# Convert to a data frame for plotting
barnes_df <- as.data.frame(st_coordinates(barnes_grid_clipped)) %>%
  cbind(Elevation_Change = barnes_grid_clipped$Elevation_Change)

# Plot the IDW results
ggplot(barnes_df, aes(X, Y, fill = Elevation_Change)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_equal() +
  theme_minimal() +
  labs(
    title = "Interpolated Elevation Change (Barnes Ice Cap)",
    x = "Longitude (m)", y = "Latitude (m)", fill = "Elevation Change (m)"
  )





