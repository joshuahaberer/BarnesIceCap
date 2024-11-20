
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