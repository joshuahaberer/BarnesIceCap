
############# Analyze and Visualize Elevation Changes
summary(matched_data$Elevation_Change)

# Remove outliers and filter valid elevation values
cleaned_data <- matched_data %>%
  filter(Elevation >= 0, Elevation <= 1400)

# Check the updated summary
summary(cleaned_data$Elevation)

# Calculate elevation change with cleaned data
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
