
# VizPointPattern

<!-- badges: start -->
<!-- badges: end -->

The goal of VizPointPattern is to visualize point patterns for contamination data. Essentially, your workflow should
take addresses, geocode them through your choice of API (OSM is easiest, however, may be time consuming) and generates
a scale that helps you visualize where the highest points of contamination are. After, you visualize your contamination points
you can then calculate Moran's I to understand the if there is a spatial correlation between the point pattern. 

The original dataset utilized to test this package was the Flint Michigan Lead Conctration data where there are indicated
addresses and lead PPB. 

## Installation

You can install the development version of VizPointPattern like so:

``` r
# install.packages("devtools") # If you don't have devtools installed
devtools::install_github("selanifranchesca/VizPointPattern")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# Load the package and other necessary libraries for this example
library(VizPointPattern)
library(dplyr) # For data manipulation
library(sf)    # For spatial data operations

# --- 1. Prepare Dummy Geocoded Data ---
# This simulates the 'sf' object your 'geocode_contamination_data' function would output.
# Replace this with your actual data loading and geocoding process.
dummy_data <- data.frame(
  SampleID = paste0("S", 1:10),
  Address = c(
    "123 Main St", "456 Oak Ave", "789 Pine Rd", "101 Elm Ln", "202 Maple Dr",
    "303 Cedar Ct", "404 Birch Bld", "505 Willow Way", "606 Spruce Sq", "707 Fir Fld"
  ),
  Lead_ppb = c(15, 5, 25, 8, 18, 10, 30, 12, 22, 7),
  latitude = c(43.012, 43.025, 43.030, 43.018, 43.022, 43.008, 43.035, 43.015, 43.028, 43.010),
  longitude = c(-83.687, -83.700, -83.695, -83.680, -83.710, -83.690, -83.705, -83.685, -83.715, -83.698)
)

# Convert the dummy data to an sf object (points)
dummy_sf_data <- dummy_data %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) # CRS 4326 is WGS84 (lat/lon)

# --- 2. Visualize and Analyze Contamination Data ---
# Define example geographic boundaries (e.g., for an area in Flint, MI)
# Ensure these boundaries encompass your data points.
example_lon_min <- -83.75
example_lon_max <- -83.65
example_lat_min <- 43.00
example_lat_max <- 43.05

# Call the main plotting and analysis function
contamination_map <- VizPointPattern::plot_contamination_map(
  sf_data = dummy_sf_data,
  contamination_col = "Lead_ppb",
  boundary_lon_min = example_lon_min,
  boundary_lon_max = example_lon_max,
  boundary_lat_min = example_lat_min,
  boundary_lat_max = example_lat_max,
  # These are default column names, but explicitly stated for clarity
  lat_col_name = "latitude",
  lon_col_name = "longitude",
  color_palette = "YlOrRd", # A warm color palette
  legend_title = "Lead Concentration (ppb)",
  map_title = "Hypothetical Lead Samples in Flint Area",
  map_zoom = 13, # Adjust initial map zoom
  circle_marker_radius = 6, # Adjust size of points on map
  morans_k_neighbors = 3, # Number of neighbors for Moran's I calculation
  morans_alternative = "greater", # Test for positive spatial autocorrelation
  sample_id_col = "SampleID" # Column to use for pop-up labels
)

# Display the interactive map (will open in RStudio Viewer or your web browser)
contamination_map

# The Moran's I results will also be printed directly to your R console.
```


