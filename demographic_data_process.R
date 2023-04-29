# Load required packages
require(tidycensus)

# Set API key for Census API
census_api_key(census.gov_api_key)

# Define the variables to retrieve
variables <- c(
  total_pop = "B01003_001",
  mean_income = "B19013_001"
)

# Read shapefile for US counties
counties_sf <- sf::st_read("./shapefiles/counties_us.shp")

# Get the state and county codes for the centroid of the businesses.
#
# Args:
#   businesses: A tibble containing business data with 'lon' and 'lat' columns for coordinates.
#
# Returns:
#   A list with 'state' and 'county' codes, or NULL if no businesses are provided.
#
get_state_county <- function(businesses) {
  if (nrow(businesses) > 0) {
    centroid_lon <- median(businesses$lon)
    centroid_lat <- median(businesses$lat)
    centroid <- st_as_sf(tibble(lon = centroid_lon, lat = centroid_lat),
                         coords = c("lon", "lat"),
                         crs = 4326)

    county_result <- st_join(centroid, counties_sf)
    list(state = county_result$STATEFP[1], county = county_result$COUNTYFP[1])
  } else {
    NULL
  }
}

# Get the demographic layers for the given state and county.
#
# Args:
#   selected_state: The state code.
#   selected_county: The county code.
#   geography: The geography level for the ACS data (default: "block group").
#
# Returns:
#   A spatial dataframe containing the demographic data.
#
get_demo_layers <- function(selected_state, selected_county, geography = "block group") {
  demo_layers <- get_acs(geography = geography,
                  variables = variables,
                  state = selected_state,
                  county = selected_county,
                  survey = "acs5",
                  year = 2021,
                  geometry = T)
  demo_layers <- demo_layers |>
    pivot_wider(id_cols = c("GEOID", "geometry"),names_from = "variable", values_from = "estimate")
  demo_layers$area_km2 <- as.numeric(st_area(demo_layers)) / 1000000
  demo_layers$density <- demo_layers$total_pop / demo_layers$area_km2

  demo_layers
}

# Define color palettes for the demographic layers.
#
# Args:
#   demo_layers: A spatial dataframe containing the demographic data.
#
# Returns:
#   A list with two color palette functions for density and income.
#
define_palettes <- function(demo_layers) {
  palette_den <- colorNumeric(palette = viridis(256), domain = demo_layers$density*.6, na.color = "transparent")
  palette_income <- colorNumeric(palette = viridis(256), domain = demo_layers$mean_income*.8, na.color = "black")
  return(list(
    density_palette = palette_den,
    income_palette = palette_income
  ))
}
