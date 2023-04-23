require(tidycensus)

census_api_key(census.gov_api_key)

# Define the variables to retrieve
variables <- c(
  total_pop = "B01003_001",
  mean_income = "B19013_001"
)
counties_sf <- sf::st_read("./shapefiles/counties_us.shp")

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

get_demo_layers <- function(selected_state, selected_county, geography = "block group") {
  # Get the census.gov data for a specific state or county
  demo_layers <- get_acs(geography = geography,
                  variables = variables,
                  state = selected_state,
                  county = selected_county,
                  survey = "acs5",
                  year = 2021,
                  geometry = T)

  # Calculate population density
  demo_layers <- demo_layers |>
    pivot_wider(id_cols = c("GEOID", "geometry"),names_from = "variable", values_from = "estimate")
  demo_layers$area_km2 <- as.numeric(st_area(demo_layers)) / 1000000
  demo_layers$density <- demo_layers$total_pop / demo_layers$area_km2

  demo_layers
}

define_palettes <- function(demo_layers) {
  palette_den <- colorNumeric(palette = viridis(256), domain = demo_layers$density*.6, na.color = "transparent")
  palette_income <- colorNumeric(palette = viridis(256), domain = demo_layers$mean_income*.8, na.color = "black")
  return(list(
    density_palette = palette_den,
    income_palette = palette_income
  ))
}
