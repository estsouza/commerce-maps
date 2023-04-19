

# Define the variables to retrieve
variables <- c(
  total_pop = "B01003_001",
  mean_income = "B19013_001"
  # age_0_17 = "B01001_003",
  # age_18_34 = "B01001_011",
  # age_35_64 = "B01001_019",
  # age_65_plus = "B01001_027"
)

get_demo_layers <- function(selected_state, selected_county, geography = "block group") {
  # Get the census.gov data for a specific state or county
  demo_layers <- get_acs(geography = geography,
                  variables = variables,
                  state = selected_state,
                  county = county,
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
  palette_income <- colorNumeric(palette = viridis(256), domain = demo_layers$mean_income*.8, na.color = "transparent")
  return(list(
    denisity_palette = palette_den,
    income_palette = palette_income
  ))
}


