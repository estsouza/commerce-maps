# install.packages("tidycensus")
require(tidyverse)
require(sf)
require(viridis)
require(leaflet)
source("config.R")
census_api_key(census.gov_api_key)

states <- tidycensus::fips_codes %>%
  dplyr::select(state_name, state) %>%
  unique() |>
  pull(state)

selected_state <- "CA"
counties <- tidycensus::fips_codes %>%
    dplyr::filter(state == selected_state) %>%
    dplyr::pull(county)
county <- counties[19]

# Define the variables you want to retrieve
variables <- c(
  total_pop = "B01003_001",
  mean_income = "B19013_001"
  # age_0_17 = "B01001_003",
  # age_18_34 = "B01001_011",
  # age_35_64 = "B01001_019",
  # age_65_plus = "B01001_027"
)

# Get the data for a specific state or county
data <- get_acs(geography = "block group",
                variables = variables,
                state = selected_state,
                county = county,
                survey = "acs5",
                year = 2021,
                geometry = T)

# Calculate population density (assuming the area is in square miles)


leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
  addPolygons(data = data, group = "Population Density", color = "#444444", weight = .8, smoothFactor = .4,
              opacity = 0.2, fillOpacity = 1,
              fillColor = ~palette_den(density)) %>%
  addPolygons(data = data, group = "Mean Income", color = "#444444", weight = .8, smoothFactor = .4,
              opacity = 0.2, fillOpacity = 1,
              fillColor = ~palette_income(mean_income)) %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Population Density", "Mean Income"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(pal = palette_den, values = data$density, title = "Population Density",
            position = "bottomright") %>%
  addLegend(pal = palette_income, values = data$mean_income, title = "Mean Income",
            position = "bottomright")
