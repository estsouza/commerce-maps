# install.packages("tidycensus")
require(tidyverse)
require(sf)
require(tidycensus)
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

demo <- get_acs(
  state = selected_state,
  county = county,
  geography = "tract",
  variables = "B19013_001",
  geometry = TRUE,
  year = 2021
) |> st_transform(crs= 4326)

ggplot(demo, aes(fill = estimate, color = estimate)) +
  geom_sf() +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "magma")

palette <- colorNumeric(palette = viridis(256), domain = demo$estimate, na.color = "transparent")

leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = demo, color = "#444444", weight = .8, smoothFactor = .5,
              opacity = 0.2, fillOpacity = 1,
              fillColor = ~palette(estimate)) %>%
  addLegend(pal = palette, values = demo$estimate, title = "Estimate")

acs_variables <- load_variables(year = 2021, dataset = "acs5", cache = TRUE)


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
data <- data |>
  pivot_wider(id_cols = c("GEOID", "geometry"),names_from = "variable", values_from = "estimate")
data$area_km2 <- as.numeric(st_area(data)) / 1000000
data$density <- data$total_pop / data$area_km2

palette_den <- colorNumeric(palette = viridis(256), domain = data$density*.7, na.color = "transparent")
palette_income <- colorNumeric(palette = viridis(256), domain = data$mean_income, na.color = "transparent")

leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = data, color = "#444444", weight = .8, smoothFactor = .5,
              opacity = 0.2, fillOpacity = 1,
              fillColor = ~palette_den(density)) %>%
  addLegend(pal = palette_den, values = data$density, title = "Estimate")

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
