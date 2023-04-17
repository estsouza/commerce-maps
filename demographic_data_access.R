# install.packages("tidycensus")
require(tidycensus)
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
county <- counties[1]

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

leaflet() |>
  addTiles() |>
  addPolygons(demo, color = "#444444", weight = 1, smoothFactor = 0.5,
            opacity = 1.0, fillOpacity = 0.5)
plot(demo)
