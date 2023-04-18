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

palette <- colorNumeric(palette = viridis(256), domain = demo$estimate, na.color = "transparent")

leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = demo, color = "#444444", weight = .8, smoothFactor = .5,
              opacity = 0.2, fillOpacity = 1,
              fillColor = ~palette(estimate)) %>%
  addLegend(pal = palette, values = demo$estimate, title = "Estimate")
