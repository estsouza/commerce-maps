# install.packages("tidycensus")
require(tidycensus)

states <- tidycensus::fips_codes %>%
  dplyr::select(state_name) %>%
  unique() |>
  pull(state_name)

selected_state <- "California"
counties <- tidycensus::fips_codes %>%
    dplyr::filter(state_name == selected_state) %>%
    dplyr::pull(county)
county <- counties[1]

orange <- get_acs(
  state = "CA",
  county = "Orange",
  geography = "tract",
  variables = "B19013_001",
  geometry = TRUE,
  year = 2020
)
