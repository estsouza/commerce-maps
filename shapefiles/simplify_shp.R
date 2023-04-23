counties_sf <- sf::st_read("./shapefiles/tl_2020_us_county.shp") |> st_transform(crs = 4326)
counties_sf |>
  dplyr::select(STATEFP, COUNTYFP) |>
  st_simplify(preserveTopology = T, dTolerance = 10) |>
  st_write("./shapefiles/counties_us.shp")
