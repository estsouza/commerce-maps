# Fetch all Yelp business data for a given city and category.
#
# This function makes multiple calls to the Yelp API, which has a limit of 50 businesses per call,
# and binds all the results into a single tibble. It also calculates the centroid of the businesses
# and filters out points that fall outside of a certain range.
#
# Args:
#   api_key: Your Yelp API key.
#   city: The name of the city to fetch businesses data for.
#   category: The category of businesses to fetch data for.
#
# Returns:
#   A tibble containing the filtered businesses data for the specified city and category.
#
# Example:
#   businesses <- fetch_all_yelp_data(api_key, "Houston", "italian")
#
fetch_all_yelp_data <- function(api_key = api_key, city, term, limit = 50) {
  base_url <- "https://api.yelp.com/v3/businesses/search"
  response <- GET(url = base_url,
                  add_headers(Authorization = paste("Bearer", yelp_api_key)),
                  query = list(location = city, term = term, limit = limit, offset = 0))
  parsed_response <- fromJSON(content(response, type = "text"))
  total_results <- parsed_response$total

  num_calls <- ceiling(total_results / limit)
  if (num_calls >= 1000) {
    cat("API 50000 results limit reached. Try narrowing the results by location or category")
  }

  all_results <- as_tibble(parsed_response$businesses)
  for (i in 2:min(num_calls, 1001)) {
    offset <- (i - 1) * limit

    response <- GET(url = base_url,
                    add_headers(Authorization = paste("Bearer", api_key)),
                    query = list(location = city, term = term, limit = limit, offset = offset))
    parsed_response <- fromJSON(content(response, as = "text"))
    page <- as_tibble(parsed_response$businesses)
    all_results <- all_results |> bind_rows(page)
  }

  all_results <- all_results |>
    # dplyr::select(id, alias, name, review_count, rating, categories, coordinates) |>
    mutate(lon = coordinates$longitude,
           lat = coordinates$latitude) %>%
    dplyr::select(-coordinates)
  centroid_lon <- median(all_results$lon)
  centroid_lat <- median(all_results$lat)
  all_results <- all_results |>
    filter(between(lon, centroid_lon - 0.25, centroid_lon + 0.25),
           between(lat, centroid_lat - 0.25, centroid_lat + 0.25))

  return(all_results)
}
