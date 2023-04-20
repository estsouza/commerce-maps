fetch_all_yelp_data <- function(api_key = api_key, city, term, limit = 50) {
  base_url <- "https://api.yelp.com/v3/businesses/search"
  # First API call to get the total number of results
  response <- GET(url = base_url,
                  add_headers(Authorization = paste("Bearer", yelp_api_key)),
                  query = list(location = city, term = term, limit = limit, offset = 0))
  parsed_response <- fromJSON(content(response, type = "text"))
  total_results <- parsed_response$total

  # Calculate the number of required API calls
  num_calls <- ceiling(total_results / limit)
  if (num_calls >= 1000) {
    cat("API 50000 results limit reached. Try narrowing the results by location or category")
  }

  # Fetch the rest of the results with multiple API calls
  all_results <- as_tibble(parsed_response$businesses)
  # row.names(all_results) <- NULL
  for (i in 2:min(num_calls, 1001)) {
    offset <- (i - 1) * limit

    response <- GET(url = base_url,
                    add_headers(Authorization = paste("Bearer", api_key)),
                    query = list(location = city, term = term, limit = limit, offset = offset))
    parsed_response <- fromJSON(content(response, as = "text"))
    page <- as_tibble(parsed_response$businesses)
    all_results <- all_results |> bind_rows(page)
  }
  all_results <- all_results |> dplyr::select(id, alias, name, review_count, rating, categories, price, coordinates) |>
    mutate(lon = coordinates$longitude,
           lat = coordinates$latitude) %>%
    dplyr::select(-coordinates)
  return(all_results)
}
