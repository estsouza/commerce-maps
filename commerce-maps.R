library(shiny)
library(tidyverse)
library(leaflet)
library(httr)
library(jsonlite)

library(sf)
# library(purrr)
library(viridis)
library(raster)
# install.packages("spatstat")
library(spatstat)
# devtools::install_github("OmaymaS/yelpr")
# library(yelpr)

# yelp api credentials loaded from config.R
# to get your own yelp api_key follow the instruction @ https://docs.developer.yelp.com/docs/fusion-authentication
source("config.R")

fetch_all_yelp_data <- function(api_key = api_key, city, term, limit = 50) {
  base_url <- "https://api.yelp.com/v3/businesses/search"
  # First API call to get the total number of results
  response <- GET(url = base_url,
                  add_headers(Authorization = paste("Bearer", api_key)),
                  query = list(location = city, term = term, limit = limit, offset = 0))
  parsed_response <- fromJSON(content(response, type = "text"))
  total_results <- parsed_response$total

  # Calculate the number of required API calls
  num_calls <- ceiling(total_results / limit)

  # Fetch the rest of the results with multiple API calls
  all_results <- as_tibble(parsed_response$businesses)
  # row.names(all_results) <- NULL
  for (i in 2:num_calls) {
    offset <- (i - 1) * limit

    # Break the loop if the offset is greater than 1000, as the Yelp API has a hard limit of 1000 results
    if (offset >= 1000) {
      cat("API 1000 results limit reached. Try narrowing the results by location or category")
      break
    }

    response <- GET(url = base_url,
                    add_headers(Authorization = paste("Bearer", api_key)),
                    query = list(location = city, term = term, limit = limit, offset = offset))
    parsed_response <- fromJSON(content(response, as = "text"))
    page <- as_tibble(parsed_response$businesses)
    all_results <- all_results |> bind_rows(page)
  }

  return(all_results)
}

# Example usage
city <- "Gold Coast"
term <- "pizza"
results <- fetch_all_yelp_data(api_key, city, term) |>
  dplyr::select(id, alias, name, review_count, rating, categories, price, coordinates) |>
  mutate(lon = coordinates$longitude,
         lat = coordinates$latitude) %>%
  dplyr::select(-coordinates)
businesses <- st_as_sf(results, coords = c("lon", "lat"), crs = 4326) |> st_transform(crs = 3857)

# owin <- as.owin(st_bbox(businesses))
pts <- as.ppp(businesses)
ds <- density.ppp(pts)
plot(ds)

heatmap_raster <- raster(ds)
raster::crs(heatmap_raster) <- "EPSG:3857"
colors <- c("#0081a7", "#00afb9", "#fdfcdc", "#fed9b7", "#f07167")
# breaks <- seq(minValue(heatmap_raster), quantile(heatmap_raster, probs = .9, ), length.out = length(colors) + 1)
# pal <- colorRampPalette(colors)
# colortable <- pal(length(breaks) - 1)
pal <- colorNumeric(colors, values(heatmap_raster),
                    na.color = "transparent")


leaflet() |>
  addTiles() |>
  addRasterImage(heatmap_raster, colors = pal, opacity = 0.7, group = "Heatmap") |>
  addCircleMarkers(data = results, lng = ~lon, lat = ~lat, radius = 3, color = "#CC2014", stroke = FALSE, fillOpacity = .5,
                   group = "Businesses",
                   popup = paste0("<strong>Name:</strong> ", results$name, "<br>",
                                  "<strong>Rating:</strong> ", results$rating, "<br>",
                                  "<strong>Categories:</strong> ", str_flatten_comma(results$categories[[1]]$title))) |>
  addLayersControl(overlayGroups = c("Businesses", "Heatmap"), options = layersControlOptions(collapsed = FALSE))# |>
  # addLegend(pal = pal, values = values(heatmap_raster),
  #           title = "Businesses heatmap")

