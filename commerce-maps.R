library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(tidyverse)
library(sf)
library(purrr)
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
  select(id, alias, name, review_count, rating, categories, price, coordinates) |>
  mutate(lon = coordinates$longitude,
         lat = coordinates$latitude) %>%
  select(-coordinates)
businesses <- st_as_sf(results, coords = c("lon", "lat"), crs = 4326)
plot(businesses)

# Define the color gradient for the heatmap
gradient_colors <- scale_fill_gradientn(
  colors = c("blue", "green", "yellow", "red")
)
heatmap_plot <- ggplot(data = results, aes(x = lon, y = lat)) +
  geom_density_2d_filled(n = 500, h = 0.4) +
  geom_point() +
  scale_color_viridis_c() +
  coord_cartesian() +
  theme_minimal() +
  labs(title = "Business Heatmap", x = "Longitude", y = "Latitude")
plot(heatmap_plot)


businesses <- businesses |> st_transform(crs = 3857)
owin <- as.owin(st_bbox(businesses))
pts <- as.ppp(businesses)
ds <- density.ppp(pts)
plot(ds)

heatmap_raster <- raster(ds)
raster::crs(heatmap_raster) <- "EPSG:3857"
plot(heatmap_raster)
# library(png)
# heatmap_img <- terra::plotRGB(heatmap_raster, col = viridis(256, alpha = 0.8), useRaster = FALSE)
# writePNG(heatmap_img, "heatmap.png")
# colormap <- viridis(256, alpha = 0.8)
# pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(heatmap_raster),
#                     na.color = "transparent")
leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addRasterImage(heatmap_raster, colors = viridis(256), opacity = 0.8)


leaflet() %>% addTiles() %>%
  addRasterImage(heatmap_raster, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(heatmap_raster),
            title = "Businesses density")
