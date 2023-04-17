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

# yelp and census.gov api keys  loaded from config.R
# to get your own yelp api_key follow the instruction @ https://docs.developer.yelp.com/docs/fusion-authentication
# to get your own census.gov api_key visit http://api.census.gov/data/key_signup.html
source("config.R")
source("businesses_data_access.R")
source("businesses_data_process.R")

# Example usage
city <- "Gold Coast"
term <- "pizza"
results <- fetch_all_yelp_data(api_key, city, term)
# businesses <- st_as_sf(results, coords = c("lon", "lat"), crs = 4326) # |> st_transform(crs = 3857)

heatmap_raster <- create_density_map(businesses = results)
pal <- create_hm_color_palette(heatmap_raster = heatmap_raster)


leaflet() |>
  addTiles() |>
  addRasterImage(heatmap_raster, colors = pal, opacity = 0.7, group = "Heatmap") |>
  addCircleMarkers(data = results, lng = ~lon, lat = ~lat, radius = 3, color = "#CC2014", stroke = FALSE, fillOpacity = .7,
                   group = "Businesses",
                   popup = paste0("<strong>Name:</strong> ", results$name, "<br>",
                                  "<strong>Rating:</strong> ", results$rating, "<br>",
                                  "<strong>Categories:</strong> ", str_flatten_comma(results$categories[[1]]$title))) |>
  addLayersControl(overlayGroups = c("Businesses", "Heatmap"), options = layersControlOptions(collapsed = FALSE))# |>
  # addLegend(pal = pal, values = values(heatmap_raster),
  #           title = "Businesses heatmap")
