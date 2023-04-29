# Create a density map based on the coordinates of businesses.
#
# Args:
#   businesses: A tibble containing business data with 'lon' and 'lat' columns for coordinates.
#
# Returns:
#   A raster object representing the density map of the businesses.
#
create_density_map <- function(businesses) {

  offset <- 0.1
  window <- owin(xrange = c(min(businesses$lon) - offset, max(businesses$lon) + offset),
                 yrange = c(min(businesses$lat) - offset, max(businesses$lat) + offset))
  pts <- ppp(x = businesses$lon, y = businesses$lat, window = window)
  ds <- density.ppp(pts, sigma = 0.02, kernel = "quartic")

  heatmap_raster <- raster(ds)
  raster::crs(heatmap_raster) <- "EPSG:4326"

  return(heatmap_raster)
}

# Create a color palette for the heatmap.
#
# Args:
#   heatmap_raster: A raster object representing the density map of the businesses.
#
# Returns:
#   A color palette function to be used with the heatmap.
#
create_hm_color_palette <- function(heatmap_raster) {
  colors <- rev(c('#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac'))
  pal <- colorNumeric(colors, values(heatmap_raster),
                      na.color = "transparent")
  return(pal)
}

