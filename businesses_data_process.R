create_density_map <- function(businesses) {

  offset <- 0.1
  window <- owin(xrange = c(min(results$lon) - offset, max(results$lon) + offset),
                 yrange = c(min(results$lat) - offset, max(results$lat) + offset))
  pts <- ppp(x = results$lon, y = results$lat, window = window)
  ds <- density.ppp(pts, sigma = 0.02, kernel = "quartic")

  heatmap_raster <- raster(ds)
  raster::crs(heatmap_raster) <- "EPSG:4326"

  return(heatmap_raster)
}

create_hm_color_palette <- function(heatmap_raster) {
  colors <- rev(c('#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac'))
  pal <- colorNumeric(colors, values(heatmap_raster),
                      na.color = "transparent")
  return(pal)
}

