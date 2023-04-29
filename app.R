# Load required libraries
library(shiny)
library(tidyverse)
library(leaflet)
library(httr)
library(jsonlite)
library(sf)
library(viridis)
library(raster)
library(spatstat)
library(shinyWidgets)

# Load external files
source("config.R")
source("businesses_data_access.R")
source("businesses_data_process.R")
source("demographic_data_process.R")

# Define the user interface for the app
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("density_location", label = "Location", placeholder = "Try Houston"),
      textInput("density_category", label = "Category", placeholder = "Try italian"),
      actionButton("density_button", "Submit"),
      br(),
      helpText("Show demographic maps.
               Only available for the US."),
      switchInput("demo_maps", label = "Demo Map", value = FALSE),
      conditionalPanel(
        condition = "input.demo_maps == true"#,
        # selectInput("selected_state", "Select a State", choices = states, selected = NULL),
        # selectizeInput("selected_county", "Select a County", choices = NULL, selected = NULL,
        #                options = list(placeholder = "Type or select a county"))
      )
    ),
    mainPanel(
      width = 9,
      fluidRow(
        conditionalPanel(
          condition = "input.demo_maps == false",
          leafletOutput("single_map", width = "100%", height = "800px")
        ),
        conditionalPanel(
          condition = "input.demo_maps == true",
          column(width = 6, leafletOutput("density_map", width = "100%", height = "800px")),
          column(width = 6, leafletOutput("demographic_map", width = "100%", height = "800px"))
        )
      )
    )
  )
)

# Define the server logic for the app
server <- function(input, output, session) {

  # Fetch business data when the submit button is clicked
  results <- eventReactive(input$density_button, {
    req(input$density_location, input$density_category)
    if (input$density_location == "" || input$density_category == "") {
      showNotification("Please provide both location and category.", type = "warning")
      return(NULL)
    }
    showNotification("Loading businesses data...\n\nPlease wait", id =  "yelp_data",duration = NULL, closeButton = FALSE)
    fetch_all_yelp_data(yelp_api_key, input$density_location, input$density_category)
  })

  # Create heatmap_raster, pal, demo_layers, and demo_palettes reactive expressions
  heatmap_raster <- reactive(create_density_map(businesses = results()))
  pal <- reactive(create_hm_color_palette(heatmap_raster = heatmap_raster()))
  demo_layers <- reactive(get_demo_layers(input$selected_state, input$selected_county))
  demo_palettes <- reactive(define_palettes(demo_layers()))

  # Create a density map with business data and heatmap
  density_map <- reactive({
    req(results())
    req(heatmap_raster())
    removeNotification(id = "yelp_data")
    leaflet() |>
      addTiles() |>
      addRasterImage(heatmap_raster(), colors = pal(), opacity = 0.7, group = "Heatmap") |>
      addCircleMarkers(data = results(), lng = ~lon, lat = ~lat, radius = 3, color = "#CC2014", stroke = FALSE, fillOpacity = .7,
                       group = "Businesses",
                       popup = paste0("<strong>Name:</strong> ", results()$name, "<br>",
                                      "<strong>Rating:</strong> ", results()$rating, "<br>",
                                      "<strong>Categories:</strong> ", str_flatten_comma(results()$categories[[1]]$title))) |>
      addLayersControl(overlayGroups = c("Businesses", "Heatmap"), options = layersControlOptions(collapsed = FALSE))
  })

  # Create a demographic map with Census data
  demo_map <- reactive({
    req(input$demo_maps)
    req(results())
    showNotification("Loading demographic data...\n\nPlease wait", id =  "census_data",duration = NULL, closeButton = FALSE)
    demo_location <- get_state_county(results())
    demo_layers <- get_demo_layers(selected_state = demo_location$state,
                                   selected_county = demo_location$county)
    demo_pal <- define_palettes(demo_layers)
    removeNotification(id = "census_data")
    map <- leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addPolygons(data = demo_layers, group = "Population Density", color = "#444444", weight = .8, smoothFactor = .4,
                  opacity = 0.2, fillOpacity = 1,
                  fillColor = ~demo_pal$density_palette(density)) %>%
      addPolygons(data = demo_layers, group = "Mean Income", color = "#444444", weight = .8, smoothFactor = .4,
                  opacity = 0.2, fillOpacity = 1,
                  fillColor = ~demo_pal$income_palette(mean_income)) %>%
      addLayersControl(
        baseGroups = c("Population Density", "Mean Income", "OpenStreetMap"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(pal = demo_pal$density_palette, values = demo_layers$density, title = "Population Density",
                position = "bottomright") %>%
      addLegend(pal = demo_pal$income_palette, values = demo_layers$mean_income, title = "Mean Income",
                position = "bottomright")
  })

  # Render leaflet outputs
  output$single_map <- renderLeaflet(density_map())
  output$density_map <- renderLeaflet(density_map())
  output$demographic_map <- renderLeaflet(demo_map())

  # Reactive values for map coordinates
  map_coords <- reactiveValues(coords = NULL)

  # Observers to sync the bounds of density and demographic maps
  observe({
    coords <- input$density_map_bounds
    if (!is.null(coords)) {
      map_coords$coords <- coords
    }
  })
  observe({
    coords <- input$demographic_map_bounds
    if (!is.null(coords)) {
      map_coords$coords <- coords
    }
  })

  # Fit the bounds of the maps to the user's viewport
  observe({
    req(map_coords$coords)
    coords <- map_coords$coords
    leafletProxy("demographic_map") %>%
      fitBounds(coords$west,
                coords$south,
                coords$east,
                coords$north)
    leafletProxy("density_map") %>%
      fitBounds(coords$west,
                coords$south,
                coords$east,
                coords$north)
  })
}

# Run the app
shinyApp(ui, server)
