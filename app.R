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
# yelp and census.gov api keys  loaded from config.R
# to get your own yelp api_key follow the instruction @ https://docs.developer.yelp.com/docs/fusion-authentication
# to get your own census.gov api_key visit http://api.census.gov/data/key_signup.html
source("config.R")
source("businesses_data_access.R")
source("businesses_data_process.R")
source("demographic_data_process.R")




# states <- tidycensus::fips_codes %>%
#   dplyr::select(state_name) %>%
#   unique() |>
#   pull(state_name)

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
          leafletOutput("single_map", width = "100%", height = "600px")
        ),
        conditionalPanel(
          condition = "input.demo_maps == true",
          column(width = 6, leafletOutput("density_map", width = "100%", height = "600px")),
          column(width = 6, leafletOutput("demographic_map", width = "100%", height = "600px"))
        )
      )
    )
  )
)

server <- function(input, output, session) {

  results <- eventReactive(input$density_button, {
    req(input$density_location, input$density_category)
    if (input$density_location == "" || input$density_category == "") {
      showNotification("Please provide both location and category.", type = "warning")
      return(NULL)
    }
    fetch_all_yelp_data(yelp_api_key, input$density_location, input$density_category)
  })

  heatmap_raster <- reactive(create_density_map(businesses = results()))

  pal <- reactive(create_hm_color_palette(heatmap_raster = heatmap_raster()))

  demo_layers <- reactive(get_demo_layers(input$selected_state, input$selected_county))

  demo_palettes <- reactive(define_palettes(demo_layers()))


#
#   counties <- reactive({
#     tidycensus::fips_codes %>%
#       dplyr::filter(state_name == input$selected_state) %>%
#       dplyr::pull(county)
#   })
#
#   observe({
#     updateSelectizeInput(session, "selected_county", choices = counties(), selected = NULL, server = TRUE)
#   })

  density_map <- reactive({
    req(results())
    req(heatmap_raster())
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

  demo_map <- reactive({
    req(input$demo_maps)
    req(results())
    demo_location <- get_state_county(results())
    demo_layers <- get_demo_layers(selected_state = demo_location$state,
                                   selected_county = demo_location$county)
    demo_pal <- define_palettes(demo_layers)
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addPolygons(data = demo_layers, group = "Population Density", color = "#444444", weight = .8, smoothFactor = .4,
                  opacity = 0.2, fillOpacity = 1,
                  fillColor = ~demo_pal$density_pallete(density)) %>%
      addPolygons(data = demo_layers, group = "Mean Income", color = "#444444", weight = .8, smoothFactor = .4,
                  opacity = 0.2, fillOpacity = 1,
                  fillColor = ~demo_pal$income_pallete(mean_income)) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Population Density", "Mean Income"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(pal = demo_pal$density_pallete, values = demo_layers$density, title = "Population Density",
                position = "bottomright") %>%
      addLegend(pal = demo_pal$income_pallete, values = demo_layers$mean_income, title = "Mean Income",
                position = "bottomright")
  })

  output$single_map <- renderLeaflet(density_map())

  output$density_map <- renderLeaflet(density_map())

  output$demographic_map <- renderLeaflet(demo_map())
}

shinyApp(ui, server)
### simplificar shape
### Bug: category sin price
### Outliers locations
### sidepanel height
### maps sync
### enter submit
