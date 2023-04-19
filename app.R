library(shiny)
library(tidyverse)
library(leaflet)
library(httr)
library(jsonlite)
library(sf)
library(viridis)
library(raster)
library(spatstat)

# yelp and census.gov api keys  loaded from config.R
# to get your own yelp api_key follow the instruction @ https://docs.developer.yelp.com/docs/fusion-authentication
# to get your own census.gov api_key visit http://api.census.gov/data/key_signup.html
source("config.R")
source("businesses_data_access.R")
source("businesses_data_process.R")
source("demographic_data_process.R")

results <- fetch_all_yelp_data(api_key, city, term)

heatmap_raster <- create_density_map(businesses = results)
pal <- create_hm_color_palette(heatmap_raster = heatmap_raster)

demo_layers <- get_demo_layers(input$selected_state, input$selected_county)
demo_palettes <- define_palettes(demo_layers)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 3,

      switchInput("split_maps", label = "Split maps", value = FALSE)
    ),
    mainPanel(
      width = 9,
      fluidRow(
        conditionalPanel(
          condition = "input.split_maps == false",
          leafletOutput("single_map", width = "100%", height = "600px")
        ),
        conditionalPanel(
          condition = "input.split_maps == true",
          column(width = 6, leafletOutput("density_map", width = "100%", height = "600px")),
          column(width = 6, leafletOutput("demographic_map", width = "100%", height = "600px"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$single_map <- renderLeaflet({
    leaflet() %>% addTiles()
  })

  output$density_map <- renderLeaflet({
    leaflet() %>% addTiles()
  })

  output$demographic_map <- renderLeaflet({
    leaflet() %>% addTiles()
  })
}

shinyApp(ui, server)



states <- tidycensus::fips_codes %>%
  dplyr::select(state_name) %>%
  unique() |>
  pull(state_name)
#
# selected_state_code <- reactive({input$selected_state})
# counties <- reactive({
#   tidycensus::tigris::fips_codes %>%
#     dplyr::filter(state == selected_state_code(), entity == "county") %>%
#     dplyr::select(county_code = fips, county_name = name)
# })

ui <- fluidPage(
  titlePanel("US States and Counties"),

  sidebarLayout(
    sidebarPanel(
      selectInput("selected_state", "Select a State", choices = states, selected = "California"),
      selectizeInput("selected_county", "Select a County", choices = NULL, selected = NULL,
                     options = list(placeholder = "Type or select a county"))
    ),

    mainPanel(
      # You can display the results here
    )
  )
)

server <- function(input, output, session) {
  counties <- reactive({
    tidycensus::fips_codes %>%
      dplyr::filter(state_name == input$selected_state) %>%
      dplyr::pull(county)
  })

  observe({
    updateSelectizeInput(session, "selected_county", choices = counties(), selected = NULL, server = TRUE)
  })

}
