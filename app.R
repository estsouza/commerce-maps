library(shiny)
library(leaflet)

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
