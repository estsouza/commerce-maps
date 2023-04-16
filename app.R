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
