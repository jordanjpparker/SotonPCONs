library(shiny)
library(sf)
library(leaflet)
library(httr)

endpoint <- 'https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Wards_December_2022_Boundaries_GB_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'

ui <- fluidPage(
  leafletOutput("map"),
  selectizeInput(inputId = "selected_locations",
                 label = "Selected:",
                 choices = nc$NAME,
                 selected = NULL,
                 multiple = TRUE),
  actionButton("save", "Save Selection"),
  actionButton("merge", "Merge Selection"),
  actionButton("export", "Export as Geopackage"),
  tableOutput("selected_features")
)

server <- function(input, output) {
  
  # Load the data
  data <- st_read(content(GET(endpoint), 'text'))
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = data, highlightOptions = highlightOptions(color = "red", 
                                                                   weight = 2))
  })
  
  # Create a reactive variable to store the selected features
  selected_features <- reactiveValues(data = NULL)
  
  # Save button
  observeEvent(input$save, {
    selected_features$data <- leafletProxy("map") %>%
      selectedFeatures()
  })
  
  # Merge button
  observeEvent(input$merge, {
    selected_features$data <- st_union(selected_features$data)
  })
  
  # Export button
  observeEvent(input$export, {
    st_write(selected_features$data, "path/to/export.gpkg", layer_options = "GEOMETRY_NAME=geometry")
  })
  
  # Display the selected features in a table
  output$selected_features <- renderTable({
    selected_features$data
  })
}

shinyApp(ui, server)
