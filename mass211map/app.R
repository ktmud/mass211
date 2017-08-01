library(shiny)
library(leaflet)
library(RColorBrewer)

if (!exists("town.geo")) {
  town.geo <- rgdal::readOGR("Mass211/data/town.geojson", "OGRGeoJSON")
}


## For UI ------
map_select_geo <- selectInput(
  "map_data",
  label = "Geographic Level",
  selected = "town.geo",
  choices = c("City/Town" = "town.geo",
              "ZIP Code" = "zip.geo",
              "County" = "county.geo"),
  selectize = FALSE
)
map_select_variable <- selectInput(
  "variable",
  label = "Select Variable",
  selected = "p_call",
  choices = names(
    town.geo %>% select(-name, -shape_area, -FIPS_ID)
  )
)
  
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10,
                right = 10,
                map_select_geo,
                map_select_variable)
)

server <- function(input, output, session) {
  
  # color pallete
  pal <- colorNumeric("Blues", NULL)
  
  # Reactive expression for the data subsetted to what the user selected
  map_data <- reactive({
    # get the data from environment variable
    get(input$map_data)
  })
  
  output$map <- renderLeaflet({
    leaflet(county.geo) %>%
      setView(-71.70, 42.1, zoom = 9) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        weight = 0.5,
        smoothFactor = 0.3,
        color = "#333",
        fillOpacity = 0.8,
        fillColor = "#e0ecf4",
        group = "base"
      )
  })
  
  observe({
    key <- input$variable
    map.geo <- map_data()
    field.vals <- map.geo@data[[key]]
    field.names <- map.geo@data$name
    leafletProxy("map", data = map.geo) %>%
      clearGroup('active') %>%
      addPolygons(
        stroke = FALSE,
        smoothFactor = 0.3,
        fillOpacity = 1,
        fillColor = pal(log1p(field.vals)),
        label = paste0(field.names, ": ", format(field.vals, big.mark = ",")),
        group = 'active'
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  # observe({
  #   proxy <- leafletProxy("map", data = map_data())
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   proxy %>% addLegend(position = "bottomright",
  #                       pal = pal,
  #                       values = ~ log10(PopDen))
  # })
}

shinyApp(ui, server)