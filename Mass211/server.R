library(shiny)
library(plotly)
library(leaflet)
library(RColorBrewer)

source("init.R")
source("plot/sankey.R")
source("plot/timeline.R")
source("plot/map.R")

server <- function(input, output, session) {
  
  cdata <- session$clientData
  sk <- DatSankey(mcalls)
  
  output$callflow_sankey <- renderSankeyNetwork({
    sankeyNetwork(
      Links = sk$links,
      Nodes = sk$nodes,
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      linkOpacity = 0.08,
      colourScale = "d3.scaleOrdinal(d3.schemeCategory10)",
      units = "calls",
      margin = list(left = 0, right = 0, top = 0, bottom = 0),
      fontSize = 10,
      nodePadding = 9,
      nodeWidth = 40
    )
  })
  
  output$main_timeline <- renderPlotly({
    PlotMainTimeline(mcalls2, input$time_unit)
  })
  
  
  # ==== Map ---------------
  
  # Reactive expression for the data subsetted to what the user selected
  map_data <- reactive({
    geo_unit <- input$geo_unit
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  output$main_map <- renderLeaflet({
    PlotMainMap(town.geo)
    leaflet(quakes) %>%
      addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
}