if (!exists("town.geo")) {
  town.geo <- rgdal::readOGR("Mass211/data/town.geojson", "OGRGeoJSON")
}

# color pallete
pal <- colorNumeric("Blues", NULL)

leaflet(town.geo) %>%
  setView(-71.1, 42, zoom = 7) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    stroke = FALSE,
    smoothFactor = 0.3,
    fillOpacity = 1,
    fillColor = ~ pal(log10(TotalPop)),
    label = ~paste0(town, ": ", formatC(TotalPop, big.mark = ","))
  )


## For UI ------