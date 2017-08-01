#
# Process and export county metric geojson
#
county.geo <- ReadOGR("data/counties", "COUNTIES_POLYM")
county.hsi <- read_csv("data/health-indicators/hsi.county.csv") %>%
  select(-white, -black, -hispanic, -asian)
names(county.geo) %<>% str_to_lower()
county.geo@data %<>%
  mutate(county = str_to_title(county)) %>%
  select(name = county, shape_area) %>%
  left_join(count.county, by = "name") %>%
  left_join(county.hsi, "name") %>%
  mutate(
    PopDen = TotalPop / (shape_area * 3.86102159 * 10e-7)
  ) %>%
  standardize("PopDen")

county.geo.simple <- ms_simplify(county.geo)

# round numbers
num_cols <- 5:ncol(county.geo.simple@data)
county.geo.simple@data[, num_cols] <- round(
  county.geo.simple@data[, num_cols], digits=6)

writeOGR(county.geo.simple,
         "Mass211/data/county.json",
         layer = "county_POLY",
         driver = "GeoJSON")