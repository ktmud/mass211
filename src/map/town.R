##
# Process Data Bindings for towns
##
if (!exists("town.geo.raw")) {
  town.geo.raw <- ReadOGR("data/towns", "TOWNS_POLYM") 
}
town.hsi <- read_csv("data/health-indicators/hsi.town.csv") %>%
  select(-white, -black, -hispanic, -asian)

town.geo <- town.geo.raw
names(town.geo) %<>% str_to_lower()
town.geo@data %<>%
  mutate(town = str_to_title(town)) %>%
  select(name = town, shape_area)

town.geo@data %<>%
  left_join(count.town, by = "name") %>%
  left_join(town.hsi, "name") %>%
  mutate(
    PopDen = TotalPop / (shape_area * 3.86102159 * 10e-7)
  ) %>%
  standardize("PopDen")


town.geo.simple <- ms_simplify(town.geo)
# round numbers
num_cols <- 5:ncol(town.geo.simple@data)
town.geo.simple@data[, num_cols] <- round(
  town.geo.simple@data[, num_cols], digits=6)

writeOGR(town.geo.simple,
         "Mass211/data/town.json",
         layer = "TOWNS_POLYM",
         driver = "GeoJSON")
