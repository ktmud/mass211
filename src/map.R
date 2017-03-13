library(rgdal)
library(rgeos)
library(maptools)
library(geojsonio)
library(spdplyr)
library(rmapshaper)


# Projection for MassGIS data
# Ref: http://spatialreference.org/ref/sr-org/15/
#      http://www.mass.gov/anf/research-and-tech/it-serv-and-support/application-serv/office-of-geographic-information-massgis/datalayers/overview.html
proj4mass <- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")

ReadOGR <- function(dat, layer) {
  readOGR(dat, layer = layer) %>% spTransform(proj4mass)
}

# zip.geo <- readShapePoly("data/zipcodes/ZIPCODES_NT_POLY")
zip.geo <- ReadOGR("data/zipcodes", "ZIPCODES_NT_POLY")
#city.geo <- ReadOGR("data/towns", "TOWNSSURVEY_POLYM")
county.geo <- ReadOGR("data/counties", "COUNTIES_POLYM")


## Add geographic metrics
source("src/geo.count.R")

names(zip.geo) %<>% str_to_lower()
zip.geo %<>%
  rename(zip = postcode) %>%
  mutate(
    city_town = str_to_title(city_town),
    pa_name = str_to_title(pa_name),
    pc_name = str_to_title(pc_name),
    county = str_to_title(county)
  )

count.zip <- zip.geo@data %>%
  left_join(count.zip, by = "zip") %>%
  mutate(PopDen = TotalPop / area_sqmi)

zip.geo@data <- count.zip %>%
  select(zip, pa_name, county, TotalPop, total.minutes, p.call)

names(county.geo) %<>% str_to_lower()
county.geo %<>%
  mutate(
    county = str_to_title(county)
  )
county.geo@data <- county.geo@data %>% left_join(count.county, by = "county") %>%
  select(county, TotalPop, total.minutes, p.call)

# names(city.geo) %<>% str_to_lower()
# city.geo %<>%
#   mutate(
#     town = str_to_title(town)
#   )
# # zip.geojson <- geojson_json(zip.geo)
# city.geojson <- geojson_json(city.geo) %>% ms_simplify()
