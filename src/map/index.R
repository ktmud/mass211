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

## Add geographic metrics
source("src/map/county.R")
source("src/map/town.R")
source("src/map/zip.R")
