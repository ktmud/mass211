library(plyr)
library(tidyverse)
library(magrittr)
library(stringr)
library(readxl)
library(xml2)
# library(data.tree)
# library(forcats)
library(lubridate)

source('src/utils/helpers.R')
source('src/utils/topics.R')

# source('src/clean/complex_1.R')
# source('src/clean/complex_2.R')
source('src/clean/resources.R')

source('src/clean/simple.R')
source('src/clean/old.R')
source('src/clean/combine.R')

source('src/transform/count_geo.R')
source('src/transform/count_cat_geo.R')
source('src/transform/count_resource.R')
source('src/map/index.R')

try({
  writeOGR(county.geo.simple,
           "m2m/static/data/county.json", layer = "county_POLY",
           check_exists = FALSE,
           driver = "GeoJSON")
  writeOGR(zip.geo.simple,
           "m2m/static/data/zip.json", layer = "ZIP_POLY",
           check_exists = FALSE,
           driver = "GeoJSON")
  writeOGR(town.geo.simple,
           "m2m/static/data/town.json", layer = "TOWNS_POLYM",
           check_exists = FALSE,
           driver = "GeoJSON")
})