source('src/utils/geo.R')

count.town <- GeoCount(mcalls, "town", TRUE)
count.zip <- GeoCount(mcalls, "zip", TRUE)
count.county <- GeoCount(mcalls, "county", TRUE)