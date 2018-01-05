#
# Generate Geographic Unit mapping for the whole MA
#
library(spdplyr)

proj4mass <- sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
ReadOGR <- function(dat, layer) {
  rgdal::readOGR(dat, layer = layer) %>% sp::spTransform(proj4mass)
}
# us.zip <- rgdal::readOGR("data/census-zip", "cb_2016_us_zcta510_500k")
if (!exists("zip.geo.raw")) {
  zip.geo.raw <- ReadOGR("data/zipcodes", "ZIPCODES_NT_POLY")
}

zip_town_county <- zip.geo.raw@data %>%
  select(
    zip = POSTCODE,
    town = CITY_TOWN,
    # all pc name equal pa nam
    pa_name = PA_NAME, # Name of postal (ZIP) code
    # pc_name = PC_NAME, # Postal area name (Last line city name from USPS sources)
    county = COUNTY,
    # area of the ZIP+Town combination
    zt_area = AREA_SQMI,
    zt_area_sqm = SHAPE_AREA
  ) %>% 
  mutate(
    pa_name = str_to_title(pa_name),
    town = str_to_title(town) %>%
      str_replace(", Town Of", "") %>%
      fct_recode(
        "Manchester"="Manchester By The Sea",
        # change Westhampton to Easthampton as
        # they both share the same ZIP code,
        # but Easthampton has much more larger population
        'Easthampton'='Westhampton'
      ),
    county = str_to_title(county)
  ) %>% 
  distinct()

# Map ZIP code the prevailing town (town with the largest area)
zip_town_county.dedup <- zip_town_county %>%
  arrange(zip, desc(zt_area)) %>%
  distinct(zip, .keep_all = TRUE) %>%
  # area doesn't matter anymore, because it might be only
  # partial of the zip area
  select(-zt_area)

town2county <- zip_town_county %>%
  select(town, county) %>%
  distinct()

# Save data
write_csv(zip_town_county, "data/MA_geo_units/ztc.csv")
write_csv(zip_town_county.dedup, "data/MA_geo_units/ztc.dedup.csv")
write_csv(town2county, "data/MA_geo_units/town2county.csv")