zip.geo <- ReadOGR("data/zipcodes", "ZIPCODES_NT_POLY")
names(zip.geo) %<>% str_to_lower()
zip.geo@data %<>%
  rename(zip = postcode) %>%
  mutate(
    town = str_to_title(city_town) %>%
      str_replace(", Town of", ""),
    pa_name = str_to_title(pa_name),
    county = str_to_title(county)
  ) %>%
  select(zip, town, county, pa_name, shape_area)

zip.main.area <- zip.geo@data %>%
  arrange(zip, desc(shape_area)) %>%
  distinct(zip, .keep_all = TRUE) %>%
  select(-shape_area)

zip.geo@data %<>%
  group_by(zip) %>%
  # some zip might be split into multiple areas,
  # we need to sum the area sizes up to get its real size
  summarize(shape_area = sum(shape_area)) %>%
  left_join(zip.main.area, by = "zip") %>%
  left_join(count.zip, by = c("zip"="name")) %>%
  mutate(
    PopDen = TotalPop / (shape_area * 3.86102159 * 10e-7)
  ) %>%
  standardize("PopDen") %>%
  # select zip column from the raw data,
  # so to make sure we have the same amount of rows
  # this may cause duplicate zips, which is fine
  right_join(zip.geo@data %>% select(zip)) %>%
  # rename "zip" to "name" as this is convention
  # for any geo unit
  rename(name = zip) %>%
  # geo data can only be raw data frame, not tibble
  as.data.frame()

zip.geo.simple <- ms_simplify(zip.geo)

# round numbers
num_cols <- 7:ncol(zip.geo.simple@data)
zip.geo.simple@data[, num_cols] <- round(
  zip.geo.simple@data[, num_cols], digits=6)

writeOGR(zip.geo.simple,
         "Mass211/data/zip.json",
         layer = "ZIP_POLY",
         driver = "GeoJSON")
