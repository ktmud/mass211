#
# Process and export county metric geojson
#
county.geo <- ReadOGR('data/counties', 'COUNTIES_POLYM')
county.hsi <- read_csv('data/health-indicators/hsi.county.csv') %>%
  select(-white, -black, -hispanic, -asian)
county.opd <- read_csv('data/by-county/op-death.csv') %>%
  select(
    name = County,
    n_op_death_2014=`2014`,
    n_op_death_2015=`2015`,
    n_op_death_2016=`2016`
  )

names(county.geo) %<>% str_to_lower()
county.geo@data %<>%
  mutate(county = str_to_title(county)) %>%
  select(name = county, shape_area) %>%
  left_join(count.county, by = 'name') %>%
  left_join(county.hsi, 'name') %>%
  left_join(county.opd, 'name') %>%
  mutate(
    # op death per 100 thousand people
    p_op_death_2014 = n_op_death_2014 / TotalPop * 100000,
    p_op_death_2015 = n_op_death_2015 / TotalPop * 100000,
    p_op_death_2016 = n_op_death_2016 / TotalPop * 100000,
    PopDen = TotalPop / (shape_area * 3.86102159 * 10e-7)
  ) %>%
  AddResidual()

county.geo.simple <- ms_simplify(county.geo)

# round numbers
num_cols <- 5:ncol(county.geo.simple@data)
county.geo.simple@data[, num_cols] <- round(
  county.geo.simple@data[, num_cols], digits=6)