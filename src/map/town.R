##
# Process Data Bindings for towns
##
if (!exists("town.geo.raw")) {
  town.geo.raw <- ReadOGR("data/towns", "TOWNS_POLYM") 
}
town.hsi <- read_csv("data/health-indicators/hsi.town.csv") %>%
  select(-white, -black, -hispanic, -asian)

town.opd <- read_csv('data/by-town/MA_opioid_deaths.csv') %>%
  mutate(name = fct_recode(`City-town`, 'North Attleborogh'='North Attleboro')) %>%
  select(name,
         n_op_death_2014=`2014`,
         n_op_death_2015=`2015`,
         n_op_death_2016=`2016`)

town.geo <- town.geo.raw
names(town.geo) %<>% str_to_lower()
town.geo@data %<>%
  mutate(town = str_to_title(town)) %>%
  select(name = town, shape_area)

town.geo@data %<>%
  left_join(count.town, by = "name") %>%
  left_join(town.hsi, "name") %>%
  left_join(town.opd, 'name') %>%
  mutate(
    # op death per 100 thousand people
    p_op_death_2014 = n_op_death_2014 / TotalPop * 100000,
    p_op_death_2015 = n_op_death_2015 / TotalPop * 100000,
    p_op_death_2016 = n_op_death_2016 / TotalPop * 100000,
    PopDen = TotalPop / (shape_area * 3.86102159 * 10e-7)
  ) %>%
  AddResidual()

town.geo.simple <- ms_simplify(town.geo)
# round numbers
num_cols <- 5:ncol(town.geo.simple@data)
town.geo.simple@data[, num_cols] <- round(
  town.geo.simple@data[, num_cols], digits=6)

