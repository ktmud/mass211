if (!exists("fma")) {
  # Five-hundred MA
  fma <- read_csv("data/500cities-full.csv") %>%
    filter(StateAbbr %in% c("US", "MA"))
}
f.cities <- fma %>%
  select(-StateDesc) %>%
  filter(GeographicLevel == "City") %>%
  # discard age-adjusted prevalence
  # filter(Data_Value_Type == "Crude prevalence") %>%
  select(ID = CityFIPS, Data_Value_Type, CityName, MeasureId, Data_Value) %>%
  spread(MeasureId, Data_Value) %>%
  mutate_if(is_double, function(x) x / 100)
f.tracts <- fma %>%
  filter(GeographicLevel == "Census Tract") %>%
  select(ID = TractFIPS, CityName, MeasureId, Data_Value) %>%
  spread(MeasureId, Data_Value) %>%
  mutate_if(is_double, function(x) x / 100)
