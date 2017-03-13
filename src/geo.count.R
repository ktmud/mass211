# number of calls
count.city <- icalls %>%
  filter(state == "MA") %>%
  count(city, sort = TRUE) %>%
  na.omit()
count.county.airs <- mcalls %>%
  count(county, airs.cat)
count.county.cat <- mcalls %>%
  count(county, cat.name)

source("src/acs.R")

count.zip <- icalls %>%
  filter(state == "MA") %>%
  group_by(zip) %>%
  summarise(
    n = n(),
    # there are no NA's in call length
    total.minutes = sum(call.length) %>% as.integer()
  ) %>%
  na.omit()

count.zip <- inner_join(count.zip, aci.zip, by = "zip") %>%
  filter(TotalPop > 10) %>%
  select(-ends_with("Total")) %>%
  mutate(
    AtLeastBachelor = Bach + Master + Prof + Doc,
    # change the unit of median house income to 1k USD.
    MedHouseIncome = MedHouseIncome / 1000
  ) %>%
  # number of calls per 1000 people
  mutate(p.call = (n * 1000) / TotalPop, n = NULL)

count.county <- icalls %>%
  filter(state == "MA") %>%
  group_by(county) %>%
  summarise(
    n = n(),
    # there are no NA's in call length
    total.minutes = sum(call.length) %>% as.integer()
  ) %>%
  na.omit()
count.county <- inner_join(count.county, aci.county, by = "county") %>%
  filter(TotalPop > 10) %>%
  select(-ends_with("Total")) %>%
  # number of calls per 1000 people
  mutate(p.call = (n * 1000) / TotalPop, n = NULL)

# ByZip(calls$`Mass 211`)


# count.zip <- icalls %>%
#   # filter(state == "MA") %>%
#   count(zip, sort = TRUE) %>%
#   na.omit()
# count.zip.cat <- mcalls %>%
#   count(zip, airs.cat, sort = TRUE)
# count.zip.cat4 <- mcalls %>%
#   count(zip, cat4.name, sort = TRUE)
