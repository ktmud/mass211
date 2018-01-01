
count.resource <- sites.ma %>%
  mutate(topic = str_split(topic, '; ')) %>%
  unnest(topic) %>%
  count(city, topic) %>%
  left_join(ztc, by=c('city'='town')) %>%
  group_by(zip, topic) %>%
  # in case a ZIP code is located in more than one town/city,
  # it will have access to resources in all of them.
  summarize(n = sum(n)) %>%
  na.omit()
