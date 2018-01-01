colors1 <- str_split('#5C3F56,#60526E,#5C6785,#527D98,#4393A4,#39A8A9,#40BDA7,#5DD19E,#84E291,#B3F283,#E6FE77',
                     ',', simplify = TRUE)

Heatmap1 <- function(dat, x, y, fill, opacity=NULL) {
  x <- substitute(x)
  y <- substitute(y)
  fill <- substitute(fill)
  opacity <- substitute(opacity)
  dat %>% ggplot(aes_(x = x, y = y)) +
    geom_tile(aes_(fill = fill, alpha=opacity)) + 
    scale_fill_gradientn(colors=colors1) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

CountCatGeo <- function(mcalls, unit='county') {
  aci.data <- get(str_c('aci.', unit))
  calls <- mcalls %>%
    rename_(name = unit) %>%
    select(name, topic) %>%
    # add total population
    left_join(aci.data %>% select(name, TotalPop), by = 'name') %>%
    # Need at least 100 people
    filter(!is.na(TotalPop)) %>%
    # expand the topic list as multiple rows
    mutate(topic = str_split(topic, '; ')) %>%
    unnest(topic)
  
  # Count of name + topic
  dat1 <- calls %>%
    # Get all possible combinations of geo unit name + topic
    expand(name, topic) %>%
    # add the actual count
    left_join(count(calls, name, topic)) %>%
    # convert NA's to zeros, so geo unit without calls of a topic will count as zero
    mutate(n = if_else(is.na(n), 0L, n)) %>%
    # add total population
    left_join(aci.data %>% select(name, TotalPop), by = 'name')
  
  # Count of each topic
  dat2 <- calls %>%
    count(topic) %>%
    rename(n_topic_total = n)
  # Count of each name
  dat3 <- calls %>%
    count(name) %>%
    rename(n_geo_total = n)

  dat1 %>%
    # topic
    left_join(dat2, by = 'topic') %>%
    # geo name
    left_join(dat3, by = 'name') %>%
    # % of total
    # mutate(
    #   n_scaled = n / n_topic_total,
    #   # per 1 thousand people
    #   per1k = n * 1000 / TotalPop,
    #   p_scaled = n_scaled * 1000 / TotalPop,
    #   p_topic_total = n_topic_total * 1000 / TotalPop,
    #   p_geo_total = n_geo_total * 1000 / TotalPop
    # ) %>%
    mutate(
      name = fct_reorder(name, n_geo_total),
      topic = fct_reorder(topic, n_topic_total)
    )
}

count.cat.county <- CountCatGeo(mcalls)
count.cat.zip <- CountCatGeo(mcalls, 'zip')
count.cat.town <- CountCatGeo(mcalls, 'town')
