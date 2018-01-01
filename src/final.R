
ggplot(count.zip, aes(p_call)) +
  geom_histogram(bins=50) +
  labs(title='Histogram of number of calls per 1,000 people',
       subtitle='aggregated at the ZIP code level',
       x = '# of calls per 1,000 people',
       y = '# of ZIP code areas') +
  theme_bw()

nrow(count.cat.zip %>% filter(n < 5)) / nrow(count.cat.zip)

count.cat.zip %>%
  filter(topic == 'housing') %>%
  mutate(p_call = n / TotalPop) %>%
  arrange(desc(p_call))

count.topic <- unlist(mcalls$topic %>% str_split(';')) %>%
  table() %>% as.data.frame()
colnames(count.topic) <- c('topic', 'n')
count.topic <- count.topic %>%
  mutate(
    # topic = if_else(n < quantile(n, 0.2), 'other', as.character(topic)),
    topic = fct_reorder(topic, -n)
  ) %>%
  group_by(topic) %>%
  summarise(n = sum(n))

ggplot(count.topic, aes(area=n, fill = topic, label = topic)) + 
  geom_treemap() +
  geom_treemap_text(reflow = T, colour = "black",
                    padding.x = grid::unit(3, "mm"),
                    padding.y = grid::unit(3, "mm")) +
  scale_fill_brewer(palette = 'Set3') +
  guides(fill = FALSE)
  
  
