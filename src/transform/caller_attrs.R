#
# Evaluate caller attrs
#

# ===== Veteran
zip.veteran <- icalls.simple %>%
  filter(military %in% c('Veteran', 'None'), zip %in% aci.zip$name) %>%
  group_by(zip) %>%
  summarise(
    # total calls
    n_call = n(),
    p_veteran = mean(military == 'Veteran', na.rm=TRUE)
  ) %>%
  filter(n_call > 5)

comp.veteran <- aci.zip %>%
  right_join(zip.veteran, by=c('name'='zip')) %>%
  mutate(p_call = n_call * 1000 / TotalPop)

comp.veteran %>%
  ggplot(aes(Veteran, p_veteran, size=p_call)) +
  geom_point() +
  scale_size_continuous(range = c(0.1,4)) +
  xlab('% veterans for all population > 18 years old') +
  ylab('% calls made by a veteran') +
  coord_fixed() +
  geom_segment(aes(x=0, y=0, xend=0.155, yend=0.155, size=1), color='red') +
  xlim(c(0,0.155)) + ylim(c(0,0.155)) +
  labs(size = '# calls/1k') +
  theme_minimal()

comp.veteran %>%
  ggplot(aes(Veteran, p_call)) +
  geom_point() +
  geom_smooth() +
  xlab('% veterans for all population > 18 years old') +
  ylab('# of calls per 1,000 people') +
  theme_minimal()

 
# Male/Female
zip.gender <- icalls.simple %>%
  group_by(zip) %>%
  # Femal to Male ratio
  summarise(
    n_call = n(),
    caller_SexRatio = sum(gender == 'F', na.rm=TRUE) / sum(gender == 'M', na.rm=TRUE)
  ) %>%
  filter(n_call > 5,
         caller_SexRatio > 0,
         caller_SexRatio < 20)

comp.gender <- aci.zip %>%
  filter(TotalPop > 200) %>%
  left_join(zip.gender, by=c('name'='zip')) %>%
  mutate(p_call = n_call * 1000 / TotalPop)

comp.gender %>% ggplot(aes(SexRatio, caller_SexRatio)) +
  geom_point(aes(size = p_call)) +
  geom_vline(xintercept=1, color='red') +
  scale_size_continuous(range = c(0.05,3)) +
  labs(size = 'calls per 1k', x='Population female:male ratio', y='Caller female:male ratio') +
  theme_minimal()
