#
# The Time Consumption for treemap
#
library(treemap)

# February to April are still in test run, let's remove some days
report.days <- max(mcalls$call.start) - min(mcalls$call.start) - 80
report.days %<>% as.integer()

mcalls.cat.tree <- mcalls %>%
  select(cat.name, cat1.name, cat2.name, cat3.name,
         cat4.name, cat5.name) %>%
  arrange(cat.name, cat5.name) %>%
  distinct(cat.name, .keep_all = TRUE)

mcalls.cats.summary <- mcalls %>%
  # add half minute to all call lengths
  left_join(call.cats5, by = "id") %>%
  mutate(call.cat.length = call.length / n.cat) %>%
  group_by(cat.name) %>%
  summarize(
    n.calls = n(),
    call.length.mean = mean(call.length, na.rm = TRUE),
    call.length.median = median(call.length, na.rm = TRUE),
    call.length.sd = sd(call.length, na.rm = TRUE),
    total.minutes = sum(call.length, na.rm = TRUE)
  ) %>%
  mutate(
    min.per.day = total.minutes / report.days
  ) %>%
  arrange(desc(total.minutes)) %>%
  left_join(mcalls.cat.tree, by = "cat.name")

mcalls.treemap <- mcalls.cats.summary %>%
  treemap(
    title = "",
    index = str_c("cat", 1:5, ".name"),
    vSize = "min.per.day",
    border.lwds = 0.1,
    vColor = "cat1.name",
    range = c(0, 300),
    draw = FALSE
  )
