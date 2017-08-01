library(tidyr)
library(forcats)

airs.cat.order <- mcalls %>%
  count(airs.cat, sort = TRUE)

# Reduce calls to only 1 category,
# the category with highest number of calls will prevail
mmcalls <- mcalls %>%
  select(id, zip, airs.cat) %>%
  left_join(airs.cat.order) %>%
  arrange(desc(n)) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(-n)

zip.cat.count <- mmcalls %>%
  count(airs.cat, zip) %>%
  spread(airs.cat, n, fill = 0) %>%
  inner_join(count.zip %>% select(zip, p.call, TotalPop), by = "zip")

zip.cat.count[2:(ncol(zip.cat.count) - 1)] <-
  1000 * zip.cat.count[2:(ncol(zip.cat.count) - 1)] / zip.cat.count$TotalPop
  
cor.test(zip.cat.count$Housing, zip.cat.count$`Income Support`)
