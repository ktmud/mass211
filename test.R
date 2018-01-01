library(dplyr)
library(tidyr)
library(forcats)

stocks <- data_frame(
  time = as.Date('2009-01-01') + 0:2,
  Q1_1 = c('YES', 'NO', 'NO'),
  Q1_2 = c('NO', 'NO', 'YES'),
  Q1_3 = c('NO', 'YES', 'NO')
)

stocks %>%
  mutate_at(vars(-time), funs(fct_recode(., '1'='YES', '0'='NO'))) %>%
  unite(Q1, Q1_1:Q1_3, sep='') %>%
  mutate(
    Q1 = fct_recode(Q1,
      "Answer 1"="100",
      "Answer 2"="010",
      "Answer 3"="001"
    ))
