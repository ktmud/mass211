# combined call categories
call.cats3 <- mcalls %>%
  select(id, cat3.name) %>%
  distinct() %>%
  group_by(id) %>%
  arrange(cat3.name) %>%
  summarize(
    n.cat = n(),
    cats = cat3.name %>% na.omit() %>% str_c(collapse = " || ")
  )

call.cats5 <- mcalls %>%
  select(id, cat.name) %>%
  distinct() %>%
  group_by(id) %>%
  arrange(cat.name) %>%
  summarize(
    n.cat = n(),
    cats = cat.name %>% na.omit() %>% str_c(collapse = " || ")
  )
  
# multiple categories and referrals
count.call.cats3 <- call.cats3 %>%
  count(cats, sort = TRUE)

count.call.cats5 <- call.cats5 %>%
  count(cats, sort = TRUE) %>%
  na.omit()

# only those with multiple categories
count.call.cats5.m <- count.call.cats5 %>% filter(str_detect(cats, " \\|\\| "))

n.multiple.cat <- count.call.cats5.m %>% summarize(n = sum(n)) %>% unlist()
n.call.with.cat <- count.call.cats5 %>% summarize(n = sum(n)) %>% unlist()
n.multiple.cat / n.call.with.cat


icalls %>%
  left_join(call.cats5, by = "id") %>%
  filter(
    str_count(external.referrals, ";") >= 2 &
      n.cat > 1
  ) %>%
  nrow()

# ----------------- Summary - for tree map ---------

# ---------------
count.call.refs <- mcalls %>%
  select(id, agency) %>%
  distinct() %>%
  group_by(id) %>%
  arrange(agency) %>%
  summarize(refs = agency %>% str_c(collapse = " || ")) %>%
  count(refs, sort = TRUE)

count.call.refs.m <- count.call.refs %>%
  filter(str_detect(refs, " \\|\\| "))

n.multiple.ref <- count.call.refs.m %>% summarize(n = sum(n)) %>% unlist()
n.call.with.ref <- count.call.refs %>% summarize(n = sum(n)) %>% unlist()
n.multiple.ref / n.call.with.ref