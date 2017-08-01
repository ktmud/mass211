# external (non 211) referrals made
n.external.ref <- icalls %>%
  filter(external.referrals == "" | is.na(external.referrals)) %>% nrow()
# 1 - n.external.ref / nrow(icalls)

icalls %>%
  filter(str_count(external.referrals, ";") >= 2) %>%
  nrow()
icalls %>%
  filter(str_count(external.referrals, ";") >= 1) %>%
  nrow()
