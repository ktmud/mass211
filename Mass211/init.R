library(plyr)
library(tidyverse)
library(magrittr)
library(stringr)
library(networkD3)

if (!exists("mcalls")) {
  mcalls <- read_csv("data/mcalls.csv")
}
if (!exists("icalls")) {
  icalls <- read_csv("data/icalls.csv")
}

# distinct calls with category information
# TODO: find a way to always keep the more important categories
# or categorize multiple category as separate
mcalls2 <- mcalls %>%
  # Remove calls with unidentified AIRS cat
  # data before May, 2016 are not complete
  filter(!is.na(airs.cat), call.start > as.Date("2016-05-01")) %>%
  select(id, call.start, airs.cat, cat5.name) %>%
  distinct(id, .keep_all = TRUE)