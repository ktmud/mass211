##
# Combine old and new data into one
# Contains only basic taxonomy cat and geo info
##

# Combine and add a `topic` column
source('src/utils/topics.R')

avail_cols <- c(colnames(mcalls.old), 'called.before')

mcalls <- mcalls.simple[, avail_cols] %>%
  mutate(
    id = as.character(id),
    date = as.Date(date)
  ) %>%
  bind_rows(mcalls.old) %>%
  mutate(
    topic = Cat2Topic(cat.code),
    # Part of Bonsville, merged to 01069 Palmer already.
    zip = if_else(zip == '01009', '01069', zip),
    # Manually change a few village/neighborhood names to the actual town name
    # these names are spotted when checking which town has zero population
    # after joining the ACI data
    town = if_else(town == 'Allston', 'Boston', town),
    town = if_else(town == 'Jamaica Plain', 'Boston', town),
    town = if_else(town == 'East Boston', 'Boston', town),
    town = if_else(town == 'South Boston', 'Boston', town),
    town = if_else(town == 'Hyde Park', 'Boston', town),
    town = if_else(town == 'Grove Hall', 'Boston', town),
    town = if_else(town == 'Buzzards Bay', 'Bourne', town),
    town = if_else(town == 'Brant Rock', 'Brant Rock', town),
    town = if_else(town == 'Glendale', 'Stockbridge', town),
    town = if_else(town == 'Whitinsville', 'Northbridge', town),
    town = if_else(town == 'Gilbertville', 'Hardwick', town),
    town = if_else(town == 'Hyannis', 'Barnstable', town),
    town = if_else(town == 'Indian Orchard', 'Springfield', town),
    town = if_else(town == 'Bondsville', 'Palmer', town),
    town = if_else(town == 'East Falmouth', 'Falmouth', town),
    town = if_else(town == 'East Otis', 'Otis', town),
    town = if_else(town == 'Leeds', 'Northampton', town),
    town = if_else(town == 'Housatonic', 'Great Barrington', town),
    town = if_else(town == 'Lanesboro', 'Lanesborough', town),
    town = if_else(town == 'Lake Pleasant', 'Montague', town),
    town = if_else(town == 'Jefferson', 'Holden', town)
  )

mcalls.full <- mcalls
call2talk.full <- mcalls.full %>%
  filter(
    cat.code %in% c('RP-1500.1400-500', 'RP-1500.1400-800')
  )
write_csv(mcalls.full, 'data/mcalls_full.csv')

# Call2Talk has too many repetive callers
# not helpful to analyais
mcalls <- mcalls.full %>%
  filter(
    # Remove "Mental Health Hotlines"
    # and "Suicide Prevention Hotlines"
    cat.code != 'RP-1500.1400-500',
    cat.code != 'RP-1500.1400-800',
    date >= ymd('2016-06-01'),
    date < ymd('2017-09-01')
  )
call2talk <- call2talk.full %>%
  filter(
    date >= ymd('2016-05-01'),
    date < ymd('2017-09-01')
  )

icalls <- DistinctCalls(mcalls)
icalls.simple <- DistinctCalls(mcalls.simple)

# Topic coverage
length(which(!is.na(mcalls$topic))) / length(mcalls$topic)
mcalls %<>% mutate(
  topic = if_else(is.na(topic), 'other', as.character(topic))
)

mcalls %>%
 count(topic, sort = TRUE) %>%
 filter(!str_detect(topic, ';')) %>%
 .$topic %>%
  str_c(collapse=', ')


mcalls %>% count(id) %>% filter(n > 1) %>%
  nrow() / length(unique(mcalls$id))
