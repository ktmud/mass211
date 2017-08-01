source("src/utils/acs.R")
source("src/utils/topics.R")

# `aci` is ACS indicators
if (!exists("aci.zip")) {
  aci.zip.orig <- FetchACS(geo.group = "zip", raw.numbers = TRUE)
  aci.zip <- MutateAcs(aci.zip.orig)
}
# if (!exists("aci.tract")) {
#   aci.tract <- FetchACS(geo.group = "tract")
# }
if (!exists("aci.county")) {
  aci.county <- FetchACS(geo.group = "county")
}
if (!exists("aci.town")) {
  aci.town <- FetchACS(geo.group = "town")
  # Consolidate town names for ACS indicators data
  aci.town %<>%
    mutate(
      name = name %>%
        str_replace("Manchester-by-the-Sea", "Manchester")
    )
}
if (!exists("zip.urbanpop")) {
  # urban area to zip code relationship
  # Data downloaded from:
  # https://www.census.gov/geo/maps-data/data/ua_rel_download.html
  ru.zip <- read_csv("data/rural_urban_zip.csv")
  zip.urbanpop <- ru.zip %>%
    filter(!str_detect(UANAME, "Not in")) %>%
    group_by(zip = ZCTA5) %>%
    summarize(
      # calculate percentage of people who live in
      # a urbanized area
      p.urbanpop = sum(ifelse(str_detect(UANAME, "Not in"), 0, ZPOPPCT)),
      # population density of the whole zip code
      pop.density = sum(ZPOP) / sum(ZAREALAND / 1000,000)
    )
}

AttachACI <- function(dat.count, dat.aci) {
  # add ACI metrics to geo.count
  dat.count %>%
    right_join(dat.aci, by = "name") %>%
    # remove small populartion areas
    filter(TotalPop > 10) %>%
    # remove "XXXTotal" columns
    select(-ends_with("Total")) %>%
    # number of calls per 1000 people
    mutate(
      # do not calculate per capital data if there are less than
      # 100 residents
      n_call = n,
      # only count percentages when:
      #   1. total population >= 100
      #   2. total number of calls >= 5
      p_call = ifelse(TotalPop >= 100 & n >= 5,
                      (n * 1000) / TotalPop, NA),
      n = NULL) %>%
    mutate(
      # No data means no calls
      n_call = ifelse(is.na(n_call), 0, n_call),
      p_call = ifelse(is.na(p_call), 0, p_call),
      p_call.s = (p_call - mean(p_call)) / sd(p_call)
    )
}

library(zipcode)

MA_ZIPCODE <- unique(zipcode %>% filter(state == "MA") %>% .$zip)
  
GeoCount <- function(icalls, geo.unit, add_topics=FALSE) {
  # number of calls per geo unit
  if (geo.unit == 'city') {
    geo.unit = 'town'
  }
  ret <- icalls %>%
    filter(zip %in% MA_ZIPCODE) %>%
    count_(geo.unit, sort = TRUE) %>%
    # consistent names are better for code reuse
    rename_("name" = geo.unit) %>%
    na.omit()
  if (geo.unit == "zip") {
    ret %<>% AttachACI(aci.zip) %>%
      left_join(zip.urbanpop, by = c("name" = "zip"))
      # mutate(UrbanPop = ToFactor(
      #   p.urbanpop,
      #   "< 10%" = 10,
      #   "10% ~ 90%" = 90,
      #   ">90%" = 101
      # ))
  } else if (geo.unit == "town") {
    ret %<>% AttachACI(aci.town) 
  } else if (geo.unit == "county") {
    ret %<>% AttachACI(aci.county) 
  }
  if (add_topics) {
    ret %<>%
      left_join(TopicCount("employment", geo.unit)) %>%
      left_join(TopicCount("income", geo.unit)) %>%
      left_join(TopicCount("childcare", geo.unit)) %>%
      left_join(TopicCount("food", geo.unit)) %>%
      left_join(TopicCount("housing", geo.unit)) %>%
      left_join(TopicCount("homeless", geo.unit)) %>%
      left_join(TopicCount("substance", geo.unit)) %>%
      left_join(TopicCount("utilities", geo.unit)) %>%
      left_join(TopicCount("health", geo.unit)) %>%
      left_join(TopicCount("mental", geo.unit)) %>%
      left_join(TopicCount("-childcare", geo.unit)) %>%
      left_join(TopicCount("-housing", geo.unit))
  }
  # additional metrics
  ret %<>%
    # percentage of people who called before
    left_join(CalledBefore(icalls, geo.unit))
  
  ret
}

GeoCount2 <- function(icalls, geo.unit, var.name) {
  # Count by geo variables and a few variables
  # Args:
  #    vars - a vector of variables
  totals <- icalls %>%
    count_(geo.unit) %>%
    rename(total = n)
  dat <- icalls %>%
    count_(c(geo.unit, var.name)) %>%
    left_join(totals) %>%
    mutate(
      percent = n / total
    )
}
CalledBefore <- function(icalls, geo.unit) {
  # Calculate percentage of calls made by people who called before
  GeoCount2(icalls, geo.unit, 'called.before') %>%
    filter(called.before != 'No', total >= 10) %>%
    group_by_(geo.unit) %>%
    summarise(p_called_before = sum(percent)) %>%
    rename_(name = geo.unit)
}

ToFactor <- function(x, ..., .default = "Other") {
  # convert a continous variable to a factor
  # Args:
  #   ... in the format of "name{string} = upperbound{numeric}"
  levs <<- c(...)
  names <- names(levs)
  ret <- c()
  for (i in length(levs):1) {
    ret[x <= levs[i]] <- names(levs[i])
  }
  factor(ret, levels = names, ordered = TRUE)
}

count.town <- GeoCount(icalls, "town", TRUE)
count.zip <- GeoCount(icalls, "zip", TRUE)

count.county <- GeoCount(icalls, "county", TRUE)
count.county.cat <- count(icalls, county, cat.name)