source("src/utils/acs.R")
source("src/utils/topics.R")

# `aci` is ACS indicators
AttachACI <- function(dat.count, dat.aci) {
  # add ACI metrics to geo.count
  dat.count %<>%
    right_join(dat.aci, by = "name") %>%
    # remove "XXXTotal" columns
    select(-ends_with("Total")) %>%
    # number of calls per 1000 people
    mutate(
      # do not calculate per capital data if there are less than
      # 100 residents
      n_call = as.double(n),
      p_call = n_call / TotalPop * 1000,
      n = NULL) %>%
    # only count percentages when:
    #   1. total population >= 200
    #   2. total number of calls >= 5
    filter(TotalPop > 200) %>%
    mutate(
      # No data means no calls
      n_call = if_else(is.na(n_call), 0, n_call),
      p_call = if_else(is.na(p_call), 0, p_call)
    )
  # remove extreme values by setting maximum value
  # to the 99% quantile
  # p_call_max <- quantile(dat.count$p_call, .99)
  # dat.count %>%
  #   mutate(
  #     # Add an upper bound for per capital number of calls
  #     p_call = if_else(p_call > p_call_max, p_call_max, p_call)
  #   )
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
      left_join(TopicCount(icalls, "income", geo.unit)) %>%
      left_join(TopicCount(icalls, "childcare", geo.unit)) %>%
      left_join(TopicCount(icalls, "food/cloth", geo.unit)) %>%
      left_join(TopicCount(icalls, "housing", geo.unit)) %>%
      left_join(TopicCount(icalls, "homeless", geo.unit)) %>%
      left_join(TopicCount(icalls, "substance abuse", geo.unit)) %>%
      left_join(TopicCount(icalls, "community", geo.unit)) %>%
      left_join(TopicCount(icalls, "disability", geo.unit)) %>%
      left_join(TopicCount(icalls, "utilities", geo.unit)) %>%
      left_join(TopicCount(icalls, "health", geo.unit)) %>%
      left_join(TopicCount(icalls, "mental", geo.unit)) %>%
      left_join(TopicCount(icalls, "-childcare", geo.unit)) %>%
      left_join(TopicCount(icalls, "-housing", geo.unit))
  }
  # additional metrics
  # ret %<>%
  #   # percentage of people who called before
  #   left_join(CalledBefore(icalls, geo.unit))
  
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
