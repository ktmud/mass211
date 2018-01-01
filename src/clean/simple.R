##
# Read simple Format 211 data:
#   those exported in iCarol -> Statistics -> Custom Reports -> "United Way Regions"
##
library(readxl)

ReadExcels <- function(files) {
  # Read multiple excel files and merge them
  map_df(files, function(x) {
    read_excel(x)
  }) %>% bind_rows()
}

CleanMass211Simple <- function(m) {
  # Clean simple format Mass211 data
  # Args:
  #   m - the raw data frame
  # Returns:
  #   the cleaned version of Mass211 data
  
  # rename columns
  names(m) <- c(
    "date", "id", "type", "cat.code", "cat.name", "town",
    "uw.region", "zip", "caller.relation", "age",
    "lang",  # Primary family language
    "lang.interp.used",  # Language interpretation used in this call?
    "gender",  # Gender of Primary Person Needing Service (PPNS)
    "military",  # Military status of PPNS
    "household.insured",  # Household covered by insurance
    "source.of.income",
    "channel",  # How did you learn about Mass211
    "called.before", "family.size", "children.age"
  )
  # remove rows with bad IDs
  m %<>% filter(!is.na(id))
  m <- cbind(
    m[, 1:4],
    cat2.code = as.factor(gsub("([A-Z]+).*", "\\1", m$cat.code)),
    m[, 5:ncol(m)]
  )
  m %<>% mutate(
    date =  date %>% as.POSIXct(
      tz = "America/New_York",
      format = "%m/%d/%Y %H:%M:%S"
    ),
    uw.region = uw.region %>%
      str_replace_all(
        "( *)(.*?)( *)United Way( of)?( *)(.*)( *)$",
        "\\2\\6"
      ),
    type = type %>%
      fct_recode(
        "SaveMe"="Save me from required fields"
      ),
    caller.relation = caller.relation %>%
      fct_recode(
        "Self/Household"="Caller Contacting for Self/Household",
        "Professional"="Professional Calling for Client",
        "Family/Friend Elsewhere"="Caller contacting for family member living elsewhere or friend"
      ),
    age = age %>% fct_recode(
      "<18"="Under age 18 Years",
      "18-24"="Age 18 to 24 Years",
      "25-44"="Age 25 to 44 Years",
      "45-64"="Age 45 to 64 Years",
      ">65"="Age 65 years and older",
      "Declined"="Declined to Answer",
      NULL = 'Unknown'
    ),
    gender = gender %>% fct_recode("M" = "Male", "F" = "Female"),
    children.age = children.age %>%
      fct_recode(
        "None"="No Children under 18 years in the home",
        "6to18"="Children 6 to under 18 years",
        "0to5"="Children 0 to 5 years"
      ),
    family.size = family.size %>% fct_recode("1"="Single Person (1)"),
    called.before = called.before %>%
      fct_recode(
        "Same issue"="Yes, for the same issue",
        "Diff issue"="Yes, for a different issue"
      )
  )
  # XXX: data before May, 2016 are incomplete
  # m %>% filter(date > as.Date("2016-05-01"))
}

TrustZipForTown <- function(dat) {
  # Sometimes there are inconsistencies in ZIP code and town data
  # We trust ZIP over town.
  
  # Filter out ZIP codes with fixed town name
  ztc1 <- ztc %>%
    count(zip) %>%
    # number of towns associated with this zip code
    filter(n == 1) %>%
    select(-n)
  ztc1 <- ztc %>%
    filter(zip %in% ztc1$zip) %>%
    rename(town_from_zip = town)
  
  # Set town name based on ZIP code
  dat %>%
    left_join(ztc1, by = 'zip') %>%
    mutate(
      # if town name can be inferred from zip code
      # use town name from the zip code.
      # This is because sometimes the recorded town name
      # is not consistent with the official township names,
      # as some (historical) towns were absorbed as neighborhoods
      # in a bigger town
      town = FirstNonNA(town_from_zip, town)
    ) %>%
    select(-town_from_zip) %>%
    left_join(ztc)
}

# zip -> town -> county map
ztc <- read_csv("data/MA_geo_units/ztc.csv")

# mcalls - calls with taxonomy
if (!exists("mcalls.simple.raw")) {
  mcalls.simple.raw <- ReadExcels(Sys.glob("data/211/20170731/*"))
}

# TODO: fix more towns
mcalls.simple <- mcalls.simple.raw %>%
  CleanMass211Simple() %>%
  TrustZipForTown() %>%
  # remove duplicate calls (distinct by id + cat.code)
  distinct(id, cat.code, .keep_all=TRUE)
mcalls.simple.mental <- mcalls.simple %>%
  filter(is.na(type))
mcalls.simple <- mcalls.simple %>%
  filter(!is.na(type))

# Completeness of data fields
mcalls.simple %>% summarise(mean(gender == 'F', na.rm=T))
mcalls.simple %>% summarise(mean(is.na(gender)))
mcalls.simple %>% summarise(mean(is.na(military)))
mcalls.simple %>% summarise(mean(caller.relation == 'Self/Household', na.rm=TRUE))
