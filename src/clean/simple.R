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
  m <- m[!is.na(m$id), ]
  m <- cbind(
    m[, 1:4],
    cat2.code = as.factor(gsub("([A-Z]+).*", "\\1", m$cat.code)),
    m[, 5:ncol(m)]
  )
  # Recode values
  mapvalues <- plyr::mapvalues
  m$type <- mapvalues(
    m$type, unique(m$type),
    # "Save me from required fields" -> "Unknown"
    c("Mass211", "Disaster", "SaveMe", "Chat", "Text", "")
  )
  m$date <- as.POSIXct(
    m$date, tz = "America/New_York", format = "%m/%d/%Y %H:%M:%S"
  )
  m$uw.region <- str_replace_all(
    m$uw.region,
    "( *)(.*?)( *)United Way( of)?( *)(.*)( *)$",
    "\\2\\6"
  )
  m$caller.relation <- mapvalues(
    m$caller.relation, unique(m$caller.relation),
    # "Professional" =
    #    Professional Calling for Client
    # "Family member" =
    #    Caller contacting for family member living elsewhere or friend
    c("Self/Household", "Professional", "Family/Friend Elsewhere", "")
  )
  m$age <- mapvalues(
    m$age, unique(m$age),
    c("45-64", "18-24", "25-44", ">65", "Unknown", "<18", "", "Declined to Answer")
  )
  m$lang.interp.used <- mapvalues(
    m$lang.interp.used, unique(m$lang.interp.used),
    # "Not Applicable" -> ""
    # Staff = "Staff Provided"
    # Other = "Someone else provided"
    # Service = "Translation Service"
    c("", "Staff", "Other", "", "Service")
  )
  m$gender <- fct_recode(m$gender, "M" = "Male", "F" = "Female")
  m$military <- mapvalues(
    m$military, unique(m$military),
    c("None", "Veteran", "", "Unknown", "Declined", "Active Duty")
  )
  m$household.insured <- mapvalues(
    m$household.insured, unique(m$household.insured),
    # -1 means unknown
    c(0, NA, -1, 1)
  )
  m$children.age <- fct_recode(
    m$children.age,
    "None"="No Children under 18 years in the home",
    "6to18"="Children 6 to under 18 years",
    "0to5"="Children 0 to 5 years"
  )
  m$family.size <- fct_recode(
    m$family.size,
    "1"="Single Person (1)"
  )
  m$called.before <- fct_recode(
    m$called.before,
    "Same issue"="Yes, for the same issue",
    "Diff issue"="Yes, for a different issue"
  )
  # XXX: data before May, 2016 are incomplete
  m %>% filter(date > as.Date("2016-05-01"))
}

DistinctCalls <- function(mcalls) {
  # Distinct 211 calls by `cat.name`, popular
  # categories will prevail.
  # Args:
  #   mcalls - data frame of all calls where calls might be in multiple categories
  #            (showing as duplicate rows)
  count.cat.code <- count(mcalls, cat.code)
  mcalls %>%
    filter(!is.na(cat.code)) %>%
    left_join(count.cat.code, by = "cat.code") %>%
    rename(cat.total = n) %>%
    arrange(id, desc(cat.total)) %>%
    distinct(id, .keep_all = TRUE)
}

TrustZipForTown <- function(dat) {
  # Sometimes there are inconsistencies in ZIP code and town data
  # We trust ZIP over town.
  
  # Filter out ZIP codes with fixed town name
  ztc1 <- ztc %>%
    count(zip, town) %>%
    filter(n == 1) %>%
    select(-n)
  
  # Set town name based on ZIP code
  dat %>%
    left_join(ztc1, by = 'zip') %>%
    mutate(
      town = ifelse(is.na(town.y), town.x, town.y)
    ) %>%
    select(-town.x, -town.y) %>%
    left_join(ztc)
}

# zip -> town -> county map
ztc <- read_csv("data/MA_geo_units/ztc.csv")

# mcalls - calls with taxonomy
if (!exists("mcalls.raw")) {
  mcalls.raw <- ReadExcels(Sys.glob("data/211/20170731/*"))
}
# TODO: fix more towns
mcalls <- mcalls.raw %>%
  CleanMass211Simple() %>%
  TrustZipForTown()
# icalls - calls deduplicated by call id
icalls <- DistinctCalls(mcalls)