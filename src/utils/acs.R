library(zipcode)
library(magrittr)
library(dplyr)
library(stringr)
library(acs)

# load all zipcodes
data(zipcode)

# as copied from the BARI's ACS indicators R syntax
kAPIKey <- "829a6baee8d09366819cd05f131b47aeaf232576"
# api.key.install(kAPIKey)

# A list of default variables to fetch
# must be one name one code
kDefaultVars <- c(
  TotalPop = "B01003_001",
  Female = "B01001_026",
  Male = "B01001_002",
  AgeU5 = "B06001_002",
  Age517 = "B06001_003",
  Age1824 = "B06001_004",
  Age2534 = "B06001_005",
  Age3544 = "B06001_006",
  Age4554 = "B06001_007",
  Age5559 = "B06001_008",
  Age6061 = "B06001_009",
  Age6264 = "B06001_010",
  Age6574 = "B06001_011",
  Age75P = "B06001_012",
  
  # Education
  Edu = str_c("B15003_", str_pad(1:25, 3, pad = "0")),
  
  BornInState = "B06001_013",
  BornInOtherState = "B06001_025",
  NativeBornOutOfUS = "B06001_037",
  ForeignBorn = "B05002_013",
  
  White = "B03002_003",
  Black = "B03002_004",
  Asian = "B03002_006",
  Hispanic = "B03002_012",
  TwoOrMore = "B03002_009", # two or more races
  
  # median house income
  MedHouseIncome = "B19013_001",
  GINI = "B19083_001",
  
  PubAssistTotal = "B19057_001",
  PubAssistYes = "B19057_002",  # household with public assistantship
  
  Poverty = str_c("B17019_", str_pad(1:12, 3, pad = "0")),
  
  LaborTotal = "B23025_003",  # civilian in labor force
  LaborUnemp = "B23025_005",
  
  # Geographical Mobility in the Past Year by Tenure
  # for Current Residence in the US
  GM_Total = "B07013_001",
  OwnerOccupied = "B07013_002",
  RenterOccupied = "B07013_003",
  SameHouse1YearAgo = "B07013_004",
  
  HouseholdTotal = "B11011_001", 
  MaleHead = "B11011_011",  # male householder, no wife
  FemaleHead = "B11011_012",  # female householder, no husband
  Nonfamily = "B11011_016", # nonfamily households
  GrandWithChildTotal = "B10050_001",
  GrandHead = "B10051_002"
)

# standardize <- function(dat, ...) {
#   # Standardize given columns
#   # ad a new column which gives
#   cols <- c(...)
#   dat[str_c(cols, ".s")] <- lapply(dat[cols], function(x) {
#     (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
#   })
#   dat
# }

MutateAcs <- function(dat) {
  # Convert abs numbers to percetages
  
  SumCols <- function(prefix, idx) {
    cols <- str_c(prefix, idx)
    dat[, cols] %>% rowSums()
  }
  
  totalPop = dat$TotalPop
  totalEdu = dat$Edu1
  
  dat %>%
    # replace(is.na(.), 0) %>%
    mutate(
      Female = Female / totalPop,
      Male = Male / totalPop,
      SexRatio = Female / Male,
      
      AgeU5 = AgeU5 / totalPop,
      Age517 = Age517 / totalPop,
      Age1824 = Age1824 / totalPop,
      Age2534 = Age2534 / totalPop,
      Age3544 = Age3544 / totalPop,
      Age4554 = Age4554 / totalPop,
      Age5559 = Age5559 / totalPop,
      Age6061 = Age6061 / totalPop,
      Age6264 = Age6264 / totalPop,
      Age6574 = Age6574 / totalPop,
      Age75P = Age75P / totalPop,
      Age6064 = (Age6061 + Age6264) / totalPop,
      AgeOld = Age6061 + Age6264 + Age6574 + Age75P,
      
      BornInState = BornInState / totalPop,
      BornInOtherState = BornInOtherState / totalPop,
      NativeBornOutOfUS = NativeBornOutOfUS / totalPop,
      ForeignBorn = ForeignBorn / totalPop,
      
      NoSchool = Edu2 / totalEdu,
      # didn't complete high school, including no school
      LessThanHS = SumCols("Edu", 2:16) / totalEdu,
      HSGrad = SumCols("Edu", 17:18) / totalEdu,  # didn't complete high school
      SomeColl = SumCols("Edu", 19:21) / totalEdu,  # including associate degree 
      Bach = Edu22 / totalEdu,  # barchelor's
      Master = Edu23 / totalEdu,  # master's
      Prof = Edu24 / totalEdu,  # professional degree
      Doc = Edu25 / totalEdu,  # doctorial
      AtLeastBachelor = Bach + Master + Prof + Doc,
      
      BelowPoverty = Poverty2 / Poverty1,
      
      # how many in poverty are owner occupied households
      BelowPovertyOccupied = SumCols("Poverty", c(4, 8, 11)) / Poverty2,
      # how many in poverty are maried couples
      BelowPovertyMarried = Poverty3 / Poverty2,
      
      White = White / totalPop,
      Black = Black / totalPop,
      Asian = Asian / totalPop,
      Hispanic = Hispanic / totalPop,
      TwoOrMore = TwoOrMore / totalPop,
      EthHet = 1 - (White^2 + Hispanic^2 + Black^2 + Asian^2 + TwoOrMore^2),
      MedHouseIncome = MedHouseIncome,
      OwnerOccupied = OwnerOccupied / GM_Total,
      RenterOccupied = RenterOccupied / GM_Total,
      SameHouse1YearAgo = SameHouse1YearAgo / GM_Total,
      MaleHead = MaleHead / HouseholdTotal,
      FemaleHead = FemaleHead / HouseholdTotal,
      Nonfamily = Nonfamily / HouseholdTotal,
      GrandHead = GrandHead / GrandWithChildTotal,
      PubAssist = PubAssistYes / PubAssistTotal,
      UnempRate = LaborUnemp / LaborTotal,
      GINI = GINI
    ) %>%
    select(-matches("^(Edu|Poverty)")) %>%
    # standardize("MedHouseIncome", "BelowPoverty",
    #            "UnempRate", "AtLeastBachelor",
    #            "White", "Black") %>%
    # convert NAN to NA's, so that the numbers can be safely jsonized
    mutate_all(funs(replace(., is.nan(.), NA)))
  
}

FetchACS <- function(
  endyr = 2015, yrspan = 5, variables = kDefaultVars, state = "MA",
  geo.group = "tract", raw.numbers = FALSE, savefile = TRUE) {
  
  # Generage census indicators for given period
  # Return: a data frame of the indicators
  
  fetch.one <- function(geo.filter) {
    # if fail, return an NA row
    dat <- as.data.frame(t(variables))
    dat[,] <- 0
    tryCatch({
      dat <- acs.fetch(
        endyear = endyr,
        span = yrspan,
        geography = geo.filter,
        variable = variables,
        col.names = names(variables)
      )
    }, error = function(e) e)
    dat
  }
  
  if (geo.group == "tract") {
    geo.filter = geo.make(state = state, county="*", tract = "*")
    dat <- fetch.one(geo.filter)
    geo.m <- geography(dat)
    CT_ID_10 <- geo.m$state * 1e9 + geo.m$county * 1e6 + as.integer(geo.m$tract)
    CT_ID_10 <- str_pad(CT_ID_10, 11, pad = "0")
    dat %<>% estimate() %>% as.data.frame()
    dat <- cbind(NAME = row.names(dat), CT_ID_10, dat)
    row.names(dat) <- NULL
  } else if (geo.group == "county") {
    geo.filter = geo.make(state = state, county="*")
    dat <- fetch.one(geo.filter)
    geo.m <- geography(dat)
    dat %<>% estimate() %>% as_tibble()
    FIPS_ID <- (geo.m$state * 1e3 + as.integer(geo.m$county)) %>%
      str_pad(5, pad = "0")
    dat <- cbind("name" = str_match(geo.m$NAME, "(.*?) County")[, 2],
                 "FIPS_ID" = FIPS_ID,
                 dat)
    row.names(dat) <- NULL
  } else if (geo.group == "city" || geo.group == "town" ||
             geo.group == "countysubdivision") {
    geo.filter = geo.make(state = state, county = "*", county.subdivision="*")
    dat <- fetch.one(geo.filter)
    geo.m <- geography(dat)
    dat %<>% estimate() %>% as_tibble()
    FIPS_ID <- (geo.m$state * 1e5 + as.integer(geo.m$countysubdivision)) %>%
      str_pad(7, pad = "0")
    dat <- cbind("name" = (geo.m$NAME %>%
                   str_match("^(.*?) ([Tt]own|[Cc]ity)"))[, 2],
                 "FIPS_ID" = FIPS_ID,
                 dat) %>%
      filter(!is.na(name))
    row.names(dat) <- NULL
  } else if (geo.group == "zip") {
    f.state <- state
    zips <- filter(zipcode, state == f.state) %>% .$zip
    # zips <- zips[1:4]
    # dat <- data.frame()
    # results <- lapply(zips, function(zip) {
    #   fetch.one(geo.make(zip.code = zip))
    # })
    # dat <- rbind(dat, do.call(rbind, results))
    # dat <- cbind(zip = zips, dat)
    dat <- acs.fetch(
      endyear = endyr,
      span = yrspan,
      geography = geo.make(zip.code = paste0(zips, collapse = ",")),
      variable = variables,
      col.names = names(variables)
    )
    # convert to a data frame
    dat %<>% estimate() %>% as.data.frame()
    # get zip codes from row names 
    zip <- str_match(row.names(dat), "[0-9]{5}")
    # put zip code in a new column
    dat <- cbind("name" = zip, dat) %>% as_tibble()
  } else {
    stop("Unsupported geo unit.")
  }
  
  if (savefile) {
    # startyr <- endyr - span
    # filename <- sprintf("ACS_%s_%s.csv", startyr, endyr)
    # write.csv(dat, file = filename, row.names = F)
  }
  
  dat %<>% mutate(
    name = as.character(name)
  )
  
  if (raw.numbers) {
    return(dat)
  }
  MutateAcs(dat)
}