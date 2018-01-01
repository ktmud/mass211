##
# Load Census Data At Any Geographic Level
#
##
library(zipcode)
library(magrittr)
library(dplyr)
library(stringr)
library(acs)

# load all zipcodes
data(zipcode)

# as copied from the BARI's ACS indicators R syntax
kAPIKey <- "829a6baee8d09366819cd05f131b47aeaf232576"
api.key.install(kAPIKey)

# Build Sequence of multi variables
MultiVar <- function(name, total) {
  str_c(name, '_', str_pad(seq(1, total), 3, pad = "0"))
}


# A list of default variables to fetch, must be one name one code
# can use `MultiVar` to include multi variables, in which case the variable
# names will have a 1-n suffix.
# Ref1: https://api.census.gov/data/2015/acs5/variables.html
# Ref2: https://www.socialexplorer.com/data/ACS2015_5yr/metadata/?ds=ACS15_5yr
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
  # Universe: Population 25 years and Over
  Edu = MultiVar("B15003", 25),
  
  # Child In Need
  CIN = MultiVar("B09010", 13),
  
  # Citizenship
  # CitizenTotal = "B05002_001",   # the same as TotalPop
  BornInState = "B05002_003",
  BornInOtherState = "B05002_004",
  NativeBornOutOfUS = "B05002_009",
  ForeignBornNaturalized = "B05002_014",
  ForeignBornNonCitizen = "B05002_021",
  
  # Health Insurance
  HI = MultiVar("B27020", 16),
  
  White = "B03002_003",
  Black = "B03002_004",
  AmIndian = "B03002_005",
  Asian = "B03002_006",
  PacificIslander = "B03002_007",
  OtherRace = "B03002_008",
  Hispanic = "B03002_012",
  TwoOrMore = "B03002_009", # two or more races
  
  Veteran = MultiVar("B21001", 3),
  
  # median household income
  MedHouseIncome = "B19013_001",
  # median housing cost
  MedHousingCost = "B25105_001",
  
  # Universe: Owner-occupied housing units
  MedHouseValue = 'B25077_001',
  
  # Median Gross Rent as a Percentage of Household Income
  # Universe: Renter-occupied housing units paying cash rent
  MedRentAsIncomePct = "B25071_001",
  
  # Gini Index of Income Inequality
  GINI = "B19083_001",
  
  PubAssistTotal = "B19057_001",
  PubAssistYes = "B19057_002",  # household with public assistantship
  
  # FamilyPoverty = str_c("B17019_", str_pad(1:12, 3, pad = "0")),
  
  Poverty = MultiVar("C17002", 8),
  
  LaborTotal = "B23025_003",  # civilian in labor force
  LaborUnemp = "B23025_005",
  
  # Geographical Mobility in the Past Year by Tenure for Current Residence in the US
  # Universe: Population 1 year and Over in households in the United States
  GM = MultiVar("B07013", 4),
  
  # Universe: Occupied housing units
  OH = MultiVar("B25003", 3),
  
  # Occupancy Status
  # Universe: Housing units
  OS = MultiVar("B25003", 3),
  
  # Household type
  Household = MultiVar("B11001", 9),
  # Grandparents as caregivers
  # Universe: Population 30 years and Over
  GH = MultiVar("B10050", 3)
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
      ForeignBornNonCitizen = ForeignBornNonCitizen / totalPop,
      ForeignBornNaturalized = ForeignBornNaturalized / totalPop,
      
      # Percentage of verterans (for population >= 18 years old)
      Veteran = Veteran2 / Veteran1,
      
      # Health Insurance
      HI_PrivateInsured = (HI4 + HI10 + HI15) / HI1,
      HI_PublicInsured = (HI5 + HI11 + HI16) / HI1,
      HI_Insured = (HI3 + HI9 + HI14) / HI1,
      
      NoSchool = Edu2 / totalEdu,
      # didn't complete high school, including no school
      LessThanHS = SumCols("Edu", 2:16) / totalEdu,
      HSGrad = SumCols("Edu", 17:18) / totalEdu,  # Regular High School + GED
      SomeColl = SumCols("Edu", 19:21) / totalEdu,  # including associate degree 
      Bach = Edu22 / totalEdu,  # barchelor's
      Master = Edu23 / totalEdu,  # master's
      Prof = Edu24 / totalEdu,  # professional degree
      Doc = Edu25 / totalEdu,  # doctorial
      AtLeastBachelor = Bach + Master + Prof + Doc,
      
      MedHouseIncome = MedHouseIncome,
      # MedHousingCost = MedHousingCost * 12,
      MedRentAsIncomePct = MedRentAsIncomePct / 100,
      
      # number of residents per occupied housing units
      PopPerHousing = if_else(OS2 == 0, as.numeric(NA), TotalPop / OS2),
      VacentUnits = OS3 / OS1,
      OwnerOccupied = OH2 / OH1,
      RenterOccupied = OH3 / OH1,
      SameHouse1YearAgo = GM4 / GM1,
      
      BelowHalfPoverty = Poverty2 / Poverty1,
      BelowPoverty = (Poverty2 + Poverty3) / Poverty1,
      BelowTwoPoverty = (Poverty1 - Poverty8) / Poverty1,
      AboveTwoPoverty = Poverty8 / Poverty1,
      PubAssist = PubAssistYes / PubAssistTotal,
      PubAssistYes = NULL,
      PubAssistTotal = NULL,
      UnempRate = LaborUnemp / LaborTotal,
      
      ChildInNeed = CIN2 / CIN1,
      ChildInNeed_SingleParent = (CIN5 + CIN6) / CIN1,
      ChildInNeed_SingleMom = CIN6 / CIN1,
      ChildInNeed_Nonfamily = CIN7 / CIN1,
      
      # FamilyBelowPoverty = FamilyPoverty2 / FamilyPoverty1,
      # # how many in poverty are owner occupied households
      # FamilyBelowPovertyOccupied = SumCols("FamilyPoverty", c(4, 8, 11)) / FamilyPoverty2,
      # # how many in poverty are maried couples
      # FamilyBelowPovertyMarried = FamilyPoverty3 / FamilyPoverty2,
      
      White = White / totalPop,
      Black = Black / totalPop,
      Asian = Asian / totalPop,
      # AmIndian = AmIndian / totalPop,
      # PacificIslander = PacificIslander / totalPop,
      OtherRace = (AmIndian + PacificIslander + OtherRace) / totalPop,
      AmIndian = NULL, PacificIslander = NULL,
      Hispanic = Hispanic / totalPop,
      TwoOrMore = TwoOrMore / totalPop,
      EthHet = 1 - (White^2 + Hispanic^2 + Black^2 + Asian^2 + OtherRace^2 + TwoOrMore^2),
      
      HH_MarriedCouple = Household3 / Household1,
      HH_SingleHead = Household4 / Household1,
      HH_MaleHead = Household5 / Household1,
      HH_FemaleHead = Household6 / Household1,
      HH_Nonfamily = Household7 / Household1,
      HH_LiveAlone = Household8 / Household1,
      HH_NotAlone = Household9 / Household1,
      
      GrandHead = GH3 / GH1,
      GINI = GINI
    ) %>%
    select(
      # clear all multi-cat variables
      # can't start with A, A is reserverd for "Age"
      -matches("^[B-Z]([a-zA-Z]*)[0-9]"),
      -matches('Labor')
    ) %>%
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
    dat <- cbind("name" = row.names(dat), CT_ID_10, dat)
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


# aci.county <- FetchACS(geo.group = "county")

# `aci` is ACS indicators
if (!exists("aci.zip")) {
  aci.zip.orig <- FetchACS(geo.group = "zip", raw.numbers = TRUE)
  aci.zip <- MutateAcs(aci.zip.orig)
  # Remove extrem values (the Smith College - an all-girl university)
  aci.zip %<>% filter(Female < 0.9)
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
  ru.zip <- read_csv("data/rural_urban_zip.csv", col_types=cols(
    .default = col_number(),
    UA = col_character(),
    UANAME = col_character(),
    ZCTA5 = col_character()
  ))
  zip.urbanpop <- ru.zip %>%
    # filter(!str_detect(UANAME, "Not in")) %>%
    mutate(
      is_non_urban = str_detect(UANAME, "Not in"),
      is_urbanized = str_detect(UANAME, "Urbanized Area"),
      is_cluster = str_detect(UANAME, " Cluster$"),
      ZPOP = as.numeric(ZPOP),
      ZAREALAND = as.numeric(ZAREALAND)
    ) %>%
    group_by(zip = ZCTA5) %>%
    summarize(
      # calculate percentage of people who live in any of the urbanized areas
      # that overlaps with this ZIP code
      p.urban_pop = sum(if_else(is_non_urban, 0, ZPOPPCT)),
      # urban cluster population
      p.urbanized_pop = sum(if_else(is_urbanized, ZPOPPCT, 0)),
      p.urban_cluster_pop = sum(if_else(is_cluster, ZPOPPCT, 0)),
      total_pop = mean(ZPOP),
      total_land = sum(ZAREALAND) / 1000000,
      total_urban_land = sum(if_else(is_non_urban, 0, ZAREALAND)) / 1000000,
      # population density of the whole zip code
      pop_den = total_pop / total_land,
      urban_popden = if_else(total_urban_land == 0, as.numeric(NA),
                             total_pop / total_urban_land)
    )
}
