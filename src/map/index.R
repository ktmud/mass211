library(rgdal)
library(rgeos)
library(maptools)
library(geojsonio)
library(spdplyr)
library(rmapshaper)
library(pscl)

# Projection for MassGIS data
# Ref: http://spatialreference.org/ref/sr-org/15/
#      http://www.mass.gov/anf/research-and-tech/it-serv-and-support/application-serv/office-of-geographic-information-massgis/datalayers/overview.html
proj4mass <- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")

ReadOGR <- function(dat, layer) {
  readOGR(dat, layer = layer) %>% spTransform(proj4mass)
}

kDefaultModel <- formula(
  n_call ~
    White +
    MedHouseValue + 
    MedHouseIncome +
    OwnerOccupied +
    HH_LiveAlone +
    HH_SingleHead + 
    AtLeastBachelor +
    offset(log(TotalPop))
)
# Zero-Inflated Poisson
kDefaultZIPModel <- formula(
  n_call ~
    White +
    MedHouseValue + 
    MedHouseIncome +
    OwnerOccupied +
    HH_LiveAlone +
    HH_SingleHead + 
    AtLeastBachelor | TotalPop
)
AddResidual <- function(dat, formula=kDefaultZIPModel) {
  #
  # Add Residual for count data
  #
  dat1 <- dat
  if (nrow(dat1) > 200) {
    dat1 %<>%
      # if we have enough observations
      # remove extreme values (they are mostly caused by small population)
      filter(p_call < quantile(p_call, .99, na.rm=TRUE))
  }
  dat1 %<>%
    mutate(
      residual = NULL,
      shape_area = as.numeric(shape_area),
      TotalPop = as.numeric(TotalPop)
    ) %>%
    # count numbers are integers
    mutate_at(vars(starts_with('n_')), as.integer)
  # m1 <- glm(formula, data = dat1, family = poisson())
  dat1 <- dat %>%
    select(PopDen, White, Black, MedHouseValue, MedHouseIncome, OwnerOccupied, HH_LiveAlone,
           HH_SingleHead, AtLeastBachelor, HI_PrivateInsured) %>%
    scale() %>%
    as.data.frame()
  dat1 %<>% mutate(
    n_call = dat$n_call,
    TotalPop = dat$TotalPop
  )
  # if not too many zeros
  if (nrow(dat1 %>% filter(n_call == 0)) < 0.1 * nrow(dat1)) {
    summary(m2 <- MASS::glm.nb(
      n_call ~ White + 
        MedHouseValue +
        MedHouseIncome +
        HH_LiveAlone +
        HH_SingleHead +
        HI_PrivateInsured +
        AtLeastBachelor +
        PopDen +
        offset(log(TotalPop)),
      data = dat1))
  } else {
    summary(m2 <- zeroinfl(
      n_call ~ White + Black +
        # MedHouseValue +
        # MedHouseIncome +
        OwnerOccupied +
        HH_LiveAlone +
        HH_SingleHead +
        # HI_PrivateInsured +
        AtLeastBachelor +
        PopDen +
        offset(log(TotalPop)) |
        Black + PopDen + offset(log(TotalPop)),
      data = dat1))
  }
  dat[names(m2$residuals), 'residual'] <- m2$residuals
  dat
}

## Add geographic metrics
source("src/map/county.R")
source("src/map/town.R")
source("src/map/zip.R")

