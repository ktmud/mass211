##
# Collect and process Health Status Indicators data from MassCHIP website
##
library(tidyverse)
library(rvest)
library(stringr)
library(purrr)

setwd('data/health-indicators/')

DownloadHSI <- function() {
  # Download Mass Health Status Indicators data from MassHCI
  start_url <- 'http://www.mass.gov/eohhs/researcher/community-health/masschip/health-status-indicators.html'
  urls <- read_html(start_url) %>%
    # 3 for town and 5 for counties
    html_nodes('div.col.col12.bodyfield > ul') %>% .[c(3,5)] %>%
    html_nodes('a.titlelink') %>%
    html_attr('href')
  urls <- str_c('http://www.mass.gov', urls)
  walk(urls, function(x) {
    if (str_detect(x, 'hsicounty')) {
      system(str_c('curl -O ', x))
    }
  })
  system('textutil -convert html ./*.rtf')
  system('rm *.rtf')
}

ExtractHSI <- function(file) {
  dom <- read_html(file)
  # all html tables
  tables <- dom %>% html_nodes('table.t2')
  # area name
  area <- dom %>%
    html_nodes('p.p2 > b') %>% .[2] %>%
    html_text() %>%
    str_match("\nfor (.+)") %>% .[2]
  # convert tables to data frames
  df <- html_table(tables)
  dat <- c(
    name = area,
    # -- Demographics --
    income = str_replace_all(df[[1]][2, 3], "[\\$,]", ""),
    poverty100 = as.numeric(df[[1]][3, 3]) / 100,
    poverty200 = as.numeric(df[[1]][4, 3]) / 100,
    child_poverty100 = as.numeric(df[[1]][5, 3]) / 100,
    umemployed = as.numeric(df[[1]][6, 3]) / 100,
    ageu18 = as.numeric(df[[2]][2, 3]) / 100,
    ageu20 = as.numeric(df[[2]][3, 3]) / 100,
    age65p = as.numeric(df[[2]][4, 3]) / 100,
    white = as.numeric(df[[2]][5, 3]) / 100,
    black = as.numeric(df[[2]][6, 3]) / 100,
    hispanic = as.numeric(df[[2]][7, 3]) / 100,
    asian = as.numeric(df[[2]][8, 3]) / 100,
    
    # mediciad recipients
    afdc_medicaid = as.numeric(df[[2]][9, 3]) / 100,
    multi_assist_medicaid = as.numeric(df[[2]][10, 3]) / 100,
    
    # -- perinatal and child health --
    # births to women ages 15 to 44
    fertility = df[[3]][2, 3],
    fertility_white = df[[3]][3, 3],
    fertility_black = df[[3]][4, 3],
    fertility_hispanic = df[[3]][5, 3],
    fertility_asian = df[[3]][6, 3],
    infantmortality = df[[3]][8, 3],
    infantmortality_white = df[[3]][9, 3],
    infantmortality_black = df[[3]][10, 3],
    infantmoratlity_hispanic = df[[3]][11, 3],
    infantmortality_asian = df[[3]][12, 3],
    lowbirthweight = as.numeric(df[[3]][14, 3]) / 100,
    teenagemoms = as.numeric(df[[3]][15, 3]) / 100,
    moms_no_care = as.numeric(df[[3]][16, 3]) / 100,
    moms_adeq_care = as.numeric(df[[3]][17, 3]) / 100,
    moms_pub_care = as.numeric(df[[3]][18, 3]) / 100,
    # children of 6mos to 5yrs
    child_lead_poison = as.numeric(df[[3]][20, 3]) / 100,
    
    # Infectious Disease
    # area crude rate (per 100,000 persons)
    hiv = df[[4]][2, 3],
    hiv_aids = df[[4]][3, 3],
    hiv_death = df[[4]][4, 3],
    tuberculosis = df[[4]][5, 3],
    pertussis = df[[4]][6, 3],
    hepb = df[[4]][7, 3],
    syphilis = df[[4]][8, 3],
    gonorrhea = df[[4]][9, 3],
    chlamydia = df[[4]][10, 3],
    
    # Injury death
    motor_death = df[[5]][2, 3],
    suicide = df[[5]][3, 3],
    homicide = df[[5]][4, 3],
    
    # Chronic Disease
    chronic_death = df[[6]][2, 3],
    cancer_death = df[[6]][3, 3],
    lung_cancer_death = df[[6]][4, 3],
    breast_cancer_death = df[[6]][5, 3],
    # hearth and blood vessels disease
    cardiovascular_death = df[[6]][6, 3],
    
    # Substance Abuse: admissions to DPH funded
    # treatment program
    substance_abuse = df[[7]][2, 3],
    injection_drug = df[[7]][3, 3],
    alcohol_other_drug = df[[7]][4, 3],
    
    # Hospital Discharges for Primary Care Manageable Conditions
    asthma = df[[8]][2, 3],
    angina = df[[8]][3, 3],
    # Bacterial pneumonia
    pneumonia = df[[8]][4, 3]
  )
  dat
}

# hsi.town <- list.files() %>%
#   .[str_detect(., "hsicity-town.*.html$")] %>%
#   map(ExtractHSI) %>%
#   do.call(rbind, .) %>%
#   as_tibble() %>%
#   # convert all columns other than the name column to be numerics
#   mutate_at(vars(-matches("name")), as.numeric)
# write_csv(hsi.town, "hsi.town.csv")

hsi.county <- list.files() %>%
  .[str_detect(., "hsicounty.*.html$")] %>%
  map(ExtractHSI) %>%
  do.call(rbind, .) %>%
  as_tibble() %>%
  mutate(
    name = str_replace(name, " *County", "")
  ) %>%
  select(-hiv) %>% # HIV column is always empty for counties
  # convert all columns other than the name column to be numerics
  mutate_at(vars(-matches("name")), as.numeric)
write_csv(hsi.county, "hsi.county.csv")


setwd('../../')
