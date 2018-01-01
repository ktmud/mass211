library(tidyverse)
library(magrittr)
library(stringr)
library(forcats)

source('src/utils/helpers.R')
source('src/utils/topics.R')

if (!exists("resources.raw")) {
  resources.raw <- read_csv(
    "data/211/resources/iCarolExport-Massachusetts211-Resources-20170720_150639.csv",
    col_types=cols(
      SearchHints=col_character(),
      Latitude=col_number(),
      Longitude=col_number()
    ),
    na=c('', 'NA', 'N/A')
    ) %>%
    CleanUselessCols()
}

# ---- Clean up resource ----------
resources <- resources.raw %>%
  mutate(
    address = str_c(MailingAddress1, ' ', MailingAddress2),
    mail_address = if_else(MailingAddressIsPrivate == 'Yes', as.character(NA), address),
    address = str_c(PhysicalAddress1, ' ', PhysicalAddress2),
    # hide private address
    phys_address = if_else(PhysicalAddressIsPrivate == 'Yes', as.character(NA), address),
    # use physical address by default, then fallback to mailing address
    address = FirstNonNA(phys_address, mail_address),
    city = FirstNonNA(PhysicalCity, MailingCity) %>%
      str_to_title() %>%
      fct_recode(Yarmouth = 'Yarmouth Port', Yarmouth = 'Yarmouthport'),
    county = FirstNonNA(PhysicalCounty, MailingCountry) %>% str_to_title(),
    zip = FirstNonNA(PhysicalPostalCode, MailingPostalCode),
    state = FirstNonNA(PhysicalStateProvince, MailingStateProvince),
    lat = if_else(Latitude != 0, Latitude, as.numeric(NA)),
    lng = if_else(Longitude != -90, Longitude, as.numeric(NA))
  ) %>%
  select(id = ResourceAgencyNum,
         public_name = PublicName,
         alternate_name = AlternateName,
         taxonomy_level_name = TaxonomyLevelName,
         desc = AgencyDescription,
         parent_agency = ParentAgency,
         parent_agency_id = ParentAgencyNum,
         site_id = ConnectsToSiteNum,
         program_id = ConnectsToProgramNum,
         address = address,
         city, county, zip, state, lat, lng,
         hours = HoursOfOperation,
         website = WebsiteAddress,
         coverage1 = CoverageArea,
         coverage2 = CoverageAreaText,
         coverage3 = Coverage,
         eligibility = Eligibility,
         taxonomy_term = TaxonomyTerm,
         taxonomy_code = TaxonomyCodes) %>%
  mutate(
    # remove html tags
    desc = str_replace_all(desc, '<(p|br|li)\\b[^>]*>', '\n') %>%
      str_replace_all('<[^>]+>', ' ') %>%
      # repalce the "Replacement character"
      # http://www.fileformat.info/info/unicode/char/fffd/index.htm
      str_replace_all('\uFFFD', '\n') %>%
      str_trim() %>%
      # discard internal note
      replace(str_detect(., '(?i)internal note'), '')
  )

ReportDataCoverage <- function(rsrc) {
  total <- nrow(rsrc)
  print(str_c('Taxonomy: ',
              nrow(rsrc %>% filter(!is.na(taxonomy_code))) / total))
  print(str_c('Address: ', nrow(rsrc %>% filter(!is.na(address))) / total))
  print(str_c(' GeoLoc: ', nrow(rsrc %>% filter(!is.na(lat))) / total))
  print(str_c('Website: ', nrow(rsrc %>% filter(!is.na(website))) / total))
}
# ReportDataCoverage(programs)
# ReportDataCoverage(sites)
# ReportDataCoverage(agencies)

# Normalized resources dataset ----------------
NormalizedResources <- function(rsrc) {
  #
  # Convert the all-in-one resources dataset to
  # a list of normalized data frames
  #
  agencies <- rsrc %>%
    filter(taxonomy_level_name == 'Agency') %>%
    select(agency_id = id,
           agency_name = public_name,
           agency_name_alt = alternate_name,
           agency_desc = desc,
           agency_address = address,
           agency_website = website)
  programs <- rsrc %>%
    filter(taxonomy_level_name == 'Program') %>%
    select(id,
           program_name = public_name,
           agency_id = parent_agency_id,
           taxonomy_code, taxonomy_term)
    # group_by(agency_id) %>%
    # summarise(
    #   # merge program names available from this site
    #   programs = str_c(program_name, collapse = '; '),
    #   program_taxonomy_term = MergeDedup(taxonomy_term),
    #   program_taxonomy_code = MergeDedup(taxonomy_code)
    # )
  sites <- rsrc %>%
    filter(taxonomy_level_name == 'Site') %>%
    rename(agency_id = parent_agency_id) %>%
    select(-starts_with('taxonomy'))
  program_sites <- rsrc %>%
    filter(taxonomy_level_name == 'ProgramAtSite') %>%
    select(site_id, program_id, coverage = coverage3) %>%
    left_join(programs, by = c('program_id'='id')) %>%
    # collect programs available at each site
    # we are assuming all programs have the same coverage
    group_by(site_id) %>%
    summarize(
      coverage = MergeDedup(coverage) %>%
        str_replace_all('MA - ', '') %>%
        str_replace_all(' County - ', ' - '),
      programs = MergeDedup(program_name),
      taxonomy_code = MergeDedup(taxonomy_code),
      taxonomy_term = MergeDedup(taxonomy_term)
    )
  list(
    agencies = agencies,
    programs = programs,
    sites = sites,
    program_sites = program_sites
  )
}

# Consolidated resource info -----------------
CompleteResourcesInfo <- function(rsrc) {
  # Complete resource info with pieces from sites, programs and agencies.
  #
  #    * Agency - Name, website
  #    * Site - GeoLocation, address, hours
  #    * Programs - Taxonomy Code
  #    * ProgramSites - Coverage
  #
  rsrc$sites %>%
    left_join(rsrc$agencies, by = 'agency_id') %>%
    left_join(rsrc$program_sites, by = c('id'='site_id')) %>%
    # agency and site info fallbacks ------------
    mutate(
      # short agency name
      agency_name_s = str_replace(agency_name, ', (Town|City) of', ''),
      # if name look like an address, then move name to the address
      address = if_else(
        is.na(address) & str_detect(public_name, "^[0-9]+( |-)"),
        public_name,
        address
      ),
      # use agency name as site name, if site name is not helpful
      name = if_else(
        # Contains "Main site", or starts with a number (address like)
        str_detect(public_name, '(?i)(\\bmain site)'),
        str_c(agency_name, ' (Main Site)'),
        str_c(
          agency_name,
          ' - ',
          public_name %>%
            # remove agency name
            str_replace(str_c('(?i)\\(?', agency_name_s, '\\)?(, (Town|City) of)?'), '') %>%
            # remove state and post code
            str_replace(', MA [0-9]+', '') %>%
            str_trim() %>%
            # remove street number and stange characters
            str_replace('^[0-9\\ \\-\\:]+[A-Z]? ', '')
        ) %>% str_replace(' - $', '')
      ),
      name_alt = FirstNonNA(alternate_name, agency_name_alt),
      # split
      name_alt = name_alt %>%
        str_split(' *[,;\\*] *') %>%
        map(function(x) {
          unique(x) %>%
            .[str_detect(., ".+")] %>%
            str_c(collapse=', ')
        }),
      desc = agency_desc,
      # [Confirmed] this is safe,
      # only a couple of sites has agency address but not address
      address = FirstNonNA(address, agency_address),
      website = FirstNonNA(website, agency_website),
      website = str_replace(website, '^www', 'http://www')
    ) %>%
    # rearrange columns ------
    select(
      id, name, name_alt, city:lng,
      address, coverage, desc,
      # Site website contains no extra info
      # than agency website
      website, hours, programs,
      taxonomy_term, taxonomy_code
    ) %>%
    # remove duplicated
    distinct(name, lat, lng, .keep_all = TRUE)
}

rsrc <- NormalizedResources(resources)
sites <- CompleteResourcesInfo(rsrc)

# Attach call count -----------
if (!exists('referrals')) {
  referrals <- read_csv("data/211/20170209/Referrals.csv")
}
site.ref.count <- referrals %>%
  count(SiteResourceNum) %>%
  rename(id = SiteResourceNum, n_call=n)
sites %<>% left_join(site.ref.count)
  
# Export JSON for client side ---------------
sites.ma <- sites %>%
  filter(state == 'MA') %>%
  mutate(
    topic = Cat2Topic(taxonomy_code)
  )
sites.ma.json <- sites.ma %>%
  select(-id) %>%
  # use only those with a geo location
  filter(!is.na(lat), !is.na(name)) %>%
  mutate(
    coverage = str_split(coverage, '; '),
    programs = str_split(programs, '; '),
    topic = str_split(topic, '; '),
    taxonomy_term = str_split(taxonomy_term, '; '),
    taxonomy_code = str_split(taxonomy_code, '; '),
    lat = as.numeric(lat),
    lng = as.numeric(lng),
    "_geoloc" = pmap(list(lat=lat, lng=lng), list),
    lat = NULL,
    lng = NULL
  )
write_file(jsonlite::toJSON(sites.ma.json, pretty=TRUE,
                            null = 'list',
                            auto_unbox = TRUE),
           'm2m/static/data/resources.json')
