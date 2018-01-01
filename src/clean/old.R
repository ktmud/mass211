if (!exists('mcalls.old.raw')) {
  mcalls.old.raw <- read_excel('data/211/Archived RH taxonomy data.xlsx',
                               col_types = c(rep('text', 17), 'date'))
}
colnames(mcalls.old.raw)[1] <- 'Search Type'

mcalls.old <- mcalls.old.raw %>%
  filter(
    `Search Type` != 'Provider'
  ) %>%
  mutate(
    date = as_date(`CreateDate-DateOnly`),
    zip = str_pad(Zip, 5, 'left', '0'),
    town = str_to_title(City),
    # some records does not have taxonomy, we have to infer
    # from `Description`
    desc = str_to_title(Description) %>%
      str_replace(".*Environmental.*", "Environmental Protection And Improvement") %>%
      str_replace(".*Mental Health.*", "Mental Health And Substance Use Disorder Services"),
    cat.name = str_to_title(TermSearchTaxonomyText),
    cat.name = FirstNonNA(cat.name, desc) %>%
      # fix renamed taxonomy terms
      fct_recode(
        "Food Stamps/SNAP" = "Snap",
        "Food Stamps/SNAP" = "Food Stamp",
        "Electric Service Payment Assistance" = "Electric",
        "BH" = "Housing/Shelter",
        "Rent Payment Assistance" = "Rent",
        "Unemployment Insurance Benefits Assistance" = "Unemployment Benefits",
        # CFCE = Coordinated Family and Community Engagement Programs
        # One of the public assistance programs by Mass goverment
        "Public Assistance Programs" = "Cfce",
        "Section 8 Housing Choice Vouchers" = "Section 8",
        "Utility Bill Payment Plans" = "Utility Bill",
        "Child Care Expense Assistance Applications" = "Child Care Expense Applications",
        "Gas Service Payment Assistance" = "Gas Payment",
        "Early Literacy Development Programs" = "Early Literacy",
        "At Risk/Homeless Housing Related Assistance Programs" = "At Risk/Homeless",
        "Landlord/Tenant Assistance" = "Landlord Tenant Assistance",
        "At Risk/Homeless Housing Related Assistance Programs" = "At Risk/Homeless",
        "Child Care Expense Assistance" = "Child Care Expense",
        "Child Care Expense Assistance" = "Childcare",
        "Child Care Expense Assistance" = "Childcare*",
        "Child Support Assistance/Enforcement" = "Child Support Enforcement",
        "Utility Bill Payment Assistance" = "Utility",
        "Utility Bill Payment Assistance" = "Utility*",
        "Utility Bill Payment Assistance" = "Utilities*",
        "Public Utility Regulation/Deregulation" = "Public Utilities*",
        "Public Utility Regulation/Deregulation" = "Public Utilities",
        "Utility Bill Payment Assistance" = "Utility Payment Assistance",
        "Discounted Utility Services" = "Utility Discount",
        "Discounted Utility Services" = "Discounted Utility",
        "Public Utility Regulation/Deregulation" = "Utility Regulation",
        "Public Utility Regulation/Deregulation" = "Public Utility Regulation",
        "Public Utility Regulation/Deregulation" = "Public Utility Deregulation",
        "Scholarships" = "Scholarship"
      ) %>% as.character(),
    # recode updated taxonomy code
    cat.code = str_to_upper(`TermSearchTaxonomy Code`) %>%
      fct_recode(
        "BV-9000.9600" = "BH-9000.9600"
      ) %>% as.character()
  ) %>%
  # only care about calls within MA
  filter(
    State == 'MA'
  ) %>%
  select(
    date,
    zip,
    town,
    desc,
    cat.code,
    cat.name
  )

# Following code tries to complete `cat.code`
# by matching `desc` with `cat.name`.
taxonomy <- mcalls.old %>%
  select(taxo_code = cat.code, cat.name, date) %>%
  # always use latest cat code
  arrange(taxo_code, desc(date)) %>%
  distinct(cat.name, .keep_all=TRUE) %>%
  select(-date)

mcalls.old %<>%
  left_join(taxonomy, by='cat.name') %>%
  mutate(
    # taxo_code is code matched from the taxo name of later inputs
    cat.code = FirstNonNA(taxo_code, cat.code),
    taxo_code = NULL,
    desc = NULL
  ) %>%
  filter(!is.na(cat.code)) %>%
  left_join(ztc %>% select(zip, county_by_zip=county) %>% distinct()) %>%
  left_join(ztc %>% select(town, county_by_town=county) %>% distinct()) %>%
  mutate(
    county = FirstNonNA(county_by_zip, county_by_town),
    county_by_zip = NULL,
    county_by_town = NULL
  )

mcalls.old <- cbind(list(id=str_c('old_', seq(1, nrow(mcalls.old)))), mcalls.old)
