topics <- list(
  # Employment
  income = c(
    "N",       # Income Support and Employment
    "FT-1000.8900"
  ),
  # Criminal Justice and Legal Services
  legal = c(
    "F"
  ),
  government = c(
    "TD-0300",      # Administrative Entities
    "TD-1100",      # Categorical Program Administrative Units
    "TH-1500",      # City Offices of Emergency Services
    "JR",           # Public Safety
    "TD-6600",      # Public Officials Offices
    "TE"            # Community Planning and Public Works
  ),
  education = c(
    "PH-6100",  # Home Based Parenting Education
    "H"        # Education
    # "HD-8200":  # School District
    # "HD-0500",  # Alternative Education;
    # "HH-0500.8000"  # English as a Second Language
  ),
  infoservice = c(
    "TJ"            # Information Services
  ),
  `community` = c(
    "TD-1200",         # Charities/Grantmaking Organizations
    "TD-1600",         # Community Action/Social Advocacy Groups
    "TD-6500",         # Planning/Coordinating/Advisory Groups
    "PH-2950.3200",    # Holiday Gifts/Toys
    "TQ-1500",         # Community Involvement Programs
    "TN"               # Occupational/Professional Associations
  ),
  # Child Care Needs
  childcare = c(
    "PH-1250",  # Child Care Providers
    "HD-1800",  # Early Childhood Education
    "PH-6500.1500", # Children’s Protective Services
    "NL-3000.1500" # child care expense program
  ),
  `youth help` = c(
    "PH-6100",  # Home Based Parenting Education
    "PH-6000",
    "PS-9800",
    "PH-6300", # Placements for Children and Youth
    "HD-0500"  # Alternative Education;
  ),
  `care/companion` = c(
    "PH-0320",  # Adult Day Programs
    "PH-1000",  # 	Case/Care Management
    "PH-1400"  # 	Companionship
  ),
  # housing/shelter
  housing = c(
    "BH",
    "BM-3000",         # Household goods (mostly furnitures)
    "TD-1600.2600",    # Homelessness Advocacy Groups
    "TD-1600.2800",    # 	Housing Advocacy Groups
    "YV"               # At Risk for Homelessness / Homeless people
  ),
  # Food and Clothes
  `food/cloth` = c(
    "BD",       # Basic Needs - Food
    "BM-6500",  # Personal Goods/Services (Clothing)
    "TI-1800.1500",  # Clothing Donation Programs
    "NL-6000",  # Nutrition Related Public Assistance Programs (SNAP/WIC)
    "YC-2000"   # Food Stamps/SNAP Recipients
  ),
  # homeless only
  homeless = c(
    "BH-0500",         # homeless/at-risk prevention
    "BH-1800.3500",    # Homeless Drop In Centers
    "BH-1800.8500",    # Homeless Shelter
    "BH-1800.9000",    # Medical Respite Facilities/Beds for Homeless People
    "TD-1600.2600",    # Homelessness Advocacy Groups
    "YV"               # At Risk for Homelessness / Homeless people
  ),
  # Mental Health and Substance Use Disorder Services
  mental = c(
    "R",
    "PN-8100.5000"
  ),
  # Substance Use Disorder Services	
  `substance abuse` = c(
    "RX"
  ),
  # (Personal) Health Care
  health = c(
    "L",
    # "JP",  # Public health    E.g. JP-1500.1700-900 - West Nile Virus Control
    "YF"   # Disabilities and Health Conditions
  ),
  # People with Disabilities
  disability = c(
   "BT-4500.6500",  # Paratransit Programs
   "BT-4500.6500-170",
   "DF-7000.6550-170",
   "FT-1000.6600",
   "LF-4900.2150",      # Hearing Screening
   "LH-0350",           # Aging and Disability Resource Centers
   "LH-2700.1700",      # Disease/Disability Information
   "LR-1700",           # Early Intervention for Children With Disabilities/Delays
   "LR-3100",           # Home/Community Based Developmental Disabilities Programs,
   "ND-6500.1500",      # Comprehensive Disability Related Employment Progra
   "ND-6500.1800",      # Disability Related Center Based Employment
   "NS-1800",           # Disability Benefits
   "PH-6100.1700",      # Disability Related Parenting Programs
   "PN-8100.3000",      # Health/Disability Related Support Groups
   "RP-1400.8000-300",  # Health/Disability Related Counseling
   "TD-1600.3100-180",  # Disability Rights Groups
   "YC-1700",           # Disability Benefit Recipients
   "YF",                # Disabilities and Health Conditions
   "YJ-8750"            # People With Disabilities/Health Conditions
  ),
  # utilities
  utilities = c(
    "BV"
  )
)
# topic must start with `topics`,
# but not start with `topic_excludes`
topic_excludes <- list(
  legal = c(
    "FT-1000.8900"  # Unemployment Insurance Benefits Assistance
  ),
  income = c(
    "NL-3000",      # child care
    "NL-6000"  # Nutrition Related Public Assistance Programs (SNAP/WIC)
  ),
  community = c(
    "TD-1600.2600"   # Homelessness Advocacy Groups
  )
)

# convert to regular expression
topics %<>% map(function(x) {
  str_c('\\b(', str_c(x, collapse='|'), ')')
})
topic_excludes %<>% map(function(x) {
  str_c('\\b(', x, ')', collapse='|')
})


# calls with certain category
# topic_calls <- topics %>% lapply(function(item) {
#   mcalls %>%
#     filter(startsWith(cat.code, item))
# })
# topic_sizes <- topic_calls %>% lapply(function(item) {
#   nrow(item)
# })

TopicCount <- function(mcalls, topic, geo.unit) {
  is_negative <- FALSE
  if (substr(topic, 0, 1) == "-") {
    is_negative <- TRUE
    topic <- str_replace(topic, "^-", "")
  }
  idx <- rowSums(topic == str_split(mcalls$topic, '; ', simplify=TRUE)) %>%
    as.logical()
  is.na(idx) <- FALSE
  if (is_negative) {
    idx <- !idx
  }
  calls <- mcalls[idx, ]
  dat <- GeoCount(calls, geo.unit) %>%
    # name is the name for given geo.unit
    select(name, p_call, n_call)
  topic <- topic %>% str_replace_all('[\\/ ]', '_')
  # rename `p_call` to contain topic
  p_call <- str_c("p_call_", ifelse(is_negative, "no_", ""), topic)
  n_call <- str_c("n_call_", ifelse(is_negative, "no_", ""), topic)
  dat[p_call] <- dat$p_call
  dat[n_call] <- dat$n_call
  dat$p_call <- NULL
  dat$n_call <- NULL
  dat
}

Cat2Topic <- function(cats) {
  ret <- list()
  # change NA to empty string, so that `str_detect`
  # wouldn't return NA
  cats[is.na(cats)] <- ''
  for (topic in names(topics)) {
    topic_cats <- topics[[topic]]
    neg_topic_cats <- topic_excludes[[topic]]
    idx <- str_detect(cats, topic_cats)
    if (!is.null(neg_topic_cats)) {
      idx <- idx & !str_detect(cats, neg_topic_cats)
    }
    ret[idx] <- str_c(ret[idx], '; ', topic)
  }
  str_replace_all(ret, 'NULL; ', '') %>%
    fct_recode(NULL = 'NULL')
}

# Calls by topic and geo
# calls.topic.town <- CountByTopics()
# calls.topic.town <- CountByTopics()