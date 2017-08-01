topics <- list(
  # Employment
  employment = c(
    "ND" 
  ),
  # Income Support/Expense Assistance
  income = c(
    "NL-1000",  # Basic Income Maintainence
    "NL-5000",  # Medical
    "NL-6000",  # Nutrition Related Public Assistance Programs
    "BH-3800",  #	Housing Expense Assistance
    "LH-5100",  # Medical Expense Assistance
    "FT-1000.8900",  # Unemployment Insurance Benefits Assistance
    "NS"  # Social Insurance
  ),
  # Criminal Justice and Legal Services
  legal = c(
    "F"
  ),
  # Child Care Needs
  childcare = c(
    "PH-1250",  # Child Care Providers
    "HD-1800",  # Early Childhood Education
    "NL-3000.1500" # child care expense program
  ),
  # housing/shelter
  housing = c(
    "BH-0500",  # At Risk/Homeless Housing Related Assistance Programs
    "BH-1800",  # Emergency Shelter
    "BH-8500",  # Supportive Housing Placement/Referral
    "TD-1600.2600",  # Homelessness Advocacy Groups
    "BH-8600"   # Transitional Housing/Shelter
  ),
  food = c(
    "BD",       # Basic Needs - Food
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
    "R"
  ),
  # Substance Use Disorder Services	
  substance = c(
    "RX"
  ),
  # Health Care
  health = c(
    "H"
  ),
  # utilities
  utilities = c(
    "BV"
  )
)

# calls with certain category
# topic_calls <- topics %>% lapply(function(item) {
#   mcalls %>%
#     filter(startsWith(cat.code, item))
# })
# topic_sizes <- topic_calls %>% lapply(function(item) {
#   nrow(item)
# })

TopicCount <- function(topic, geo.unit) {
  is_negative <- FALSE
  if (substr(topic, 0, 1) == "-") {
    is_negative <- TRUE
    topic <- str_replace(topic, "^-", "")
  }
  topic_cats <- topics[[topic]]
  idx <- startsWith(mcalls$cat.code, topic_cats)
  if (is_negative) {
    idx <- !idx
  }
  calls <- mcalls[idx, ]
  dat <- GeoCount(calls, geo.unit) %>%
    # name is the name for given geo.unit
    select(name, p_call, n_call)
  # rename `p_call` to contain topic
  p_call <- str_c("p_call_", ifelse(is_negative, "no_", ""), topic)
  n_call <- str_c("n_call_", ifelse(is_negative, "no_", ""), topic)
  dat[p_call] <- dat$p_call
  dat[n_call] <- dat$n_call
  dat$p_call <- NULL
  dat$n_call <- NULL
  dat
}