CleanCalls <- function(calls) {
  # call2talk referred to call2talk doesn't make sense
  calls$Call2Talk$ReferralsMade %<>% str_replace("Call2Talk;", "")
  
  # bind_rows(calls$`Mass 211`, calls$Call2Talk)
  
  # "important" calls
  # icalls <- calls$`Mass 211`
  icalls <- list(Mass211 = calls$`Mass 211`,
                 Call2Talk = calls$Call2Talk)
  # icalls <- bind_rows(icalls) %>%
  #   .[, Reduce(intersect, lapply(icalls, names))]
  icalls <- bind_rows(icalls)
  
  ret <- icalls %>%
    mutate(
      caller.name =
        str_replace(caller.name, "^(|\\?+|\\-+|\\d+)$", "Unknown") %>%
        recode(Unknown = NULL, .default = caller.name) %>%
        str_to_title(),
      external.referrals = ReferralsMade %>%
        # remove internal referrals
        str_replace("(^|;)[^;]*2-?1-?1.*?;", "\\1"), 
      city = city %>%
        str_replace(" +(Town|City) *", "") %>%
        str_replace("North Attleboro", "North Attleborough"),
      town = city,
      city.state = str_c(city, state, sep = ", "),
      call.length = call.length + 0.5
    )
  
  # consolidate values
  ret$called.before <- recode(ret$called.before,
      `Yes for Same Issue` = "Yes, for the same issue",
      `Yes for the same issue` = "Yes, for the same issue",
      `Yes for a different issue` = "Yes, for a different issue",
      `Yes for different issue` = "Yes, for a different issue")
  ret$called.before[which(is.na(ret$called.before))] <- "Unknown"
  
  ret$how.heard <- recode(ret$how.heard,
      `Postcard` = "Mailing/Advertisement",
      `Mail` = "Mailing/Advertisement",
      `Poster` = "Mailing/Advertisement",
      `Work` = "Friend/Coworker",
      `Family/Friend` = "Family Member",
      `Family Memeber` = "Family Member",
      `CCR&R` = "Agency (Human Service)",
      `EEC` = "Agency (State Government)",
      `Newspaper/Article` = "Radio/Television/Newspaper",
      `Radio` = "Radio/Television/Newspaper",
      `Newspaper` = "Radio/Television/Newspaper",
      `Television` = "Radio/Television/Newspaper",
      `Online Search/Web Search` = "Online/Social Media",
      `Social Media` = "Online/Social Media",
      `Web Search` = "Online/Social Media",
      `UWTC website/internet` = "Online/Social Media",
      `Court (Adult)` = "Police/Attorney/Court",
      `Court (Juvenile)` = "Police/Attorney/Court",
      `Police` = "Police/Attorney/Court",
      `Attorney` = "Police/Attorney/Court",
      `Psychiatric Emergency Service` = "Therapist",
      `NSPL` = "(Internal)",
      `National Suicide Prevention Line` = "(Internal)",
      `Call2Talk` = "(Internal)",
      `Mass211` = "(Internal)",
      `Unknown` = "Source Unknown",
      `Other` = "Other Sources",
      .default = ret$how.heard)
  ret$how.heard[which(is.na(ret$how.heard))] <- "Source Unknown"
  
  ret
}

icalls <- CleanCalls(calls)
cities <- unique(icalls$city) %>% sort()

CleanStrings <- function(x, to.remove = cities) {
  pattern <- str_c(to.remove, collapse = "|")
  pattern <- sprintf("[\\- ]*(%s)[ ,/]*", pattern)
  str_replace_all(x, pattern, "")
  # str_replace(x, " Program$", "")
}

CleanReferralsMade <- function() {
  # Only pick out Mass211 calls, call2talk and Mass211 text,
  # because other categories are small and only these categories
  # can identify callers by phone number.
  referrals.made <- icalls$ReferralsMade %>%
    str_replace("; *$", "") %>%
    str_split(" *; *")
  names(referrals.made) <- icalls$id
  
  # melt the list of referrals to a data frame
  referrals_made <<- lapply(names(referrals.made), function(id) {
    tibble(call_id = id, referred_to = referrals.made[[id]])
  }) %>% bind_rows() %>% na.omit()
  
  referrals_made %>%
    mutate(agency = CleanAgencies(referred_to))
}

CleanAgency <- function(x) {
  CleanStrings(x, to.remove = cities) %>%
    fct_recode(
      "RAFT Program" = "Raft Program"
    ) %>%
    str_replace("RAFT.*", "RAFT Program") %>%
    str_replace(".*Catholic Char.*", "Catholic Charities") %>%
    str_replace(".*(Helpline|Hotline).*", "Helpline") %>%
    str_replace(".*SNAP.*", "SNAP") %>%
    str_replace(".*Elder .*", "Elder Support") %>%
    str_replace(".*Emergency (Fund|Aid|Finacial).*",
                "Emergency Financial Support") %>%
    str_replace(".*Early Education (&|and) Care.*",
                "Early Education & Care") %>%
    str_replace(".*(2-1-1|211.org|211).*", "211 System") %>%
    str_replace(".*(3-1-1|311).*", "311 System") %>%
    str_replace(".*Salvation.*", "Salvation Army") %>%
    str_replace(".* Corps.*", "Salvation Army") %>%
    str_replace(".*CFCE.*", "CFCE") %>%
    str_replace(".*Parent.*", "Parenting Help") %>%
    str_replace(".*CCR&R.*", "CCR&R") %>%
    str_replace(".*Project Bread.*", "Food Pantry") %>%
    str_replace(".*Food (.*)(Pantry|Center).*", "Food Pantry") %>%
    str_replace(".*Behavioral.*", "Behavioral Health Agency") %>%
    str_replace(".*(Lawyer|Legal|Attorney).*", "Legal Service") %>%
    str_replace(".*Career.*", "Employment Assistance") %>%
    str_replace(".*mployment.*", "Employment Assistance") %>%
    # str_replace(".* Assistance.*", "Assistance Service") %>%
    str_replace(".*(Fuel|Heating) Assistance.*", "Utility/Fuel Assistance") %>%
    str_replace(".*Housing.*", "Housing Assistance") %>%
    str_replace(".*Rent.*Assistance.*", "Housing Assistance") %>%
    str_replace(".*(HomeStart|HomeCorps|Homes ).*", "Housing Assistance") %>%
    str_replace(".*(Shelter|House|Hospitality|Family Promise|Family Center)($| .*)",
                "Shelter House") %>%
    str_replace(".*(Clinic|Medical|Hospital\\b).*",
                "Hospital/Clinic/Medical Center") %>%
    str_replace(".*Health.*", "Health Care Assistance") %>%
    str_replace(".*Red Cross.*", "Red Cross") %>%
    str_replace(".* (Community|Family Resource) (Connection|Action|Center|Program).*", "Community Action")
}
# CleanAgencies(unique(met_unmet$AgencyNamePublic)) %>%
#   table() %>% as.data.frame() %>%
#   arrange(desc(Freq))

# Sanity check whether our merging of duplicate data fields works
# testd <- icalls %>%
#   select(id, called.before, how.heard) %>%
#   left_join(m %>% select(id, called.before, channel), by = "id") %>%
#   distinct()
# View(testd %>% filter(called.before.x != called.before.y))

CleanCatName <- function(x) {
  x %>%
    str_replace(".*Child Care.*", "Child Care") %>%
    str_replace(".*(Consumer|Service Complaints).*", "Consumer Complaints") %>%
    str_replace(".*(Electric Service|Fuel).*", "Utility")
}

if (!exists("met_unmet.clean")) {
  met_unmet.clean <- met_unmet %>%
    mutate(agency.simple = CleanAgency(AgencyNamePublic))
}
  

calloutcome <- met_unmet.clean %>% 
  select(id = CallReportNum,
         cat.code = TaxonomyCode,
         cat.name = TaxonomyName,
         cat1.name = Level1Name,
         cat2.name = Level2Name,
         cat3.name = Level3Name,
         cat4.name = Level4Name,
         cat5.name = Level5Name,
         unmet = NeedWasUnmet,
         reason = ReasonIfUnmetOrPartial,
         airs.cat = AIRSNeedCategory,
         agency = AgencyNamePublic,
         agency.parent = ParentAgencyName,
         agency.simple) %>%
  mutate(
    cat = CleanCatName(cat5.name)
  ) %>%
  distinct()

# check whether NA values in AIRS Cat are extraneous info that could be safely removed
bad.ids <-
  calloutcome %>% filter(is.na(airs.cat)) %>% distinct(id) %>% unlist()
ok.ids <-
  calloutcome %>% filter(!is.na(airs.cat)) %>% distinct(id) %>% unlist()
bad.rows <-
  calloutcome %>% filter(id %in% bad.ids[which(!(bad.ids %in% ok.ids))])

# check the top missing rows
count(bad.rows, cat3.name, sort = TRUE)
count(bad.rows, cat4.name, sort = TRUE)

calloutcome[calloutcome$id %in% bad.rows$id,]$airs.cat <-
  recode(
    bad.rows$cat3.name,
    # sheltering? food? we don't know.
    # `Homeless People` = "Individual, Family and Community Support",
    `Health Conditions` = "Health Care",
    `Developmental Disabilities` = "Health Care",
    `Health Conditions` = "Health Care",
    `Older Adults` = "Information Services",
    `Housing Issues` = "Housing",
    .default = recode(
      bad.rows$cat4.name,
      `Low Income` = "Income Support/Assistance",
      .default = bad.rows$airs.cat
    )
  )

# AIRS cats with small categories combined into one
calloutcome$airs.cat2 <- calloutcome$airs.cat
calloutcome$airs.cat2 %<>% recode(.,
  `Other Government/Economic Services` = "Other Services",
  `Arts, Culture and Recreation` = "Other Services",
  `Volunteers/Donations` = "Other Services",
  `Transportation` = "Other Services",
  .default = .
)
calloutcome$airs.cat %<>% str_replace_all("(, | and )", "/") %>%
  recode(
    .,
    `Other Government/Economic Services` =
      "Other Govt./Econ. Services",
    `Legal/Consumer/Public Safety Services` =
      "Legal/Consumer/PubSafe",
    `Individual/Family/Community Support` =
      "Indivd./Family/Cmty.",
    `Income Support/Assistance` =
      "Income Support",
    .default = .
  )

mcalls.complex <- icalls %>% left_join(calloutcome, by = "id")

count.cat.code <- count(mcalls.complex, cat.code)

icalls.complex.m <- mcalls.complex %>%
  # data before May, 2016 are not complete
  filter(call.start > as.Date("2016-05-01")) %>%
  # keep popular cat at the top
  # so to make sure each call is associated with
  # one prominent cat
  left_join(count.cat.code, by = "cat.code") %>%
  arrange(id, desc(n)) %>%
  select(id, airs.cat, cat.code, cat.name) %>%
  distinct(id, .keep_all = TRUE) %>%
  full_join(icalls, by = "id") %>%
  # remove NA's
  filter(!is.na(cat.code))
# View(mcalls %>% count(agency.simple, sort = T))

mcalls.complex %>% summarise(mean(is.na(phone), na.rm=T))

mcalls <- mcalls.complex
icalls.m <- icalls.complex.m