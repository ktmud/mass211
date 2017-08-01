
CallerID <- function(icalls) {
  # Generate unique IDs for callers, based on
  #  1. if phone number is available, use phone number
  #  2. if name is available, use zipcode + name
  #  3. fallback to zip code + age + gender
  
  # fix NA values in columns we depend on
  icalls$family.size <- ifelse(is.na(icalls$family.size), "",
                               icalls$family.size)
  icalls$age.of.child <- ifelse(is.na(icalls$age.of.child), "",
                                icalls$age.of.child)
  icalls$lang <- ifelse(is.na(icalls$lang), "", icalls$lang)
  icalls$dd.source.of.income <- ifelse(is.na(icalls$dd.source.of.income), "",
                                    icalls$dd.source.of.income)
  
  ifelse(
    !is.na(icalls$phone),
    icalls$phone,
    ifelse(
      !is.na(icalls$caller.name),
      str_c(icalls$zip, icalls$caller.name),
      str_c(icalls$zip, icalls$age, icalls$lang, icalls$gender,
            icalls$dd.source.of.income, icalls$family.size, icalls$age.of.child)
    )
  ) %>% openssl::sha256()
}

icalls$caller.id <- CallerID(icalls)

